(in-package #:autolith)

;;;; -- Native Search Tests --

(-> search-tests--configuration (pathname) configuration)
(defun search-tests--configuration (workspace)
  "Return an isolated search configuration rooted beside WORKSPACE."
  (let ((base (test-configuration)))
    (make-instance 'configuration
                   :source-root (configuration-source-root base)
                   :working-directory workspace
                   :config-root (configuration-config-root base)
                   :data-root (configuration-data-root base)
                   :state-root (configuration-state-root base)
                   :cache-root (configuration-cache-root base)
                   :config-root (configuration-config-root base)
                   :codex-auth-path (configuration-codex-auth-path base)
                   :model (configuration-model base)
                   :reasoning-effort (configuration-reasoning-effort base)
                   :provider-endpoint (configuration-provider-endpoint base))))

(-> search-tests--write-file (pathname string) null)
(defun search-tests--write-file (pathname content)
  "Write CONTENT to PATHNAME for one native search fixture."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  nil)

(-> search-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun search-tests--call (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with alternating JSON ARGUMENTS."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> test-search-tools () null)
(defun test-search-tools ()
  "Exercise the clifff adapter and all four indexed workspace operations."
  (let* ((default-configuration
           (configuration-create
            :source-root (asdf:system-source-directory :autolith)
            :working-directory (asdf:system-source-directory :autolith)))
         (configured-library (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (library
           (if (non-empty-string-p configured-library)
               (pathname configured-library)
               (merge-pathnames "native/fff/libfff_c.so"
                                (configuration-data-root
                                 default-configuration))))
         (previous-library (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (workspace-root (uiop:ensure-directory-pathname
                          (merge-pathnames
                           (format nil "autolith-search-tests-~A/"
                                   (make-identifier))
                           (uiop:temporary-directory))))
         (configuration nil)
         (registry nil))
    (unwind-protect
         (progn
           (test-assert (probe-file library)
                        "bootstrap installs the private fff library")
           (sb-posix:setenv "AUTOLITH_FFF_LIBRARY" (namestring library) 1)
           (ensure-directories-exist workspace-root)
           (search-tests--write-file
            (merge-pathnames "src/model-selection.lisp" workspace-root)
            (format nil "first context line~%AUTOLITH_FFF_PRIMARY~%last context line~%"))
           (search-tests--write-file
            (merge-pathnames "docs/search-guide.org" workspace-root)
            (format nil "AUTOLITH_FFF_SECONDARY~%"))
           (setf configuration (search-tests--configuration workspace-root)
                 registry (make-default-tool-registry))
           (let* ((conversation
                    (conversation-create configuration :identifier "fff-search"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :registry registry))
                  (files-tool (tool-registry-find registry "search" "files"))
                  (glob-tool (tool-registry-find registry "search" "glob"))
                  (content-tool (tool-registry-find registry "search" "content"))
                  (multi-tool
                    (tool-registry-find registry "search" "multi-content")))
             (test-assert (and files-tool glob-tool content-tool multi-tool)
                          "all native search tools are registered")
             (test-assert
              (eq (search-tool-engine files-tool)
                  (search-tool-engine multi-tool))
              "one registry shares one isolated index across search operations")
             (let ((result (search-tests--call registry context
                                               "search" "files"
                                               "query" "model selection")))
               (test-assert (tool-result-success-p result)
                            (format nil
                                    "search.files completes through clifff: ~A"
                                    (tool-result-content result)))
               (test-assert (search "src/model-selection.lisp"
                                    (tool-result-content result))
                            "search.files returns fuzzy workspace-relative paths"))
             (let ((result (search-tests--call registry context
                                               "search" "glob"
                                               "pattern" "**/*.lisp")))
               (test-assert (tool-result-success-p result)
                            "search.glob completes through the shared index")
               (test-assert (search "src/model-selection.lisp"
                                    (tool-result-content result))
                            "search.glob filters indexed relative paths"))
             (let ((result (search-tests--call registry context
                                               "search" "content"
                                               "query" "AUTOLITH_FFF_PRIMARY"
                                               "context" 1)))
               (test-assert (tool-result-success-p result)
                            "search.content completes through the content index")
               (test-assert
                (and (search "src/model-selection.lisp:2:"
                             (tool-result-content result))
                     (search "first context line"
                             (tool-result-content result))
                     (search "last context line"
                             (tool-result-content result)))
                (format nil "search.content renders locations and bounded context: ~S"
                        (tool-result-content result))))
             (let ((result
                     (search-tests--call
                      registry context
                      "search" "multi-content"
                      "patterns" #("AUTOLITH_FFF_PRIMARY"
                                   "AUTOLITH_FFF_SECONDARY")
                      "constraints" "*.lisp")))
               (test-assert (tool-result-success-p result)
                            "search.multi-content searches alternatives in one pass")
               (test-assert (and (search "src/model-selection.lisp"
                                         (tool-result-content result))
                                 (not (search "docs/search-guide.org"
                                              (tool-result-content result))))
             "search.multi-content honors separate file constraints"))
             (let* ((worker (search-tool-engine files-tool))
                    (failed-process (worker-process worker))
                    (failed-pid (uiop:process-info-pid failed-process))
                    (frecency-marker
                      (merge-pathnames "fff/frecency/test-marker"
                                       (configuration-cache-root configuration)))
                    (history-marker
                      (merge-pathnames "fff/history/test-marker"
                                       (configuration-cache-root configuration))))
               (search-tests--write-file frecency-marker "discard me")
               (search-tests--write-file history-marker "discard me")
               (sb-posix:kill failed-pid sb-posix:sigkill)
               (loop repeat 100
                     while (uiop:process-alive-p failed-process)
                     do (sleep 0.01))
               (let ((result (search-tests--call registry context
                                                 "search" "files"
                                                 "query" "model selection")))
                 (test-assert
                  (and (tool-result-success-p result)
                       (not (probe-file frecency-marker))
                       (not (probe-file history-marker))
                       (probe-file (search-worker--log-path configuration))
                       (uiop:process-alive-p (worker-process worker))
                       (/= failed-pid
                           (uiop:process-info-pid
                            (worker-process worker))))
                  "a killed helper resets its databases and restarts without killing Autolith")))
             (tool-registry-close-search-state registry)
             (test-assert
              (null (worker-process (search-tool-engine files-tool)))
              "closing a registry stops and clears its isolated watcher")))
      (when registry
        (ignore-errors (tool-registry-close-search-state registry)))
      (if previous-library
          (sb-posix:setenv "AUTOLITH_FFF_LIBRARY" previous-library 1)
          (sb-posix:unsetenv "AUTOLITH_FFF_LIBRARY"))
      (uiop:delete-directory-tree workspace-root
                                  :validate t
                                  :if-does-not-exist :ignore)
      (when configuration
        (uiop:delete-directory-tree
         (test-configuration-root configuration)
         :validate t
         :if-does-not-exist :ignore))))
  nil)
