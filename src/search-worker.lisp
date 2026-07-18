(in-package #:autolith)

;;;; -- clifff Worker Adapter --

(-> search-worker-create () worker)
(defun search-worker-create ()
  "Create Autolith's lazy supervised clifff helper."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (script (merge-pathnames "bin/autolith-search-worker" source-root))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (unless (probe-file script)
      (error 'search-error
             :message (format nil "The private fff helper is missing at ~A."
                              script)
             :operation ':worker
             :pathname script
             :cause nil))
    (make-worker
     :command (list sbcl-command "--noinform" "--script" (namestring script)))))

(-> search-worker--cache-directory (configuration) pathname)
(defun search-worker--cache-directory (configuration)
  "Return CONFIGURATION's private fff database directory."
  (merge-pathnames "fff/" (configuration-cache-root configuration)))

(-> search-worker--log-path (configuration) pathname)
(defun search-worker--log-path (configuration)
  "Return CONFIGURATION's private diagnostic log for the fff helper."
  (merge-pathnames "worker.log" (search-worker--cache-directory configuration)))

(-> search-worker-request
    (worker configuration &key (:operation keyword) (:arguments list))
    string)
(defun search-worker-request (worker configuration &key operation arguments)
  "Execute one isolated clifff request for CONFIGURATION."
  (handler-case
      (worker-request
       worker
       :library-path (search--validated-library-path configuration)
       :base-path (configuration-working-directory configuration)
       :cache-directory (search-worker--cache-directory configuration)
       :log-pathname (search-worker--log-path configuration)
       :operation operation
       :arguments arguments)
    (clifff-error (condition)
      (error 'search-error
             :message (princ-to-string condition)
             :operation (clifff-error-operation condition)
             :pathname (or (clifff-error-pathname condition)
                           (configuration-working-directory configuration))
             :cause (or (clifff-error-cause condition) condition)))))

(-> tool-registry--search-workers (tool-registry) list)
(defun tool-registry--search-workers (registry)
  "Return the distinct clifff workers owned by REGISTRY."
  (let ((seen (make-hash-table :test #'eq))
        (workers nil))
    (dolist (tool (tool-registry-tools registry))
      (when (typep tool 'search-tool)
        (let ((worker (search-tool-engine tool)))
          (unless (gethash worker seen)
            (setf (gethash worker seen) t)
            (push worker workers)))))
    workers))

(-> tool-registry-close-search-state (tool-registry) null)
(defun tool-registry-close-search-state (registry)
  "Stop every isolated clifff worker owned by REGISTRY."
  (dolist (worker (tool-registry--search-workers registry))
    (worker-close worker))
  nil)

(-> tool-registry-detach-search-state (tool-registry) null)
(defun tool-registry-detach-search-state (registry)
  "Detach inherited clifff streams before saving a forked Lisp image."
  (dolist (worker (tool-registry--search-workers registry))
    (worker-detach worker))
  nil)


;;;; -- Tool Execution --

(defmethod tool-execute ((tool search-files-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Fuzzy-search indexed workspace paths through isolated clifff."
  (let ((query (search-tool--string-argument tool arguments "query"
                                             :required t))
        (page (search-tool--bounded-integer arguments "page"
                                            :maximum #xffffffff))
        (maximum-results
          (search-tool--bounded-integer
           arguments "max-results"
           :fallback +search-default-result-limit+
           :minimum 1
           :maximum +search-maximum-result-limit+)))
    (when (find #\Newline query)
      (error 'tool-error
             :message "search.files query must fit on one line."
             :tool-name "search.files"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':files
      :arguments (list query
                       :glob-p nil
                       :page page
                       :page-size maximum-results)))))

(defmethod tool-execute ((tool search-glob-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Glob indexed workspace paths through isolated clifff."
  (let ((pattern (search-tool--string-argument tool arguments "pattern"
                                               :required t))
        (page (search-tool--bounded-integer arguments "page"
                                            :maximum #xffffffff))
        (maximum-results
          (search-tool--bounded-integer
           arguments "max-results"
           :fallback +search-default-result-limit+
           :minimum 1
           :maximum +search-maximum-result-limit+)))
    (when (find #\Newline pattern)
      (error 'tool-error
             :message "search.glob pattern must fit on one line."
             :tool-name "search.glob"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':files
      :arguments (list pattern
                       :glob-p t
                       :page page
                       :page-size maximum-results)))))

(defmethod tool-execute ((tool search-content-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search indexed workspace contents through isolated clifff."
  (let* ((query (search-tool--string-argument tool arguments "query"
                                              :required t))
         (mode-name (search-tool--string-argument tool arguments "mode"
                                                  :fallback "plain"))
         (mode (cond
                 ((string= mode-name "plain") ':plain)
                 ((string= mode-name "regex") ':regex)
                 ((string= mode-name "fuzzy") ':fuzzy)
                 (t
                  (error 'tool-error
                         :message
                         "search.content mode must be plain, regex, or fuzzy."
                         :tool-name "search.content")))))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':content
      :arguments (append (list query :mode mode)
                         (search-tool--common-content-options arguments))))))

(defmethod tool-execute ((tool search-multi-content-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search indexed contents for several literal patterns through clifff."
  (let* ((value (tool-argument arguments "patterns" :required t))
         (patterns (and (vectorp value) (coerce value 'list)))
         (constraints
           (search-tool--string-argument tool arguments "constraints")))
    (unless (and patterns
                 (every (lambda (pattern)
                          (and (non-empty-string-p pattern)
                               (not (find #\Newline pattern))))
                        patterns))
      (error 'tool-error
             :message
             "search.multi-content requires non-empty literal patterns without newlines."
             :tool-name "search.multi-content"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':multi-content
      :arguments
      (append (list patterns :constraints constraints)
              (search-tool--common-content-options arguments))))))
