(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-tool-registry () null)
(defun test-tool-registry ()
  "Test namespaced tool schema construction and total dispatch failure handling."
  (let* ((registry (make-default-tool-registry))
         (schemas (tool-registry-provider-schemas registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "tool-registry"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (unknown-call (json-object
                               "namespace" "missing"
                               "name" "operation"
                               "arguments" "{}"))
                (result (tool-registry-execute-call
                         registry unknown-call context)))
           (test-assert (= (length (tool-registry-tools registry)) 26)
                        "the default registry exposes the complete initial tool set")
           (test-assert (= (length schemas) 4)
                        "the provider schemas contain four namespaces")
           (test-assert (string= (json-get (aref schemas 0) "name") "fs")
                        "the workspace filesystem namespace is first")
           (test-assert (= (length (json-get (aref schemas 0) "tools")) 4)
                        "the fs namespace exposes four workspace operations")
           (test-assert (string= (json-get (aref schemas 1) "name") "shell")
                        "the workspace shell namespace is second")
           (test-assert (string= (json-get (aref schemas 2) "name") "lisp")
                        "the disposable Lisp namespace follows the workspace tools")
           (test-assert (= (length (json-get (aref schemas 2) "tools")) 10)
                        "the Lisp namespace exposes ten worker operations")
           (test-assert (string= (json-get (aref schemas 3) "name") "self")
                        "the active-image namespace is last")
           (test-assert (= (length (json-get (aref schemas 3) "tools")) 11)
                        "the self namespace exposes eleven active-image operations")
           (test-assert (tool-registry-find registry "self" "source")
                        "tracked source inspection has a dedicated self tool")
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-workspace-tools () null)
(defun test-workspace-tools ()
  "Test workspace file reading, listing, and bounded shell commands."
  (let* ((registry (make-default-tool-registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "workspace")))
           (labels ((run (namespace name &rest arguments)
                      "Execute NAMESPACE.NAME with ARGUMENTS through the registry."
                      (tool-registry-execute-call
                       registry
                       (json-object "namespace" namespace
                                    "name" name
                                    "arguments" (json-encode
                                                 (apply #'json-object
                                                        arguments)))
                       (make-instance 'tool-context
                                      :configuration configuration
                                      :worker nil
                                      :conversation conversation))))
             (let ((sample (merge-pathnames "sample.txt" root)))
               (with-open-file (stream sample
                                       :direction :output
                                       :if-does-not-exist :create)
                 (loop for index from 1 to 10
                       do (format stream "line ~D~%" index)))
               (let ((result (run "fs" "read"
                                  "path" (namestring sample)
                                  "start-line" 3
                                  "line-count" 2)))
                 (test-assert (tool-result-success-p result)
                              "fs.read reads existing files")
                 (test-assert (search "   3  line 3"
                                      (tool-result-content result))
                              "fs.read numbers lines from the window start")
                 (test-assert (search "lines 3-4 of 10"
                                      (tool-result-content result))
                              "fs.read reports its window and total honestly")
                 (test-assert (not (search "line 5"
                                           (tool-result-content result)))
                              "fs.read honors the requested line window")))
             (test-assert (not (tool-result-success-p
                                (run "fs" "read"
                                     "path" (namestring
                                             (merge-pathnames "absent.txt"
                                                              root)))))
                          "fs.read fails cleanly for missing files")
             (ensure-directories-exist (merge-pathnames "nested/" root))
             (let ((result (run "fs" "list" "path" (namestring root))))
               (test-assert (tool-result-success-p result)
                            "fs.list lists directories")
               (test-assert (search "d           nested/"
                                    (tool-result-content result))
                            "fs.list marks subdirectories")
               (test-assert (search "sample.txt" (tool-result-content result))
                            "fs.list shows files with their sizes"))
             (let ((result (run "shell" "run"
                                "command" "echo autolith-shell-works && exit 3")))
               (test-assert (tool-result-success-p result)
                            "shell.run reports command completion")
               (test-assert (search "exit 3" (tool-result-content result))
                            "shell.run reports nonzero exit codes")
               (test-assert (search "autolith-shell-works"
                                    (tool-result-content result))
                            "shell.run captures combined output"))
             (let ((result (run "shell" "run"
                                "command" "sleep 5"
                                "timeout-seconds" 1)))
               (test-assert (not (tool-result-success-p result))
                            "shell.run stops runaway commands")
               (test-assert (search "stopped after 1"
                                    (tool-result-content result))
                            "shell.run explains its timeout"))
             (let ((target (merge-pathnames "written.txt" root)))
               (test-assert (tool-result-success-p
                             (run "fs" "write"
                                  "path" (namestring target)
                                  "content" (format nil "alpha beta~%alpha")))
                            "fs.write creates new files")
               (test-assert (search "alpha beta"
                                    (uiop:read-file-string target))
                            "fs.write stores the supplied content")
               (test-assert (not (tool-result-success-p
                                  (run "fs" "edit"
                                       "path" (namestring target)
                                       "old-text" "alpha"
                                       "new-text" "gamma")))
                            "ambiguous fs.edit matches are rejected")
               (test-assert (tool-result-success-p
                             (run "fs" "edit"
                                  "path" (namestring target)
                                  "old-text" "alpha"
                                  "new-text" "gamma"
                                  "replace-all" t))
                            "fs.edit replaces everywhere when asked")
               (test-assert (string= (uiop:read-file-string target)
                                     (format nil "gamma beta~%gamma"))
                            "fs.edit rewrites exactly the matched text")
               (test-assert (not (tool-result-success-p
                                  (run "fs" "edit"
                                       "path" (namestring target)
                                       "old-text" "missing text"
                                       "new-text" "x")))
                            "fs.edit fails cleanly when old-text is absent"))
             (test-assert (not (tool-result-success-p
                                (run "fs" "write"
                                     "path" (namestring
                                             (merge-pathnames
                                              "bin/autolith"
                                              (configuration-source-root
                                               configuration)))
                                     "content" "overwritten")))
                          "fs.write refuses the stable launcher")
             (test-assert (not (tool-result-success-p
                                (run "fs" "edit"
                                     "path" (namestring
                                             (merge-pathnames
                                              "recovery/runtime.lisp"
                                              (configuration-source-root
                                               configuration)))
                                     "old-text" "autolith"
                                     "new-text" "borf")))
                          "fs.edit refuses recovery artifacts")
             (let ((repo-root (merge-pathnames "fake-repo/" root))
                   (outside-root (merge-pathnames "elsewhere/" root)))
               (ensure-directories-exist repo-root)
               (ensure-directories-exist outside-root)
               (labels ((write-via (working-directory path)
                          "Write PATH through a context whose workspace is WORKING-DIRECTORY."
                          (tool-registry-execute-call
                           registry
                           (json-object
                            "namespace" "fs"
                            "name" "write"
                            "arguments" (json-encode
                                         (json-object "path" path
                                                      "content" "note")))
                           (make-instance
                            'tool-context
                            :configuration (make-instance
                                            'configuration
                                            :source-root repo-root
                                            :working-directory
                                            working-directory)
                            :worker nil
                            :conversation conversation))))
                 (test-assert
                  (tool-result-success-p
                   (write-via repo-root
                              (namestring (merge-pathnames "note.txt"
                                                           repo-root))))
                  "Autolith develops its own repository from inside it")
                 (test-assert
                  (not (tool-result-success-p
                        (write-via outside-root
                                   (namestring (merge-pathnames "reach.txt"
                                                                repo-root)))))
                  "other workspaces cannot reach into Autolith's repository")
                 (test-assert
                  (not (tool-result-success-p
                        (write-via repo-root
                                   (namestring (merge-pathnames "bin/autolith"
                                                                repo-root)))))
                  "launcher artifacts stay read-only even while developing")))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
