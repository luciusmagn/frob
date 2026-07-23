(in-package #:autolith)

;;;; -- Runtime Test Boundary --

(defclass tool-test-runtime-tool (tool)
  ((runtime-identity
    :initarg :runtime-identity
    :reader tool-test-runtime-identity
    :type t
    :documentation "The shared test runtime identity.")
   (close-function
    :initarg :close-function
    :reader tool-test-runtime-close-function
    :type function
    :documentation "The callback recording runtime closure.")
   (resume-function
    :initarg :resume-function
    :reader tool-test-runtime-resume-function
    :type function
    :documentation "The callback recording runtime restart.")
   (close-priority
    :initarg :close-priority
    :initform 0
    :reader tool-test-runtime-close-priority
    :type integer
    :documentation "The deterministic test runtime dependency priority.")
   (detach-function
    :initarg :detach-function
    :reader tool-test-runtime-detach-function
    :type function
    :documentation "The callback recording runtime detachment."))
  (:documentation "A tool exposing deterministic ephemeral-runtime callbacks."))

(defmethod tool-runtime-identity ((tool tool-test-runtime-tool))
  "Return TOOL's shared test runtime identity."
  (tool-test-runtime-identity tool))

(defmethod tool-runtime-close ((tool tool-test-runtime-tool))
  "Invoke TOOL's deterministic close callback."
  (funcall (tool-test-runtime-close-function tool))
  nil)

(defmethod tool-runtime-close-priority ((tool tool-test-runtime-tool))
  "Return TOOL's deterministic dependency priority."
  (tool-test-runtime-close-priority tool))

(defmethod tool-runtime-resume
    ((tool tool-test-runtime-tool) (registry tool-registry))
  "Invoke TOOL's deterministic resume callback."
  (declare (ignore registry))
  (funcall (tool-test-runtime-resume-function tool))
  nil)

(defmethod tool-runtime-detach ((tool tool-test-runtime-tool))
  "Invoke TOOL's deterministic detach callback."
  (funcall (tool-test-runtime-detach-function tool))
  nil)


;;;; -- Subsystem Tests --

(-> test-tool-registry () null)
(defun test-tool-registry ()
  "Test tool schemas, dispatch failure handling, and runtime lifecycle cleanup."
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
           (test-assert (= (length (tool-registry-tools registry)) 47)
                        "the default registry exposes the complete initial tool set")
           (test-assert (= (length schemas) 8)
                        "the provider schemas contain eight namespaces")
           (test-assert (string= (json-get (aref schemas 0) "name") "fs")
                        "the workspace filesystem namespace is first")
           (test-assert (= (length (json-get (aref schemas 0) "tools")) 5)
                        "the fs namespace exposes five workspace operations")
           (test-assert (tool-registry-find registry "fs" "view-image")
                        "native local image inspection has a filesystem tool")
           (test-assert (string= (json-get (aref schemas 1) "name") "search")
                        "indexed workspace search follows file access")
           (test-assert (= (length (json-get (aref schemas 1) "tools")) 4)
                        "the search namespace exposes four indexed operations")
           (test-assert (string= (json-get (aref schemas 2) "name") "shell")
                        "the workspace shell namespace follows indexed search")
           (test-assert (string= (json-get (aref schemas 3) "name") "memory")
                        "persistent memories have a dedicated namespace")
           (test-assert (= (length (json-get (aref schemas 3) "tools")) 5)
                        "the memory namespace exposes five operations")
           (test-assert (string= (json-get (aref schemas 4) "name") "agenda")
                        "workspace agendas have a dedicated namespace")
           (test-assert (= (length (json-get (aref schemas 4) "tools")) 5)
                        "the agenda namespace exposes five operations")
           (test-assert (string= (json-get (aref schemas 5) "name") "lisp")
                        "the named Lisp namespace follows the workspace tools")
           (test-assert (= (length (json-get (aref schemas 5) "tools")) 12)
                        "the Lisp namespace exposes twelve worker operations")
           (test-assert (tool-registry-find registry "lisp" "source")
                        "matching implementation source has a dedicated Lisp tool")
           (test-assert (string= (json-get (aref schemas 6) "name") "self")
                        "the active-image namespace is last")
           (test-assert (= (length (json-get (aref schemas 6) "tools")) 14)
                        "the self namespace exposes fourteen active-image operations")
           (test-assert (tool-registry-find registry "self" "source")
                        "tracked source inspection has a dedicated self tool")
           (test-assert (string= (json-get (aref schemas 7) "name") "skill")
                        "request-local Skill selection follows core tools")
           (let* ((immutable-registry
                    (make-default-tool-registry :immutable-p t))
                  (immutable-schemas
                    (tool-registry-provider-schemas immutable-registry))
                  (self-schema (aref immutable-schemas 6)))
             (test-assert (= (length (tool-registry-tools immutable-registry)) 38)
                          "immutable mode omits nine active-image state tools")
             (test-assert (= (length (json-get self-schema "tools")) 5)
                          "immutable mode advertises five self inspection tools")
             (dolist (name '("inspect" "source" "status" "diff" "generations"))
               (test-assert (tool-registry-find immutable-registry "self" name)
                            (format nil "immutable mode retains self.~A" name)))
             (dolist (name '("eval" "redefine" "set" "persist-definition"
                             "discard" "exercise" "commit" "checkpoint"
                             "rollback"))
               (test-assert
                (null (tool-registry-find immutable-registry "self" name))
                (format nil "immutable mode omits self.~A" name))))
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let ((registry (make-instance 'tool-registry))
        (runtime-identity (list ':shared-runtime))
        (close-count 0)
        (resume-count 0)
        (detach-count 0))
    (flet ((make-runtime-tool (name)
             "Return one test tool sharing the lexical runtime counters."
             (make-instance
              'tool-test-runtime-tool
              :namespace "test"
              :name name
              :description "Exercise the runtime lifecycle protocol."
              :parameters (tool-object-schema (json-object) nil)
              :runtime-identity runtime-identity
              :close-function (lambda () (incf close-count))
              :resume-function (lambda () (incf resume-count))
              :detach-function (lambda () (incf detach-count)))))
      (tool-registry-register registry (make-runtime-tool "first"))
      (tool-registry-register registry (make-runtime-tool "second"))
      (tool-registry-close-runtime-state registry)
      (tool-registry-resume-runtime-state registry)
      (tool-registry-detach-runtime-state registry)
      (test-assert (= close-count 1)
                   "a shared tool runtime closes exactly once per registry")
      (test-assert (= resume-count 1)
                   "a shared tool runtime resumes exactly once per registry")
      (test-assert (= detach-count 1)
                   "a shared tool runtime detaches exactly once per registry")))
  (let ((registry (make-instance 'tool-registry))
        (close-order nil)
        (resume-order nil)
        (failure nil))
    (flet ((make-runtime-tool
               (&key name identity priority close-function resume-function)
             "Return one independently identified close-test tool."
             (make-instance
              'tool-test-runtime-tool
              :namespace "failure-test"
              :name name
              :description "Exercise complete runtime cleanup after failure."
              :parameters (tool-object-schema (json-object) nil)
              :runtime-identity identity
              :close-priority priority
              :close-function close-function
              :resume-function resume-function
              :detach-function (lambda () nil))))
      (tool-registry-register
       registry
       (make-runtime-tool
        :name "failure"
        :identity (list ':failure)
        :priority 50
        :close-function
        (lambda ()
          (push ':failure close-order)
          (error "expected runtime close failure"))
        :resume-function
        (lambda () (push ':failure resume-order))))
      (tool-registry-register
       registry
       (make-runtime-tool
        :name "later"
        :identity (list ':later)
        :priority 100
        :close-function (lambda () (push ':later close-order))
        :resume-function (lambda () (push ':later resume-order))))
      (setf failure
            (handler-case
                (progn
                  (tool-registry-close-runtime-state registry)
                  nil)
              (error (condition)
                condition)))
      (test-assert failure
                   "runtime closure reports the first cleanup failure")
      (test-assert (equal (nreverse close-order) '(:later :failure))
                   "runtime closure unwinds dependencies and survives a failure")
      (tool-registry-resume-runtime-state registry)
      (test-assert (equal (nreverse resume-order) '(:failure :later))
                   "runtime restart restores dependencies before dependents")))
  nil)

(-> test-fs-read-streaming () null)
(defun test-fs-read-streaming ()
  "Test bounded fs.read windows across large and malformed files."
  (let* ((registry (make-default-tool-registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "fs-read-streaming"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation)))
           (labels ((run (path start-line line-count)
                      "Read one explicit line window from PATH."
                      (tool-registry-execute-call
                       registry
                       (json-object
                        "namespace" "fs"
                        "name" "read"
                        "arguments"
                        (json-encode
                         (json-object
                          "path" (namestring path)
                          "start-line" start-line
                          "line-count" line-count)))
                       context)))
             (let ((many-lines (merge-pathnames "many-lines.txt" root)))
               (with-open-file (stream many-lines
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (loop for line-number from 1 to 120000
                       do (format stream "row ~D~%" line-number)))
               (let* ((*fs-read-stream-buffer-characters* 257)
                      (result (run many-lines 119999 2)))
                 (test-assert
                  (tool-result-success-p result)
                  "fs.read streams a late window from a large line sequence")
                 (test-assert
                  (search "lines 119999-120000 of 120000"
                          (tool-result-content result))
                  "streamed fs.read preserves exact total and window metadata")
                 (test-assert
                  (and (search "119999  row 119999"
                               (tool-result-content result))
                       (search "120000  row 120000"
                               (tool-result-content result)))
                  "streamed fs.read numbers late lines across small buffers")))
             (let ((empty (merge-pathnames "empty.txt" root)))
               (with-open-file (stream empty
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (declare (ignore stream)))
               (let ((result (run empty 1 1)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "lines 1-0 of 0"
                               (tool-result-content result))
                       (not (search "   1  "
                                    (tool-result-content result))))
                  "streamed fs.read preserves empty-file line semantics")))
             (let ((long-line (merge-pathnames "long-line.txt" root))
                   (chunk (make-string 8192 :initial-element #\x)))
               (with-open-file (stream long-line
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (dotimes (index 256)
                   (declare (ignore index))
                   (write-string chunk stream))
                 (format stream "~%tail marker~%"))
               (let* ((*fs-read-stream-buffer-characters* 4096)
                      (*fs-read-maximum-result-characters* 512)
                      (result (run long-line 1 1))
                      (content (tool-result-content result)))
                 (test-assert
                  (tool-result-success-p result)
                  "fs.read bounds a selected multi-megabyte line")
                 (test-assert
                  (<= (length content)
                      *fs-read-maximum-result-characters*)
                  "fs.read constructs no oversized result for a long line")
                 (test-assert
                  (search "fs.read output truncated" content)
                  "fs.read explains selected-line truncation")
                 (test-assert
                  (search "requested lines 1-1 of 2" content)
                  "fs.read labels a truncated interval as the requested window"))
               (let* ((*fs-read-stream-buffer-characters* 4096)
                      (result (run long-line 2 1)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "lines 2-2 of 2"
                               (tool-result-content result))
                       (search "   2  tail marker"
                               (tool-result-content result)))
                  "fs.read skips a huge preceding line without retaining it")))
             (let ((sparse (merge-pathnames "sparse-line.txt" root)))
               (with-open-file (stream sparse
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :element-type '(unsigned-byte 8))
                 (file-position stream (* 8 1024 1024))
                 (loop for octet across
                       #(10 115 112 97 114 115 101 32 116 97 105 108 10)
                       do (write-byte octet stream)))
               (let* ((*fs-read-stream-buffer-characters* 4096)
                      (result (run sparse 2 1)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "lines 2-2 of 2"
                               (tool-result-content result))
                       (search "   2  sparse tail"
                               (tool-result-content result)))
                  "fs.read reaches a line after a sparse multi-megabyte prefix")))
             (let* ((long-name
                      (format nil "~A.txt"
                              (make-string 220 :initial-element #\p)))
                    (long-path (merge-pathnames long-name root)))
               (with-open-file (stream long-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string
                  (make-string 1024 :initial-element #\x)
                  stream))
               (let* ((*fs-read-maximum-result-characters* 256)
                      (result (run long-path 1 1))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (<= (length content)
                           *fs-read-maximum-result-characters*))
                  "fs.read bounds the complete result including a long path")
                 (test-assert
                  (and (search "requested lines 1-1 of 1" content)
                       (search "fs.read output truncated" content))
                  "fs.read retains honest metadata and its marker with a long path")))
             (let ((malformed (merge-pathnames "malformed-utf8.txt" root)))
               (with-open-file (stream malformed
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :element-type '(unsigned-byte 8))
                 (write-byte 255 stream)
                 (write-byte 254 stream))
               (let ((result (run malformed 1 1)))
                 (test-assert
                  (not (tool-result-success-p result))
                  "fs.read reports malformed UTF-8 as a tool failure")
                 (test-assert
                  (search "fs.read failed:" (tool-result-content result))
                  "fs.read contains malformed input at the tool boundary")))))
      (tool-registry-close-runtime-state registry)
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
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
                                      :conversation conversation
                                      :command-authorization-function
                                      (lambda (command directory)
                                        (declare (ignore command directory))
                                        ':full-access)))))
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
             (let* ((image-path (merge-pathnames "tool-image.png" root))
                    (image (test-conversation--write-tiny-png image-path))
                    (result (run "fs" "view-image"
                                 "path" (namestring image)))
                    (attachments (tool-result-image-attachments result)))
               (test-assert
                (and (tool-result-success-p result)
                     (= (length attachments) 1)
                     (probe-file
                      (image-attachment-pathname (first attachments))))
                "fs.view-image validates and privately preserves a local image")
               (test-assert
                (search "1x1, image/png" (tool-result-content result))
                "fs.view-image reports the prepared image metadata"))
             (let ((result (run "shell" "run"
                                "command" "echo autolith-shell-works && exit 3")))
               (test-assert (tool-result-success-p result)
                            "shell.run reports command completion")
               (test-assert (search "exit 3" (tool-result-content result))
                            "shell.run reports nonzero exit codes")
               (test-assert (search "autolith-shell-works"
                                    (tool-result-content result))
                            "shell.run captures combined output"))
             (let* ((target (merge-pathnames "denied-command.txt" root))
                    (result
                      (tool-registry-execute-call
                       registry
                       (json-object
                        "namespace" "shell"
                        "name" "run"
                        "arguments"
                        (json-encode
                         (json-object
                          "command"
                          (format nil "printf denied > ~A"
                                  (uiop:escape-shell-token
                                   (namestring target))))))
                       (make-instance 'tool-context
                                      :configuration configuration
                                      :worker nil
                                      :conversation conversation))))
               (test-assert (not (tool-result-success-p result))
                            "shell.run denies execution without authorization")
               (test-assert (not (probe-file target))
                            "a denied shell command has no side effects"))
             (let* ((inside (merge-pathnames "sandboxed-command.txt" root))
                    (outside
                      (merge-pathnames
                       (format nil "autolith-blocked-~A.txt" (make-identifier))
                       (user-homedir-pathname)))
                    (sandbox-configuration
                      (configuration--clone configuration
                                            :working-directory root)))
               (unwind-protect
                    (let ((result
                            (tool-registry-execute-call
                             registry
                             (json-object
                              "namespace" "shell"
                              "name" "run"
                              "arguments"
                              (json-encode
                               (json-object
                                "command"
                                (format nil
                                        "printf ok > ~A; printf blocked > ~A"
                                        (uiop:escape-shell-token
                                         (namestring inside))
                                        (uiop:escape-shell-token
                                         (namestring outside))))))
                             (make-instance
                              'tool-context
                              :configuration sandbox-configuration
                              :worker nil
                              :conversation conversation
                              :command-authorization-function
                              (lambda (command directory)
                                (declare (ignore command directory))
                                ':sandboxed)))))
                      (test-assert
                       (tool-result-success-p result)
                       "an authorized shell command runs inside the sandbox")
                      (test-assert (probe-file inside)
                                   "the command sandbox permits workspace writes")
                      (test-assert
                       (not (probe-file outside))
                       "the command sandbox rejects writes outside the workspace"))
                 (when (probe-file outside)
                   (delete-file outside))))
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
  (test-fs-read-streaming)
  nil)
