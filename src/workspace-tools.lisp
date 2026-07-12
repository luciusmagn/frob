(in-package #:frob)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A tool touching only workspace files and subprocesses, never the active image."))

(defclass fs-read-tool (workspace-tool)
  ()
  (:documentation "Read one workspace file with numbered lines."))

(defclass fs-list-tool (workspace-tool)
  ()
  (:documentation "List one workspace directory with entry kinds and sizes."))

(defclass fs-write-tool (workspace-tool)
  ()
  (:documentation "Create or replace one workspace file with supplied content."))

(defclass fs-edit-tool (workspace-tool)
  ()
  (:documentation "Replace exact text occurrences inside one workspace file."))

(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one bounded external command in the workspace."))


;;;; -- Workspace Constants --

(define-constant +fs-read-default-line-count+ 400
  :documentation "The file lines returned by fs.read when no window is given.")

(define-constant +shell-default-timeout-seconds+ 60
  :documentation "The seconds one shell.run command may take by default.")

(define-constant +shell-maximum-timeout-seconds+ 600
  :documentation "The largest timeout one shell.run command may request.")


;;;; -- Path Resolution --

(-> workspace-tool-path (tool-context (option string)) pathname)
(defun workspace-tool-path (context path)
  "Return PATH resolved against CONTEXT's working directory."
  (let ((working-directory (configuration-working-directory
                            (tool-context-configuration context))))
    (if (non-empty-string-p path)
        (merge-pathnames (pathname path) working-directory)
        working-directory)))

(-> workspace-tool-protected-path-p (tool-context pathname) boolean)
(defun workspace-tool-protected-path-p (context path)
  "Return true when PATH is a stable launcher or recovery artifact."
  (let* ((source-root (configuration-source-root
                       (tool-context-configuration context)))
         (launcher-root (merge-pathnames "bin/" source-root))
         (recovery-root (merge-pathnames "recovery/" source-root)))
    (and (uiop:subpathp path source-root)
         (or (uiop:subpathp path launcher-root)
             (uiop:subpathp path recovery-root)
             (string= (enough-namestring path source-root) "build-recovery"))
         t)))

(-> workspace-tool-integer-argument
    (json-object string &key (:fallback (option integer)))
    (option integer))
(defun workspace-tool-integer-argument (arguments name &key fallback)
  "Return integer argument NAME from ARGUMENTS, or FALLBACK when absent."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       fallback)
      ((integerp value)
       value)
      ((and (numberp value) (= value (round value)))
       (round value))
      (t
       (error 'tool-error
              :message (format nil "Tool argument ~S must be an integer." name)
              :tool-name name)))))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool fs-read-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a numbered window of the requested file."
  (let* ((path (workspace-tool-path
                context
                (tool-argument arguments "path" :required t)))
         (start-line (max 1 (or (workspace-tool-integer-argument
                                 arguments "start-line")
                                1)))
         (line-count (max 1 (or (workspace-tool-integer-argument
                                 arguments "line-count")
                                +fs-read-default-line-count+))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure
        (format nil "~A is a directory; use fs.list instead." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (let* ((split (uiop:split-string (uiop:read-file-string path)
                                        :separator '(#\Newline)))
              (lines (if (and (rest split)
                              (string= (first (last split)) ""))
                         (butlast split)
                         split))
              (total (length lines))
              (window-start (min (1- start-line) total))
              (window-end (min (+ window-start line-count) total)))
         (tool-success
          (format nil "~A lines ~D-~D of ~D~%~{~A~^~%~}"
                  path
                  (1+ window-start)
                  window-end
                  total
                  (loop for line in (subseq lines window-start window-end)
                        for number from (1+ window-start)
                        collect (format nil "~4D  ~A" number line)))))))))

(defmethod tool-execute ((tool fs-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the requested directory's entries with kinds and byte sizes."
  (let ((path (workspace-tool-path context (tool-argument arguments "path"))))
    (if (not (uiop:directory-exists-p path))
        (tool-failure (format nil "~A is not a directory." path))
        (let ((directories (sort (mapcar (lambda (directory)
                                           (first (last (pathname-directory
                                                         directory))))
                                         (uiop:subdirectories path))
                                 #'string<))
              (files (sort (uiop:directory-files path)
                           #'string<
                           :key #'namestring)))
          (tool-success
           (format nil "~A~%~{~A~%~}~{~A~%~}"
                   path
                   (loop for name in directories
                         collect (format nil "d           ~A/" name))
                   (loop for file in files
                         collect (format nil "f ~9D  ~A"
                                         (handler-case
                                             (with-open-file (stream file
                                                              :element-type
                                                              '(unsigned-byte 8))
                                               (file-length stream))
                                           (error ()
                                             0))
                                         (file-namestring file)))))))))

(-> workspace--count-occurrences (string string) (integer 0))
(defun workspace--count-occurrences (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(-> workspace--replace-occurrences
    (string string string &key (:all boolean))
    string)
(defun workspace--replace-occurrences (needle replacement haystack &key all)
  "Return HAYSTACK with NEEDLE replaced by REPLACEMENT, once or everywhere."
  (with-output-to-string (stream)
    (loop with start = 0
          for position = (search needle haystack :start2 start)
          while position
          do (write-string haystack stream :start start :end position)
             (write-string replacement stream)
             (setf start (+ position (length needle)))
          unless all
            do (loop-finish)
          finally (write-string haystack stream :start start))))

(defmethod tool-execute ((tool fs-write-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Create or replace the requested file with the supplied content."
  (let ((path (workspace-tool-path
               context
               (tool-argument arguments "path" :required t)))
        (content (tool-argument arguments "content" :required t)))
    (unless (stringp content)
      (error 'tool-error
             :message "fs.write requires string content."
             :tool-name "fs.write"))
    (cond
      ((workspace-tool-protected-path-p context path)
       (tool-failure
        (format nil "~A is a protected launcher or recovery artifact." path)))
      ((uiop:directory-exists-p path)
       (tool-failure (format nil "~A is a directory." path)))
      (t
       (let ((existed-p (and (probe-file path) t)))
         (ensure-directories-exist path)
         (with-open-file (stream path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
           (write-string content stream))
         (tool-success
          (format nil "~:[Created~;Replaced~] ~A with ~:D character~:P."
                  existed-p
                  path
                  (length content))))))))

(defmethod tool-execute ((tool fs-edit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Replace exact occurrences of old-text inside the requested file."
  (let ((path (workspace-tool-path
               context
               (tool-argument arguments "path" :required t)))
        (old-text (tool-argument arguments "old-text" :required t))
        (new-text (tool-argument arguments "new-text" :required t))
        (replace-all (tool-argument arguments "replace-all")))
    (unless (and (stringp old-text) (stringp new-text))
      (error 'tool-error
             :message "fs.edit requires string old-text and new-text."
             :tool-name "fs.edit"))
    (cond
      ((zerop (length old-text))
       (tool-failure "fs.edit requires non-empty old-text."))
      ((workspace-tool-protected-path-p context path)
       (tool-failure
        (format nil "~A is a protected launcher or recovery artifact." path)))
      ((or (uiop:directory-exists-p path) (not (probe-file path)))
       (tool-failure (format nil "~A is not an existing file." path)))
      (t
       (let* ((text (uiop:read-file-string path))
              (occurrences (workspace--count-occurrences old-text text)))
         (cond
           ((zerop occurrences)
            (tool-failure
             (format nil "The old-text was not found in ~A." path)))
           ((and (> occurrences 1) (not replace-all))
            (tool-failure
             (format nil "The old-text matches ~D times in ~A; include more ~
                          context or set replace-all."
                     occurrences
                     path)))
           (t
            (with-open-file (stream path
                                    :direction :output
                                    :if-exists :supersede
                                    :external-format :utf-8)
              (write-string (workspace--replace-occurrences
                             old-text
                             new-text
                             text
                             :all (and replace-all t))
                            stream))
            (tool-success
             (format nil "Replaced ~D occurrence~:P in ~A."
                     (if replace-all
                         occurrences
                         1)
                     path)))))))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run one bounded external command and return its exit code and output."
  (let* ((command (tool-argument arguments "command" :required t))
         (directory (workspace-tool-path
                     context
                     (tool-argument arguments "directory")))
         (timeout (min +shell-maximum-timeout-seconds+
                       (max 1 (or (workspace-tool-integer-argument
                                   arguments "timeout-seconds")
                                  +shell-default-timeout-seconds+)))))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (uiop:with-temporary-file (:pathname output-path :prefix "frob-shell")
      (let* ((process (uiop:launch-program
                       command
                       :output output-path
                       :error-output output-path
                       :if-output-exists :supersede
                       :directory directory))
             (deadline (+ (get-universal-time) timeout))
             (timed-out-p nil))
        (loop while (uiop:process-alive-p process)
              do (when (> (get-universal-time) deadline)
                   (setf timed-out-p t)
                   (uiop:terminate-process process :urgent t))
                 (sleep 0.05))
        (let ((code (uiop:wait-process process))
              (output (handler-case
                          (uiop:read-file-string output-path)
                        (error ()
                          ""))))
          (if timed-out-p
              (tool-failure
               (format nil "The command was stopped after ~D seconds.~%~A"
                       timeout
                       output))
              (tool-success
               (format nil "exit ~D~%~A" code output))))))))
