(in-package #:autolith)

;;;; -- Preloaded Active Image --

(define-constant +active-image-protocol-version+ 1
  :documentation "The installed active-image handshake version.")

(define-constant +active-image-probe-argument+
  "--autolith-internal-active-image-probe"
  :test #'string=
  :documentation "The private argument requesting active-image validation.")

(defvar *active-image-build-record* nil
  "The source and runtime identity embedded in a preloaded active image.")

(-> active-image--git-output (pathname list) string)
(defun active-image--git-output (source-root arguments)
  "Return trimmed output from Git ARGUMENTS beneath SOURCE-ROOT."
  (handler-case
      (string-trim
       '(#\Space #\Tab #\Newline #\Return)
       (uiop:run-program
        (append (list "git" "-C" (namestring source-root)) arguments)
        :output :string
        :error-output :output))
    (error (condition)
      (error 'active-image-build-error
             :message (format nil "Could not identify active-image source: ~A"
                              condition)
             :stage ':source
             :pathname source-root))))

(-> active-image-source-paths (pathname) list)
(defun active-image-source-paths (source-root)
  "Return sorted repository-relative inputs compiled into an active image."
  (let* ((source-directory (merge-pathnames "src/" source-root))
         (lisp-paths
           (mapcar (lambda (pathname)
                     (enough-namestring pathname source-root))
                   (uiop:directory-files source-directory "*.lisp"))))
    (sort (append '("bin/autolith"
                    "bin/autolith-active"
                    "bin/autolith-search-worker"
                    "bin/autolith-runtime"
                    "script/build-active"
                    "script/build-active.lisp"
                    "autolith.asd"
                    "qlfile"
                    "qlfile.lock"
                    "sbcl.version")
                  lisp-paths)
          #'string<)))

(-> active-image--source-blobs (pathname list) list)
(defun active-image--source-blobs (source-root relative-pathnames)
  "Return Git content identities for RELATIVE-PATHNAMES beneath SOURCE-ROOT."
  (let ((identities
          (uiop:split-string
           (active-image--git-output
            source-root
            (append '("hash-object" "--") relative-pathnames))
           :separator '(#\Newline #\Return))))
    (unless (= (length identities) (length relative-pathnames))
      (error 'active-image-build-error
             :message "Git returned the wrong number of active-image source identities."
             :stage ':source
             :pathname source-root))
    identities))

(-> active-image-build-record-create (pathname) list)
(defun active-image-build-record-create (source-root)
  "Return the exact source and runtime identity for a new active image."
  (setf source-root (uiop:ensure-directory-pathname source-root))
  (let* ((paths (active-image-source-paths source-root))
         (blobs (active-image--source-blobs source-root paths))
         (status (active-image--git-output
                  source-root
                  (append '("status" "--porcelain" "--") paths))))
    (list :active-image-build
          :version +active-image-protocol-version+
          :source-commit
          (active-image--git-output source-root '("rev-parse" "HEAD"))
          :source-clean-p (zerop (length status))
          :source-files
          (mapcar #'list paths blobs)
          :sbcl-version (lisp-implementation-version)
          :operating-system (software-type)
          :operating-system-version (software-version)
          :architecture (machine-type))))

(-> active-image-build-record-p (t) boolean)
(defun active-image-build-record-p (value)
  "Return true when VALUE is a complete portable active-image build record."
  (and (listp value)
       (eq (first value) :active-image-build)
       (= (or (getf (rest value) :version) 0)
          +active-image-protocol-version+)
       (non-empty-string-p (getf (rest value) :source-commit))
       (member (getf (rest value) :source-clean-p) '(t nil))
       (let ((source-files (getf (rest value) :source-files)))
         (and (consp source-files)
              (every (lambda (entry)
                       (and (listp entry)
                            (= (length entry) 2)
                            (non-empty-string-p (first entry))
                            (non-empty-string-p (second entry))))
                     source-files)
              (= (length source-files)
                 (length (remove-duplicates source-files
                                            :key #'first
                                            :test #'string=)))))
       (non-empty-string-p (getf (rest value) :sbcl-version))
       (non-empty-string-p (getf (rest value) :operating-system))
       (non-empty-string-p (getf (rest value) :operating-system-version))
       (non-empty-string-p (getf (rest value) :architecture))
       t))

(-> active-image-build-record-compatible-p (t pathname) boolean)
(defun active-image-build-record-compatible-p (record source-root)
  "Return true when RECORD exactly matches SOURCE-ROOT and this runtime."
  (handler-case
      (let* ((source-root (uiop:ensure-directory-pathname
                           (truename source-root)))
             (source-files (and (active-image-build-record-p record)
                                (getf (rest record) :source-files))))
        (and source-files
             (string= (getf (rest record) :sbcl-version)
                      (lisp-implementation-version))
             (string= (getf (rest record) :operating-system)
                      (software-type))
             (string= (getf (rest record) :operating-system-version)
                      (software-version))
             (string= (getf (rest record) :architecture)
                      (machine-type))
             (equal (mapcar #'first source-files)
                    (active-image-source-paths source-root))
             (equal (mapcar #'second source-files)
                    (active-image--source-blobs source-root
                                                (mapcar #'first source-files)))
             t))
    (error ()
      nil)))

(-> active-image-probe-record (list) list)
(defun active-image-probe-record (build-record)
  "Return the exact public handshake for BUILD-RECORD."
  (list :autolith-active-image
        :version +active-image-protocol-version+
        :source-commit (getf (rest build-record) :source-commit)))

(-> active-image-probe-output (list) string)
(defun active-image-probe-output (record)
  "Return canonical one-line output for active-image probe RECORD."
  (with-output-to-string (stream)
    (let ((*print-base* 10)
          (*print-case* ':upcase)
          (*print-circle* nil)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* nil)
          (*print-radix* nil)
          (*print-readably* t))
      (write record :stream stream)
      (terpri stream))))

(-> active-image-manifest-form (pathname list) list)
(defun active-image-manifest-form (core-pathname build-record)
  "Return the portable manifest for CORE-PATHNAME and BUILD-RECORD."
  (list :active-image
        :version +active-image-protocol-version+
        :core (namestring core-pathname)
        :built-at (get-universal-time)
        :source-commit (getf (rest build-record) :source-commit)
        :source-clean-p (getf (rest build-record) :source-clean-p)
        :source-files (getf (rest build-record) :source-files)
        :sbcl-version (getf (rest build-record) :sbcl-version)
        :operating-system (getf (rest build-record) :operating-system)
        :operating-system-version
        (getf (rest build-record) :operating-system-version)
        :architecture (getf (rest build-record) :architecture)))


;;;; -- Image Entry and Publication --

(-> active-image-main () null)
(defun active-image-main ()
  "Validate a probe or run Autolith from a preloaded active image."
  (sb-ext:disable-debugger)
  (let ((arguments (uiop:command-line-arguments)))
    (handler-case
        (let ((source-root
                (and arguments
                     (uiop:ensure-directory-pathname
                      (pathname (first arguments))))))
          (cond
            ((and (= (length arguments) 2)
                  (string= (second arguments)
                           +active-image-probe-argument+))
             (unless (active-image-build-record-compatible-p
                      *active-image-build-record*
                      source-root)
               (error 'active-image-build-error
                      :message "The preloaded active image does not match its source."
                      :stage ':probe
                      :pathname source-root))
             (write-string
              (active-image-probe-output
               (active-image-probe-record *active-image-build-record*))
              *standard-output*)
             (finish-output *standard-output*))
            ((null source-root)
             (error 'active-image-build-error
                    :message "The preloaded active image needs its source root."
                    :stage ':entry
                    :pathname nil))
            (t
             (sb-posix:setenv "AUTOLITH_SOURCE_ROOT" (namestring source-root) 1)
             (restart-case
                 (main (rest arguments))
               (abort ()
                 :report "Exit the preloaded Autolith image."
                 nil)))))
      (serious-condition (condition)
        (format *error-output* "Autolith's preloaded active image failed: ~A~%"
                condition)
        (uiop:quit 1))))
  nil)

(-> active-image--save-child (pathname list) null)
(defun active-image--save-child (pathname build-record)
  "Save a detached preloaded image for BUILD-RECORD at PATHNAME."
  (handler-case
      (progn
        (setf *active-image-build-record* build-record
              *active-application* nil
              *credentials-in-request-scope* nil
              *checkpoint-in-progress-p* nil
              *checkpoint-core-probe-record* nil)
        (sb-ext:save-lisp-and-die
         (namestring pathname)
         :toplevel #'active-image-main
         :executable nil
         :purify nil
         :compression nil))
    (error ()
      (sb-posix:_exit 1)))
  nil)

(-> active-image--probe-core (pathname pathname list) null)
(defun active-image--probe-core (core-pathname source-root build-record)
  "Boot CORE-PATHNAME and require its exact BUILD-RECORD handshake."
  (let* ((configured-command (uiop:getenv "AUTOLITH_SBCL"))
         (sbcl-command (if (non-empty-string-p configured-command)
                           configured-command
                           "sbcl"))
         (actual
           (handler-case
               (uiop:run-program
                (list sbcl-command
                      "--noinform"
                      "--core" (namestring core-pathname)
                      "--end-runtime-options"
                      (namestring source-root)
                      +active-image-probe-argument+)
                :input nil
                :output :string
                :error-output :output)
             (error (condition)
               (error 'active-image-build-error
                      :message (format nil "The active-image probe failed: ~A"
                                       condition)
                      :stage ':probe
                      :pathname core-pathname))))
         (expected
           (active-image-probe-output
            (active-image-probe-record build-record))))
    (unless (string= actual expected)
      (error 'active-image-build-error
             :message "The saved active image returned the wrong identity."
             :stage ':probe
             :pathname core-pathname)))
  nil)

(-> active-image--write-manifest (pathname list) pathname)
(defun active-image--write-manifest (pathname form)
  "Atomically replace PATHNAME with portable active-image manifest FORM."
  (let ((temporary
          (merge-pathnames
           (format nil ".manifest.~D.tmp" (sb-posix:getpid))
           (uiop:pathname-directory-pathname pathname))))
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* t))
        (prin1 form stream)
        (terpri stream)
        (finish-output stream)))
    (sb-posix:chmod (namestring temporary) #o444)
    (uiop:rename-file-overwriting-target temporary pathname)
    pathname))

(-> active-image-install (pathname pathname) pathname)
(defun active-image-install (source-root core-pathname)
  "Build, validate, and atomically install a preloaded active image."
  (setf source-root (uiop:ensure-directory-pathname (truename source-root))
        core-pathname (pathname core-pathname))
  (let* ((directory (uiop:pathname-directory-pathname core-pathname))
         (temporary
           (merge-pathnames
            (format nil ".~A.~D.core" (pathname-name core-pathname)
                    (sb-posix:getpid))
            directory))
         (manifest (merge-pathnames "manifest.sexp" directory))
         (identity-before (active-image-build-record-create source-root))
         (child-pid nil))
    (ensure-directories-exist core-pathname)
    (when (probe-file temporary)
      (delete-file temporary))
    (unless (checkpoint--single-threaded-p)
      (error 'active-image-build-error
             :message "Building an active image requires one live Lisp thread."
             :stage ':fork
             :pathname core-pathname))
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (unwind-protect
         (progn
           (setf child-pid
                 (handler-case
                     (sb-posix:fork)
                   (error (condition)
                     (error 'active-image-build-error
                            :message (format nil
                                             "Could not fork the active-image saver: ~A"
                                             condition)
                            :stage ':fork
                            :pathname temporary))))
           (if (zerop child-pid)
               (active-image--save-child temporary identity-before)
               (multiple-value-bind (waited-pid status)
                   (handler-case
                       (sb-posix:waitpid child-pid 0)
                     (error (condition)
                       (error 'active-image-build-error
                              :message (format nil
                                               "Could not wait for the active-image saver: ~A"
                                               condition)
                              :stage ':save
                              :pathname temporary)))
                 (unless (and (= waited-pid child-pid)
                              (sb-posix:wifexited status)
                              (zerop (sb-posix:wexitstatus status))
                              (probe-file temporary))
                   (error 'active-image-build-error
                          :message "The active-image saver child failed."
                          :stage ':save
                          :pathname temporary))
                 (active-image--probe-core temporary
                                           source-root
                                           identity-before)
                 (let ((identity-after
                         (active-image-build-record-create source-root)))
                   (unless (equal identity-before identity-after)
                     (error 'active-image-build-error
                            :message "Active-image inputs changed during the build."
                            :stage ':source
                            :pathname source-root)))
                 (handler-case
                     (progn
                       (uiop:rename-file-overwriting-target temporary
                                                            core-pathname)
                       (sb-posix:chmod (namestring core-pathname) #o444)
                       (active-image--write-manifest
                        manifest
                        (active-image-manifest-form core-pathname
                                                    identity-before)))
                   (error (condition)
                     (error 'active-image-build-error
                            :message (format nil
                                             "Could not publish the active image: ~A"
                                             condition)
                            :stage ':publish
                            :pathname core-pathname))))))
      (unless (and child-pid (zerop child-pid))
        (when (probe-file temporary)
          (delete-file temporary))))
    core-pathname))
