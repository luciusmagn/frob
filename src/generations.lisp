(in-package #:autolith)

;;;; -- Retained Generations --

(defclass generation ()
  ((identifier
    :initarg :identifier
    :reader generation-identifier
    :type non-empty-string
    :documentation "The stable generation identifier.")
   (directory
    :initarg :directory
    :reader generation-directory
    :type pathname
    :documentation "The directory containing generation artifacts.")
   (core-pathname
    :initarg :core-pathname
    :reader generation-core-pathname
    :type pathname
    :documentation "The atomically published SBCL core pathname.")
   (temporary-core-pathname
    :initarg :temporary-core-pathname
    :reader generation-temporary-core-pathname
    :type pathname
    :documentation "The unpublished core pathname used by the saver child.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader generation-manifest-pathname
    :type pathname
    :documentation "The portable generation manifest pathname.")
   (reconstruction-pathname
    :initarg :reconstruction-pathname
    :initform nil
    :reader generation-reconstruction-pathname
    :type (option pathname)
    :documentation "The complete base-image Lisp reconstruction script.")
   (image-commit-identifier
    :initarg :image-commit-identifier
    :initform nil
    :reader generation-image-commit-identifier
    :type (option string)
    :documentation "The private image commit captured by this generation.")
   (mutation-history-commit
    :initarg :mutation-history-commit
    :initform nil
    :reader generation-mutation-history-commit
    :type (option string)
    :documentation "The private Git commit retaining the captured image state.")
   (git-commit
    :initarg :git-commit
    :reader generation-git-commit
    :type non-empty-string
    :documentation "The clean source revision represented by the generation.")
   (journal-position
    :initarg :journal-position
    :reader generation-journal-position
    :type integer
    :documentation "The mutation journal byte position captured before the fork.")
   (created-at
    :initarg :created-at
    :reader generation-created-at
    :type timestamp
    :documentation "The generation creation time as Common Lisp universal time.")
   (status
    :initarg :status
    :accessor generation-status
    :type (member :pending :ready :failed)
    :documentation "The parent process's current publication status.")
   (coordinator-pid
    :initform nil
    :accessor generation-coordinator-pid
    :type (option integer)
    :documentation "The coordinator process identifier while publication is pending."))
  (:documentation "A saved live image paired with its source and runtime identity."))

(defclass generation-core-probe-runner ()
  ()
  (:documentation
   "The execution boundary used to query an unpublished generation core."))

(defclass sbcl-generation-core-probe-runner (generation-core-probe-runner)
  ((command
    :initarg :command
    :reader generation-core-probe-runner-command
    :type non-empty-string
    :documentation "The SBCL executable used to boot an unpublished core."))
  (:documentation "A core probe runner implemented by a separate SBCL process."))

(define-constant +checkpoint-core-probe-argument+
  "--autolith-internal-checkpoint-probe"
  :test #'string=
  :documentation "The exact private argument that requests a retained-core identity probe.")

(defvar *checkpoint-in-progress-p* nil
  "True while this active process has one unpublished checkpoint.")

(defvar *checkpoint-core-probe-record* nil
  "The portable generation identity embedded in a checkpoint saver core.")

(-> generation-core-probe-run
    (generation-core-probe-runner generation)
    string)
(defgeneric generation-core-probe-run (runner generation)
  (:documentation
   "Boot GENERATION's temporary core through RUNNER and return its probe output."))

(defmethod generation-core-probe-run
    ((runner sbcl-generation-core-probe-runner) (generation generation))
  "Boot GENERATION's unpublished core and capture its exact internal probe output."
  (handler-case
      (uiop:run-program
       (list (generation-core-probe-runner-command runner)
             "--noinform"
             "--core"
             (namestring (generation-temporary-core-pathname generation))
             "--end-runtime-options"
             +checkpoint-core-probe-argument+)
       :input nil
       :output :string
       :error-output :output)
    (error ()
      (error 'checkpoint-error
             :message "The saved checkpoint core could not run its internal probe."
             :stage ':probe
             :pathname (generation-temporary-core-pathname generation)))))

(-> generation-core-probe-runner-create () generation-core-probe-runner)
(defun generation-core-probe-runner-create ()
  "Create the production subprocess runner for unpublished generation cores."
  (let ((configured-command (uiop:getenv "AUTOLITH_SBCL")))
    (make-instance 'sbcl-generation-core-probe-runner
                   :command (if (non-empty-string-p configured-command)
                                configured-command
                                "sbcl"))))

(-> generation-root (configuration) pathname)
(defun generation-root (configuration)
  "Return CONFIGURATION's retained-generation directory."
  (merge-pathnames "generations/" (configuration-data-root configuration)))

(-> generation-current-pathname (configuration) pathname)
(defun generation-current-pathname (configuration)
  "Return CONFIGURATION's selected-generation record pathname."
  (merge-pathnames "current-generation.sexp"
                   (configuration-state-root configuration)))

(-> generation-create-record
    (configuration &key (:git-commit (option string))
                        (:mutation-checker (option mutation-checker)))
    generation)
(defun generation-create-record (configuration &key git-commit mutation-checker)
  "Create a pending generation record with immutable artifact paths."
  (let* ((identifier (make-identifier))
         (directory (merge-pathnames
                     (format nil "~A/" identifier)
                     (generation-root configuration)))
         (reconstruction-pathname
           (merge-pathnames "reconstruct.lisp" directory))
         (image-commit
           (image-commit-prepare-checkpoint
            configuration
            identifier
            :checker (or mutation-checker
                         (make-instance 'standard-mutation-checker)))))
    (image-commit-write-generation-script
     configuration reconstruction-pathname identifier image-commit)
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname (merge-pathnames "autolith.core" directory)
                   :temporary-core-pathname (merge-pathnames ".autolith.core.tmp" directory)
                   :manifest-pathname (merge-pathnames "manifest.sexp" directory)
                   :reconstruction-pathname reconstruction-pathname
                   :image-commit-identifier
                   (and image-commit (image-commit-identifier image-commit))
                   :mutation-history-commit
                   (and image-commit
                        (image-commit-history-commit image-commit))
                   :git-commit (or git-commit
                                   (string-trim
                                    '(#\Space #\Tab #\Newline #\Return)
                                    (self-git-command
                                     configuration
                                     '("rev-parse" "HEAD"))))
                   :journal-position
                   (let ((journal (configuration-journal-path configuration)))
                     (if (probe-file journal)
                         (with-open-file (stream journal
                                                 :direction :input
                                                 :element-type '(unsigned-byte 8))
                           (file-length stream))
                         0))
                   :created-at (get-universal-time)
                   :status ':pending)))

(-> generation-manifest-form (generation) list)
(defun generation-manifest-form (generation)
  "Return the portable ready manifest for GENERATION."
  (list :generation
        :version 3
        :id (generation-identifier generation)
        :core (namestring (generation-core-pathname generation))
        :reconstruction
        (namestring (generation-reconstruction-pathname generation))
        :image-commit (generation-image-commit-identifier generation)
        :mutation-history-commit
        (generation-mutation-history-commit generation)
        :git-commit (generation-git-commit generation)
        :journal-position (generation-journal-position generation)
        :sbcl-version (lisp-implementation-version)
        :operating-system (software-type)
        :operating-system-version (software-version)
        :architecture (machine-type)
        :created-at (generation-created-at generation)))

(-> generation-core-probe-record (generation) list)
(defun generation-core-probe-record (generation)
  "Return the exact portable identity expected from GENERATION's saved core."
  (list :autolith-checkpoint-core
        :version 3
        :generation-id (generation-identifier generation)
        :git-commit (generation-git-commit generation)
        :image-commit (generation-image-commit-identifier generation)
        :mutation-history-commit
        (generation-mutation-history-commit generation)))

(-> generation-core-probe-output (list) string)
(defun generation-core-probe-output (record)
  "Return canonical one-line output for a retained-core probe RECORD."
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

(-> generation--write-form-atomically (pathname list) pathname)
(defun generation--write-form-atomically (pathname form)
  "Atomically write portable FORM to PATHNAME."
  (let ((temporary (merge-pathnames
                    (format nil ".~A.~D.tmp"
                            (pathname-name pathname)
                            (sb-posix:getpid))
                    (uiop:pathname-directory-pathname pathname))))
    (ensure-directories-exist pathname)
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-circle* t)
            (*print-readably* t))
        (prin1 form stream)
        (terpri stream)
        (finish-output stream)))
    (uiop:rename-file-overwriting-target temporary pathname)
    pathname))

(-> generation--validate-core-probe
    (generation generation-core-probe-runner)
    null)
(defun generation--validate-core-probe (generation runner)
  "Require RUNNER to return GENERATION's exact embedded core identity."
  (let ((actual
          (handler-case
              (generation-core-probe-run runner generation)
            (checkpoint-error (condition)
              (error condition))
            (error ()
              (error 'checkpoint-error
                     :message "The checkpoint core probe failed unexpectedly."
                     :stage ':probe
                     :pathname
                     (generation-temporary-core-pathname generation))))))
    (unless (and (stringp actual)
                 (string= actual
                          (generation-core-probe-output
                           (generation-core-probe-record generation))))
      (error 'checkpoint-error
             :message "The checkpoint core returned the wrong generation identity."
             :stage ':probe
             :pathname (generation-temporary-core-pathname generation))))
  nil)

(-> generation-publish
    (configuration generation
     &key (:probe-runner generation-core-probe-runner))
    generation)
(defun generation-publish
    (configuration generation
     &key (probe-runner (generation-core-probe-runner-create)))
  "Validate and publish GENERATION's temporary core, manifest, and selection."
  (unless (probe-file (generation-temporary-core-pathname generation))
    (error 'checkpoint-error
           :message "The checkpoint saver produced no core file."
           :stage ':publish
           :pathname (generation-temporary-core-pathname generation)))
  (unless (and (generation-reconstruction-pathname generation)
               (uiop:subpathp
                (generation-reconstruction-pathname generation)
                (generation-directory generation))
               (probe-file (generation-reconstruction-pathname generation)))
    (error 'checkpoint-error
           :message "The checkpoint has no valid reconstruction script."
           :stage ':publish
           :pathname (generation-reconstruction-pathname generation)))
  (generation--validate-core-probe generation probe-runner)
  (uiop:rename-file-overwriting-target
   (generation-temporary-core-pathname generation)
   (generation-core-pathname generation))
  (generation--write-form-atomically
   (generation-manifest-pathname generation)
   (generation-manifest-form generation))
  (generation--write-form-atomically
   (generation-current-pathname configuration)
   (list :current-generation
         :version 1
         :id (generation-identifier generation)
         :manifest (namestring (generation-manifest-pathname generation))))
  (setf (generation-status generation) ':ready)
  generation)

(-> generation-record-failure (generation keyword t) pathname)
(defun generation-record-failure (generation stage detail)
  "Record a bounded non-secret checkpoint failure at STAGE for GENERATION."
  (let ((pathname (merge-pathnames "failure.sexp"
                                   (generation-directory generation))))
    (generation--write-form-atomically
     pathname
     (list :checkpoint-failure
           :version 1
           :id (generation-identifier generation)
           :time (get-universal-time)
           :stage stage
           :detail (bounded-string detail :limit 4000)))
    pathname))


;;;; -- Manifest Loading --

(-> generation-load-manifest ((or pathname string)) generation)
(defun generation-load-manifest (pathname)
  "Load and validate one ready generation manifest from PATHNAME."
  (setf pathname (pathname pathname))
  (let ((form (read-portable-form pathname)))
    (unless (and (listp form)
                 (eq (first form) :generation)
                 (member (getf (rest form) :version) '(1 2 3))
                 (non-empty-string-p (getf (rest form) :id))
                 (non-empty-string-p (getf (rest form) :core))
                 (non-empty-string-p (getf (rest form) :git-commit)))
      (error 'checkpoint-error
             :message (format nil "Invalid generation manifest at ~A." pathname)
             :stage ':manifest
             :pathname pathname))
    (let* ((properties (rest form))
           (version (getf properties :version))
           (directory (uiop:pathname-directory-pathname pathname))
           (core-pathname (pathname (getf properties :core)))
           (reconstruction-value (getf properties :reconstruction))
           (reconstruction-pathname
             (and (non-empty-string-p reconstruction-value)
                  (pathname reconstruction-value)))
           (image-commit-identifier (getf properties :image-commit))
           (mutation-history-commit
             (getf properties :mutation-history-commit)))
      (unless (uiop:subpathp core-pathname directory)
        (error 'checkpoint-error
               :message "A generation core is outside its artifact directory."
               :stage ':manifest
               :pathname pathname))
      (when (member version '(2 3))
        (unless (and reconstruction-pathname
                     (uiop:subpathp reconstruction-pathname directory)
                     (probe-file reconstruction-pathname)
                     (or (null image-commit-identifier)
                         (image-commit--identifier-p
                          image-commit-identifier)))
          (error 'checkpoint-error
                 :message "A generation reconstruction manifest is invalid."
                 :stage ':manifest
                 :pathname pathname)))
      (when (= version 3)
        (unless (if image-commit-identifier
                    (image-history--commit-p mutation-history-commit)
                    (null mutation-history-commit))
          (error 'checkpoint-error
                 :message "A generation mutation-history identity is invalid."
                 :stage ':manifest
                 :pathname pathname)))
      (make-instance 'generation
                     :identifier (getf properties :id)
                     :directory directory
                     :core-pathname core-pathname
                     :temporary-core-pathname
                     (merge-pathnames ".autolith.core.tmp" directory)
                     :manifest-pathname pathname
                     :reconstruction-pathname reconstruction-pathname
                     :image-commit-identifier
                     image-commit-identifier
                     :mutation-history-commit mutation-history-commit
                     :git-commit (getf properties :git-commit)
                     :journal-position
                     (or (getf properties :journal-position) 0)
                     :created-at (or (getf properties :created-at) 0)
                     :status ':ready))))

(-> generation-compatible-p (generation) boolean)
(defun generation-compatible-p (generation)
  "Return true when GENERATION has a plausible core for this exact runtime."
  (handler-case
      (let ((manifest (read-portable-form
                       (generation-manifest-pathname generation))))
        (and (probe-file (generation-core-pathname generation))
             (with-open-file (stream (generation-core-pathname generation)
                                     :direction :input
                                     :element-type '(unsigned-byte 8))
               (> (file-length stream) 1048576))
             (string= (or (getf (rest manifest) :sbcl-version) "")
                      (lisp-implementation-version))
             (string= (or (getf (rest manifest) :operating-system) "")
                      (software-type))
             (string= (or (getf (rest manifest) :operating-system-version) "")
                      (software-version))
             (string= (or (getf (rest manifest) :architecture) "")
                      (machine-type))
             t))
    (error ()
      nil)))

(-> generation-list (configuration) list)
(defun generation-list (configuration)
  "Return valid retained generations newest first."
  (let ((root (generation-root configuration)))
    (if (probe-file root)
        (sort
         (loop for directory in (uiop:subdirectories root)
               for manifest = (merge-pathnames "manifest.sexp" directory)
               for generation = (and (probe-file manifest)
                                     (handler-case
                                         (generation-load-manifest manifest)
                                       (error ()
                                         nil)))
               when generation
                 collect generation)
         #'>
         :key #'generation-created-at)
        nil)))

(-> generation-find (configuration string) (option generation))
(defun generation-find (configuration identifier)
  "Return retained generation IDENTIFIER, or NIL when it is unknown."
  (find identifier (generation-list configuration)
        :key #'generation-identifier
        :test #'string=))

(-> generation-select (configuration generation) generation)
(defun generation-select (configuration generation)
  "Select compatible GENERATION for the next recovery startup."
  (when *checkpoint-in-progress-p*
    (error 'checkpoint-error
           :message "A generation cannot be selected while a checkpoint publishes."
           :stage ':selection
           :pathname (generation-current-pathname configuration)))
  (unless (generation-compatible-p generation)
    (error 'checkpoint-error
           :message (format nil "Generation ~A is not compatible with this runtime."
                            (generation-identifier generation))
           :stage ':selection
           :pathname (generation-manifest-pathname generation)))
  (generation--write-form-atomically
   (generation-current-pathname configuration)
   (list :current-generation
         :version 1
         :id (generation-identifier generation)
         :manifest (namestring (generation-manifest-pathname generation))))
  generation)

(-> generation-request-rollback (configuration string) null)
(defun generation-request-rollback (configuration identifier)
  "Select retained generation IDENTIFIER and request an immediate rollback."
  (let ((generation (generation-find configuration identifier)))
    (unless generation
      (error 'checkpoint-error
             :message (format nil "Unknown retained generation ~A." identifier)
             :stage ':selection
             :pathname nil))
    (generation-select configuration generation)
    (error 'rollback-requested
           :message (format nil "Rollback requested for retained generation ~A."
                            (generation-identifier generation))
           :generation-id (generation-identifier generation))))

(-> generation-selected (configuration) (option generation))
(defun generation-selected (configuration)
  "Return CONFIGURATION's selected retained generation, if it remains valid."
  (let ((pathname (generation-current-pathname configuration)))
    (when (probe-file pathname)
      (handler-case
          (let* ((record (read-portable-form pathname))
                 (identifier (and (listp record)
                                  (getf (rest record) :id)))
                 (manifest (and (listp record)
                                (getf (rest record) :manifest))))
            (when (and (listp record)
                       (eq (first record) :current-generation)
                       (non-empty-string-p identifier)
                       (non-empty-string-p manifest)
                       (uiop:subpathp (pathname manifest)
                                      (generation-root configuration))
                       (probe-file manifest))
              (let ((generation (generation-load-manifest manifest)))
                (and (string= identifier (generation-identifier generation))
                     generation))))
        (error ()
          nil)))))

(-> generation-render-list (configuration) string)
(defun generation-render-list (configuration)
  "Return a concise model-visible list of retained generations."
  (let ((generations (generation-list configuration)))
    (if generations
        (with-output-to-string (stream)
          (dolist (generation generations)
            (format stream
                    "~A  ~A  source ~A~%  image ~A~%  history ~A~%  replay ~A~%"
                    (generation-identifier generation)
                    (if (generation-compatible-p generation)
                        "compatible"
                        "incompatible")
                    (generation-git-commit generation)
                    (or (generation-image-commit-identifier generation)
                        "base")
                    (or (generation-mutation-history-commit generation)
                        "unavailable")
                    (if (generation-reconstruction-pathname generation)
                        (namestring
                         (generation-reconstruction-pathname generation))
                        "unavailable for legacy generation"))))
        "No retained generations exist.")))


;;;; -- Checkpoint Backend --

(defclass checkpoint-backend ()
  ((configuration
    :initarg :configuration
    :reader checkpoint-backend-configuration
    :type configuration
    :documentation "The runtime configuration whose image is saved.")
   (worker
    :initarg :worker
    :reader checkpoint-backend-worker
    :type t
    :documentation "The disposable worker whose inherited descriptors are detached."))
  (:documentation "The platform boundary for non-stopping live-image checkpoints."))

(defclass linux-sbcl-checkpoint-backend (checkpoint-backend)
  ()
  (:documentation "A Linux SBCL checkpoint implemented with coordinator and saver forks."))

(defvar *checkpoint-thread-quiescer* nil
  "A dynamic callback running checkpoint work without ephemeral application threads.")

(-> checkpoint-create (checkpoint-backend) generation)
(defgeneric checkpoint-create (backend)
  (:documentation "Begin a validated checkpoint and return its pending generation."))

(-> checkpoint-detach-state (t) t)
(defgeneric checkpoint-detach-state (state)
  (:documentation "Detach ephemeral resources from globally rooted checkpoint STATE."))

(defmethod checkpoint-detach-state ((state t))
  "Leave unrecognized checkpoint STATE unchanged."
  state)

(-> checkpoint-backend-create (configuration t) checkpoint-backend)
(defun checkpoint-backend-create (configuration worker)
  "Return the checkpoint backend supported by this runtime."
  (if (and (member :linux *features*)
           (member :sbcl *features*))
      (make-instance 'linux-sbcl-checkpoint-backend
                     :configuration configuration
                     :worker worker)
      (error 'checkpoint-error
             :message "Non-stopping checkpoints currently require SBCL on Linux."
             :stage ':backend
             :pathname nil)))

(-> checkpoint--source-snapshot (configuration) string)
(defun checkpoint--source-snapshot (configuration)
  "Return the clean checked commit from CONFIGURATION's source tree."
  (labels ((clean-commit ()
             "Return HEAD when the source is clean, otherwise signal a checkpoint error."
             (let ((status (self-git-command
                            configuration
                            '("status" "--porcelain"))))
               (when (non-empty-string-p status)
                 (error 'checkpoint-error
                        :message "A checkpoint requires a clean source revision."
                        :stage ':validation
                        :pathname (configuration-source-root configuration))))
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              (self-git-command configuration '("rev-parse" "HEAD")))))
    (let ((before (clean-commit)))
      (handler-case
          (uiop:run-program
           (list (namestring
                  (merge-pathnames "script/check"
                                   (configuration-source-root configuration))))
           :directory (configuration-source-root configuration)
           :output :string
           :error-output :output)
        (error (condition)
          (error 'checkpoint-error
                 :message (format nil "The repository check failed: ~A" condition)
                 :stage ':validation
                 :pathname (configuration-source-root configuration))))
      (let ((after (clean-commit)))
        (unless (string= before after)
          (error 'checkpoint-error
                 :message "The source revision changed during checkpoint validation."
                 :stage ':validation
                 :pathname (configuration-source-root configuration)))
        after))))

(-> checkpoint--revalidate-source (configuration string) null)
(defun checkpoint--revalidate-source (configuration expected-commit)
  "Require CONFIGURATION to remain clean at EXPECTED-COMMIT immediately before fork."
  (let ((status (self-git-command configuration '("status" "--porcelain")))
        (commit (string-trim
                 '(#\Space #\Tab #\Newline #\Return)
                 (self-git-command configuration '("rev-parse" "HEAD")))))
    (unless (and (not (non-empty-string-p status))
                 (string= commit expected-commit))
      (error 'checkpoint-error
             :message "The source changed after checkpoint validation."
             :stage ':validation
             :pathname (configuration-source-root configuration))))
  nil)

(-> checkpoint--single-threaded-p () boolean)
(defun checkpoint--single-threaded-p ()
  "Return true when SBCL's current thread is the only live Lisp thread."
  (notany (lambda (thread)
            (and (not (eq thread sb-thread:*current-thread*))
                 (sb-thread:thread-alive-p thread)))
          (sb-thread:list-all-threads)))

(-> checkpoint--detach-worker (t) null)
(defun checkpoint--detach-worker (worker)
  "Detach SAVER's inherited worker streams without signaling the live subprocess."
  (labels ((detach-one (repl)
             "Detach one REPL's inherited descriptors in the saver process."
             (dolist (stream (list (lisp-worker-input repl)
                                   (lisp-worker-output repl)))
               (when (and stream (open-stream-p stream))
                 (ignore-errors (close stream))))
             (setf (lisp-worker-process repl) nil
                   (lisp-worker-input repl) nil
                   (lisp-worker-output repl) nil
                   (lisp-worker-next-request-id repl) 1)))
    (typecase worker
      (lisp-worker
       (detach-one worker))
      (lisp-worker-pool
       (maphash (lambda (name repl)
                  (declare (ignore name))
                  (detach-one repl))
                (lisp-worker-pool-workers worker)))))
  nil)

(-> checkpoint-resume-main () null)
(defun checkpoint-resume-main ()
  "Run a retained core's exact identity probe or Autolith's normal entry point."
  (sb-ext:disable-debugger)
  (let ((arguments (uiop:command-line-arguments)))
    (if (equal arguments (list +checkpoint-core-probe-argument+))
        (progn
          (write-string
           (generation-core-probe-output *checkpoint-core-probe-record*)
           *standard-output*)
          (finish-output *standard-output*))
        (restart-case
            (main arguments)
          (abort ()
            :report "Exit the retained Autolith core."
            nil))))
  nil)

(-> checkpoint--save-core (generation t) null)
(defun checkpoint--save-core (generation worker)
  "Detach inherited resources and save GENERATION's temporary core in this child."
  (handler-case
      (progn
        (setf *checkpoint-in-progress-p* nil
              *credentials-in-request-scope* nil
              *checkpoint-core-probe-record*
              (generation-core-probe-record generation))
        (checkpoint--detach-worker worker)
        (when (boundp '*active-application*)
          (checkpoint-detach-state
           (symbol-value '*active-application*)))
        (sb-ext:save-lisp-and-die
         (namestring (generation-temporary-core-pathname generation))
         :toplevel #'checkpoint-resume-main
         :executable nil
         :purify nil
         :compression nil))
    (error (condition)
      (ignore-errors
        (generation-record-failure generation ':save condition))
      (sb-posix:_exit 1)))
  nil)

(-> checkpoint--coordinate (configuration generation t) null)
(defun checkpoint--coordinate (configuration generation worker)
  "Fork the saver, publish its completed artifacts, and exit this coordinator."
  (handler-case
      (progn
        (checkpoint--detach-worker worker)
        (let ((saver-pid (sb-posix:fork)))
          (if (zerop saver-pid)
              (checkpoint--save-core generation worker)
              (multiple-value-bind (waited-pid status)
                  (sb-posix:waitpid saver-pid 0)
                (if (and (= waited-pid saver-pid)
                         (sb-posix:wifexited status)
                         (zerop (sb-posix:wexitstatus status)))
                    (progn
                      (generation-publish configuration generation)
                      (sb-posix:_exit 0))
                    (progn
                      (unless (probe-file
                               (merge-pathnames "failure.sexp"
                                                (generation-directory generation)))
                        (generation-record-failure
                         generation
                         ':saver-exit
                         (if (sb-posix:wifexited status)
                             (format nil "Saver exited with status ~D."
                                     (sb-posix:wexitstatus status))
                             (format nil "Saver terminated with status word ~D."
                                     status))))
                      (sb-posix:_exit 1)))))))
    (error (condition)
      (ignore-errors
        (generation-record-failure generation ':coordinator condition))
      (sb-posix:_exit 1)))
  nil)

(-> checkpoint--watch-coordinator (generation integer) null)
(defun checkpoint--watch-coordinator (generation coordinator-pid)
  "Update parent-side GENERATION status after COORDINATOR-PID terminates."
  (unwind-protect
       (handler-case
           (multiple-value-bind (waited-pid status)
               (sb-posix:waitpid coordinator-pid 0)
             (setf (generation-status generation)
                   (if (and (= waited-pid coordinator-pid)
                            (sb-posix:wifexited status)
                            (zerop (sb-posix:wexitstatus status)))
                       ':ready
                       ':failed)))
         (error ()
           (setf (generation-status generation) ':failed)))
    (setf *checkpoint-in-progress-p* nil))
  nil)

(defmethod checkpoint-create ((backend linux-sbcl-checkpoint-backend))
  "Fork a coordinator while briefly excluding live mutations, then resume the parent."
  (when *checkpoint-thread-quiescer*
    (let ((quiescer *checkpoint-thread-quiescer*))
      (return-from checkpoint-create
        (funcall
         quiescer
         (lambda ()
           (let ((*checkpoint-thread-quiescer* nil))
             (checkpoint-create backend)))))))
  (when *credentials-in-request-scope*
    (error 'checkpoint-error
           :message "A checkpoint cannot run inside a credential request scope."
           :stage ':validation
           :pathname nil))
  (let* ((configuration (checkpoint-backend-configuration backend))
         (worker (checkpoint-backend-worker backend))
         (source-commit (checkpoint--source-snapshot configuration))
         (generation nil)
         (coordinator-p nil)
         (coordinator-pid nil))
    (with-live-mutation
      (when *checkpoint-in-progress-p*
        (error 'checkpoint-error
               :message "A checkpoint is already being published."
               :stage ':validation
               :pathname nil))
      (checkpoint--revalidate-source configuration source-commit)
      (unless (checkpoint--single-threaded-p)
        (error 'checkpoint-error
               :message "A checkpoint requires the current Lisp thread to be the only live thread."
               :stage ':fork
               :pathname nil))
      (finish-output *standard-output*)
      (finish-output *error-output*)
      (setf generation (generation-create-record
                        configuration
                        :git-commit source-commit))
      (ensure-directories-exist (generation-directory generation))
      (setf *checkpoint-in-progress-p* t)
      (handler-case
          (let ((pid (sb-posix:fork)))
            (if (zerop pid)
                (setf coordinator-p t)
                (setf coordinator-pid pid
                      (generation-coordinator-pid generation) pid)))
        (error (condition)
          (setf *checkpoint-in-progress-p* nil)
          (error 'checkpoint-error
                 :message (format nil "Could not fork checkpoint coordinator: ~A"
                                  condition)
                 :stage ':fork
                 :pathname (generation-directory generation)))))
    (if coordinator-p
        (checkpoint--coordinate configuration generation worker)
        (progn
          (make-thread
           (lambda ()
             (checkpoint--watch-coordinator generation coordinator-pid))
           :name (format nil "Autolith checkpoint ~A"
                         (generation-identifier generation)))
          generation))))


;;;; -- Generation Tools --

(defmethod tool-execute ((tool self-checkpoint-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Begin a validated non-stopping checkpoint of CONTEXT's active image."
  (declare (ignore tool arguments))
  (let ((generation
          (checkpoint-create
           (checkpoint-backend-create
            (tool-context-configuration context)
            (tool-context-worker context)))))
    (tool-success
     (format nil "Checkpoint ~A is being published by coordinator process ~D."
             (generation-identifier generation)
             (generation-coordinator-pid generation)))))

(defmethod tool-execute ((tool self-generations-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List retained generations visible to CONTEXT."
  (declare (ignore tool arguments))
  (tool-success
   (generation-render-list (tool-context-configuration context))))

(defmethod tool-execute ((tool self-rollback-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Select a compatible retained generation and request an immediate rollback."
  (declare (ignore tool))
  (generation-request-rollback
   (tool-context-configuration context)
   (tool-argument arguments "generation" :required t)))
