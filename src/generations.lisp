(in-package #:frob)

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

(defvar *checkpoint-in-progress-p* nil
  "True while this active process has one unpublished checkpoint.")

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
    (configuration &key (:git-commit (option string)))
    generation)
(defun generation-create-record (configuration &key git-commit)
  "Create a pending generation record with immutable artifact paths."
  (let* ((identifier (make-identifier))
         (directory (merge-pathnames
                     (format nil "~A/" identifier)
                     (generation-root configuration))))
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname (merge-pathnames "frob.core" directory)
                   :temporary-core-pathname (merge-pathnames ".frob.core.tmp" directory)
                   :manifest-pathname (merge-pathnames "manifest.sexp" directory)
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
        :version 1
        :id (generation-identifier generation)
        :core (namestring (generation-core-pathname generation))
        :git-commit (generation-git-commit generation)
        :journal-position (generation-journal-position generation)
        :sbcl-version (lisp-implementation-version)
        :operating-system (software-type)
        :operating-system-version (software-version)
        :architecture (machine-type)
        :created-at (generation-created-at generation)))

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

(-> generation-publish (configuration generation) generation)
(defun generation-publish (configuration generation)
  "Publish GENERATION's completed temporary core, manifest, and selection."
  (unless (probe-file (generation-temporary-core-pathname generation))
    (error 'checkpoint-error
           :message "The checkpoint saver produced no core file."
           :stage ':publish
           :pathname (generation-temporary-core-pathname generation)))
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
                 (= (or (getf (rest form) :version) 0) 1)
                 (non-empty-string-p (getf (rest form) :id))
                 (non-empty-string-p (getf (rest form) :core))
                 (non-empty-string-p (getf (rest form) :git-commit)))
      (error 'checkpoint-error
             :message (format nil "Invalid generation manifest at ~A." pathname)
             :stage ':manifest
             :pathname pathname))
    (let* ((directory (uiop:pathname-directory-pathname pathname))
           (core-pathname (pathname (getf (rest form) :core))))
      (unless (uiop:subpathp core-pathname directory)
        (error 'checkpoint-error
               :message "A generation core is outside its artifact directory."
               :stage ':manifest
               :pathname pathname))
      (make-instance 'generation
                     :identifier (getf (rest form) :id)
                     :directory directory
                     :core-pathname core-pathname
                     :temporary-core-pathname
                     (merge-pathnames ".frob.core.tmp" directory)
                     :manifest-pathname pathname
                     :git-commit (getf (rest form) :git-commit)
                     :journal-position
                     (or (getf (rest form) :journal-position) 0)
                     :created-at (or (getf (rest form) :created-at) 0)
                     :status ':ready))))

(-> generation-compatible-p (generation) boolean)
(defun generation-compatible-p (generation)
  "Return true when this runtime can load GENERATION's core."
  (handler-case
      (let ((manifest (read-portable-form
                       (generation-manifest-pathname generation))))
        (and (probe-file (generation-core-pathname generation))
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
            (format stream "~A  ~A  commit ~A~%"
                    (generation-identifier generation)
                    (if (generation-compatible-p generation)
                        "compatible"
                        "incompatible")
                    (generation-git-commit generation))))
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
                  (merge-pathnames "check"
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
  (when (typep worker 'lisp-worker)
    (dolist (stream (list (lisp-worker-input worker)
                          (lisp-worker-output worker)))
      (when (and stream (open-stream-p stream))
        (ignore-errors (close stream))))
    (setf (lisp-worker-process worker) nil
          (lisp-worker-input worker) nil
          (lisp-worker-output worker) nil
          (lisp-worker-next-request-id worker) 1))
  nil)

(-> checkpoint-resume-main () null)
(defun checkpoint-resume-main ()
  "Run Frob's normal entry point when a retained core is booted."
  (sb-ext:disable-debugger)
  (restart-case
      (main (uiop:command-line-arguments))
    (abort ()
      :report "Exit the retained Frob core."
      nil))
  nil)

(-> checkpoint--save-core (generation t) null)
(defun checkpoint--save-core (generation worker)
  "Detach inherited resources and save GENERATION's temporary core in this child."
  (handler-case
      (progn
        (setf *checkpoint-in-progress-p* nil
              *credentials-in-request-scope* nil)
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
           :name (format nil "Frob checkpoint ~A"
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
  "Select a compatible retained generation for the stable recovery launcher."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (identifier (tool-argument arguments "generation" :required t))
         (generation (generation-find configuration identifier)))
    (unless generation
      (error 'checkpoint-error
             :message (format nil "Unknown retained generation ~A." identifier)
             :stage ':selection
             :pathname nil))
    (generation-select configuration generation)
    (tool-success
     (format nil "Selected generation ~A. Start it with frob --recovery."
             identifier))))
