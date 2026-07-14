(in-package #:autolith)

;;;; -- Probe Test Boundary --

(defclass test-generation-core-probe-runner (generation-core-probe-runner)
  ((output
    :initarg :output
    :reader test-generation-core-probe-runner-output
    :type string
    :documentation "The exact probe output returned without starting an SBCL core."))
  (:documentation "A deterministic generation-core probe runner for publication tests."))

(defmethod generation-core-probe-run
    ((runner test-generation-core-probe-runner) (generation generation))
  "Return RUNNER's configured probe output for GENERATION."
  (declare (ignore generation))
  (test-generation-core-probe-runner-output runner))

(-> test-generation-replay-target () integer)
(defun test-generation-replay-target ()
  "Return the baseline value used by generation reconstruction tests."
  0)

(-> generation-tests--generation (configuration string string) generation)
(defun generation-tests--generation (configuration identifier git-commit)
  "Return a pending test generation named IDENTIFIER at GIT-COMMIT."
  (let ((directory (merge-pathnames (format nil "~A/" identifier)
                                    (generation-root configuration))))
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname (merge-pathnames "autolith.core" directory)
                   :temporary-core-pathname
                   (merge-pathnames ".autolith.core.tmp" directory)
                   :manifest-pathname
                   (merge-pathnames "manifest.sexp" directory)
                   :reconstruction-pathname
                   (merge-pathnames "reconstruct.lisp" directory)
                   :git-commit git-commit
                   :journal-position 27
                   :created-at 4000000000
                   :status ':pending)))

(-> generation-tests--write-fake-core (generation) pathname)
(defun generation-tests--write-fake-core (generation)
  "Write one deliberately non-bootable byte to GENERATION's temporary core."
  (ensure-directories-exist (generation-temporary-core-pathname generation))
  (with-open-file (stream (generation-temporary-core-pathname generation)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-byte 42 stream))
  (generation-temporary-core-pathname generation))

(-> generation-tests--write-reconstruction (generation) pathname)
(defun generation-tests--write-reconstruction (generation)
  "Write GENERATION's deterministic test reconstruction script."
  (image-commit-write-script
   (generation-reconstruction-pathname generation)
   (generation-identifier generation)
   "Test generation reconstruction"
   nil))

(-> generation-tests--make-core-plausible (generation) pathname)
(defun generation-tests--make-core-plausible (generation)
  "Expand GENERATION's published core past the static compatibility threshold."
  (with-open-file (stream (generation-core-pathname generation)
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :error
                          :element-type '(unsigned-byte 8))
    (file-position stream 1048576)
    (write-byte 42 stream))
  (generation-core-pathname generation))

(-> generation-tests--unpublished-p (configuration generation) boolean)
(defun generation-tests--unpublished-p (configuration generation)
  "Return true when failed GENERATION left no visible publication artifacts."
  (and (probe-file (generation-temporary-core-pathname generation))
       (not (probe-file (generation-core-pathname generation)))
       (not (probe-file (generation-manifest-pathname generation)))
       (not (probe-file (generation-current-pathname configuration)))
       (eq (generation-status generation) ':pending)))

(-> generation-tests--test-rollback-control-path () null)
(defun generation-tests--test-rollback-control-path ()
  "Test rollback selection and propagation through the tool registry."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (generation
           (generation-tests--generation configuration
                                         "rollback-generation"
                                         "0123456789abcdef"))
         (runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record generation)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core generation)
           (generation-tests--write-reconstruction generation)
           (generation-publish configuration generation :probe-runner runner)
           (generation-tests--make-core-plausible generation)
           (delete-file (generation-current-pathname configuration))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "rollback-control-path"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
                  (call
                    (json-object
                     "namespace" "self"
                     "name" "rollback"
                     "arguments"
                     (json-encode
                      (json-object "generation" "rollback-generation"))))
                  (condition
                    (handler-case
                        (progn
                          (tool-registry-execute-call
                           (make-default-tool-registry)
                           call
                           context)
                          nil)
                      (rollback-requested (condition)
                        condition))))
             (test-assert condition
                          "self.rollback propagates its control condition")
             (test-assert
              (string= (rollback-requested-generation-id condition)
                       "rollback-generation")
              "the rollback condition carries the selected generation ID")
             (let ((selected (generation-selected configuration)))
               (test-assert selected
                            "rollback selects the generation before signaling")
               (test-assert
                (string= (generation-identifier selected)
                         "rollback-generation")
                "the rollback selection names the requested generation"))
             (let* ((application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :provider nil
                                     :tool-registry (make-instance 'tool-registry)
                                     :worker nil
                                     :agent nil
                                     :ui nil))
                    (command-condition
                      (handler-case
                          (progn
                            (application-command
                             application
                             "/rollback rollback-generation")
                            nil)
                        (rollback-requested (condition)
                          condition))))
               (test-assert command-condition
                            "/rollback requests process recovery immediately")
               (test-assert
                (string= (rollback-requested-generation-id command-condition)
                         "rollback-generation")
                "/rollback carries the selected generation into recovery"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> generation-tests--test-reconstruction-capture () null)
(defun generation-tests--test-reconstruction-capture ()
  "Test automatic mutation commits and per-generation replay scripts."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (previous-function (symbol-function 'test-generation-replay-target))
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-lineage-identifier *active-image-lineage-identifier*)
         (check-count 0)
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              (incf check-count)
              "active generation checks passed")
            :source-callback
            (lambda (checked-configuration paths)
              (declare (ignore checked-configuration paths))
              (error "Generation capture must not run source checks here.")))))
    (unwind-protect
         (progn
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-lineage-identifier* nil)
           (test-assert (null (image-state-load configuration))
                        "generation capture initializes an empty image lineage")
           (self-install-definition
            configuration
            "(defun test-generation-replay-target () \"Return captured state.\" 73)")
           (let* ((generation
                    (generation-create-record
                     configuration
                     :git-commit "0123456789abcdef"
                     :mutation-checker checker))
                  (commit (image-commit-current configuration))
                  (script
                    (uiop:read-file-string
                     (generation-reconstruction-pathname generation))))
             (test-assert (= check-count 1)
                          "checkpoint capture checks staged live mutations once")
             (test-assert commit
                          "checkpoint capture automatically creates a private commit")
             (test-assert
              (string= (or (generation-image-commit-identifier generation) "")
                       (image-commit-identifier commit))
              "a generation records the exact private image commit")
             (test-assert
              (and (search "Return captured state." script)
                   (uiop:subpathp
                    (generation-reconstruction-pathname generation)
                    (generation-directory generation)))
              "a generation receives a contained full replay script")
             (test-assert
              (null (image-commit-pending-records configuration))
              "checkpoint capture consumes staged reconstructible mutations")))
      (setf (symbol-function 'test-generation-replay-target) previous-function
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-lineage-identifier* previous-lineage-identifier)
      (remhash (definition-key '(defun test-generation-replay-target () 0))
               *exploratory-definitions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Subsystem Tests --

(-> test-generation-manifest () null)
(defun test-generation-manifest ()
  "Test generation publication, loading, selection, and compatibility checks."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (generation
           (generation-tests--generation configuration
                                         "generation-under-test"
                                         "0123456789abcdef"))
         (runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record generation)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core generation)
           (generation-tests--write-reconstruction generation)
           (generation-publish configuration generation :probe-runner runner)
           (let ((loaded (generation-find configuration
                                          "generation-under-test")))
             (test-assert loaded
                          "a published generation appears in retained listings")
             (test-assert (not (generation-compatible-p loaded))
                          "a fake one-byte core is never reported as bootable")
             (test-assert
              (let ((*checkpoint-in-progress-p* t))
                (handler-case
                    (progn
                      (generation-select configuration loaded)
                      nil)
                  (checkpoint-error (condition)
                    (search "while a checkpoint publishes"
                            (autolith-error-message condition)))))
              "rollback selection cannot race asynchronous publication")
             (test-assert (= (generation-journal-position loaded) 27)
                          "generation manifests preserve mutation journal position")
             (test-assert
              (and (generation-reconstruction-pathname loaded)
                   (probe-file (generation-reconstruction-pathname loaded))
                   (search "Autolith image reconstruction script"
                           (uiop:read-file-string
                            (generation-reconstruction-pathname loaded))))
              "generation manifests retain a complete reconstruction script")
             (test-assert
              (string= (generation-identifier
                        (generation-selected configuration))
                       "generation-under-test")
              "publication atomically selects the ready generation")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (directory (merge-pathnames "legacy-generation/"
                                     (generation-root configuration)))
         (manifest (merge-pathnames "manifest.sexp" directory))
         (core (merge-pathnames "autolith.core" directory)))
    (unwind-protect
         (progn
           (generation--write-form-atomically
            manifest
            (list :generation
                  :version 1
                  :id "legacy-generation"
                  :core (namestring core)
                  :git-commit "0123456789abcdef"
                  :journal-position 11
                  :sbcl-version (lisp-implementation-version)
                  :operating-system (software-type)
                  :operating-system-version (software-version)
                  :architecture (machine-type)
                  :created-at 3999999999))
           (let ((legacy (generation-load-manifest manifest)))
             (test-assert
              (and (string= (generation-identifier legacy)
                            "legacy-generation")
                   (null (generation-reconstruction-pathname legacy)))
              "version-one generation manifests remain readable")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (wrong-commit
           (generation-tests--generation configuration
                                         "wrong-commit"
                                         "0123456789abcdef"))
         (other-identity
           (generation-tests--generation configuration
                                         "wrong-commit"
                                         "fedcba9876543210"))
         (wrong-runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record other-identity))))
         (corrupt
           (generation-tests--generation configuration
                                         "corrupt-core"
                                         "0123456789abcdef"))
         (missing-reconstruction
           (generation-tests--generation configuration
                                         "missing-reconstruction"
                                         "0123456789abcdef"))
         (missing-runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record
                      missing-reconstruction)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core wrong-commit)
           (generation-tests--write-reconstruction wrong-commit)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration
                                      wrong-commit
                                      :probe-runner wrong-runner)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':probe)))
            "publication rejects a core whose probe names another Git commit")
           (test-assert
            (generation-tests--unpublished-p configuration wrong-commit)
            "a wrong probe identity leaves every publication path untouched")
           (generation-tests--write-fake-core corrupt)
           (generation-tests--write-reconstruction corrupt)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration corrupt)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':probe)))
            "the production probe rejects a corrupt fake core")
           (test-assert
            (generation-tests--unpublished-p configuration corrupt)
            "a corrupt core leaves every publication path untouched")
           (generation-tests--write-fake-core missing-reconstruction)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration
                                      missing-reconstruction
                                      :probe-runner missing-runner)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':publish)))
            "publication rejects a missing reconstruction script")
           (test-assert
            (generation-tests--unpublished-p configuration
                                             missing-reconstruction)
            "missing reconstruction leaves publication paths untouched"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (generation-tests--test-rollback-control-path)
  (generation-tests--test-reconstruction-capture)
  nil)

(-> test-crash-capsule-correlation () null)
(defun test-crash-capsule-correlation ()
  "Test secret-free crash capsules and per-launch pointer publication."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "crash-capsule"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider nil
                          :tool-registry (make-instance 'tool-registry)
                          :worker nil
                          :agent nil
                          :ui nil))
         (pointer (merge-pathnames "crash-pointers/test-launch.path"
                                   (configuration-state-root configuration)))
         (previous-pointer (uiop:getenv "AUTOLITH_CRASH_POINTER")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "preserve crash context")
           (setf (application-rendered-sequence application) 42)
           (sb-posix:setenv "AUTOLITH_CRASH_POINTER" (namestring pointer) 1)
           (test-assert (string= (uiop:getenv "AUTOLITH_CRASH_POINTER")
                                 (namestring pointer))
                        "the launch pointer is visible in the active environment")
           (test-assert (uiop:subpathp pointer
                                       (configuration-state-root configuration))
                        "the launch pointer is contained by private Autolith state")
           (let* ((capsule
                    (application-write-crash-capsule
                     application
                     (make-condition 'simple-error
                                     :format-control "secret ~A"
                                     :format-arguments '("credential-value"))
                     :backtrace '((secret-frame "credential-value"))))
                  (record (read-portable-form capsule))
                  (mode (sb-posix:stat-mode
                         (sb-posix:stat (namestring capsule)))))
             (test-assert (= (logand mode #o777) #o600)
                          "crash capsules are private user state")
             (test-assert
              (not (search "credential-value"
                           (uiop:read-file-string capsule)))
              "crash capsules never serialize arbitrary condition arguments")
             (test-assert (= (getf (rest record) :rendered-sequence) 42)
                          "crash capsules retain scrollback presentation progress")
             (test-assert
              (string= (getf (rest record) :conversation-id) "crash-capsule")
              "crash capsules correlate persisted conversations")
             (test-assert
              (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (uiop:read-file-string pointer))
                       (namestring capsule))
              "the exact launch pointer names its own crash capsule"))
           (let* ((empty (conversation-create configuration
                                              :identifier "empty-crash"))
                  (empty-capsule
                    (progn
                      (setf (application-conversation application) empty)
                      (application-write-crash-capsule
                       application
                       (make-condition 'simple-error
                                       :format-control "empty crash"
                                       :format-arguments nil))))
                  (empty-record (read-portable-form empty-capsule)))
             (test-assert (null (getf (rest empty-record) :conversation-id))
                          "crashes do not advertise an unpersisted conversation")
             (test-assert (not (probe-file (conversation-pathname empty)))
                          "crash reporting does not materialize an empty conversation")))
      (if previous-pointer
          (sb-posix:setenv "AUTOLITH_CRASH_POINTER" previous-pointer 1)
          (sb-posix:unsetenv "AUTOLITH_CRASH_POINTER"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
