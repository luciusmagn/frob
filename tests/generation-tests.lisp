(in-package #:frob)

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

(-> generation-tests--generation (configuration string string) generation)
(defun generation-tests--generation (configuration identifier git-commit)
  "Return a pending test generation named IDENTIFIER at GIT-COMMIT."
  (let ((directory (merge-pathnames (format nil "~A/" identifier)
                                    (generation-root configuration))))
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname (merge-pathnames "frob.core" directory)
                   :temporary-core-pathname
                   (merge-pathnames ".frob.core.tmp" directory)
                   :manifest-pathname
                   (merge-pathnames "manifest.sexp" directory)
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

(-> generation-tests--unpublished-p (configuration generation) boolean)
(defun generation-tests--unpublished-p (configuration generation)
  "Return true when failed GENERATION left no visible publication artifacts."
  (and (probe-file (generation-temporary-core-pathname generation))
       (not (probe-file (generation-core-pathname generation)))
       (not (probe-file (generation-manifest-pathname generation)))
       (not (probe-file (generation-current-pathname configuration)))
       (eq (generation-status generation) ':pending)))


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
                            (frob-error-message condition)))))
              "rollback selection cannot race asynchronous publication")
             (test-assert (= (generation-journal-position loaded) 27)
                          "generation manifests preserve mutation journal position")
             (test-assert
              (string= (generation-identifier
                        (generation-selected configuration))
                       "generation-under-test")
              "publication atomically selects the ready generation")))
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
                                         "0123456789abcdef")))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core wrong-commit)
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
            "a corrupt core leaves every publication path untouched"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
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
         (previous-pointer (uiop:getenv "FROB_CRASH_POINTER")))
    (unwind-protect
         (progn
           (setf (application-rendered-sequence application) 42)
           (sb-posix:setenv "FROB_CRASH_POINTER" (namestring pointer) 1)
           (test-assert (string= (uiop:getenv "FROB_CRASH_POINTER")
                                 (namestring pointer))
                        "the launch pointer is visible in the active environment")
           (test-assert (uiop:subpathp pointer
                                       (configuration-state-root configuration))
                        "the launch pointer is contained by private Frob state")
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
              (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (uiop:read-file-string pointer))
                       (namestring capsule))
              "the exact launch pointer names its own crash capsule")))
      (if previous-pointer
          (sb-posix:setenv "FROB_CRASH_POINTER" previous-pointer 1)
          (sb-posix:unsetenv "FROB_CRASH_POINTER"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
