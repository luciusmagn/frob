(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test--write-sparse-lisp-core (pathname) pathname)
(defun test--write-sparse-lisp-core (pathname)
  "Write a sparse file large enough to pass saved-core shape validation."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (file-position stream +minimum-lisp-image-core-size+)
    (write-byte 0 stream))
  pathname)

(-> test-lisp-image-manifests () null)
(defun test-lisp-image-manifests ()
  "Test immutable saved worker-image manifests, notes, and compatibility."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "instrumented-compiler")
         (directory (lisp-image--directory configuration identifier))
         (core (merge-pathnames "worker.core" directory)))
    (unwind-protect
         (progn
           (test--write-sparse-lisp-core core)
           (let ((image
                   (lisp-image-publish-manifest
                    configuration
                    identifier
                    +pristine-lisp-image-identifier+
                    "Traces compiler type derivation for comparison."
                    core
                    :source-commit "0123456789abcdef")))
             (test-assert (string= (lisp-image-identifier image) identifier)
                          "saved Lisp images retain their identifier")
             (test-assert
              (string= (lisp-image-note image)
                       "Traces compiler type derivation for comparison.")
              "saved Lisp images retain their durable note")
             (test-assert (lisp-image-compatible-p image)
                          "a manifest written by this runtime is compatible")
             (test-assert
              (search "instrumented-compiler"
                      (lisp-image-render-inventory configuration))
              "the image inventory reminds the model about saved images")
             (test-assert
              (search "Traces compiler type derivation"
                      (lisp-image-prompt-notes configuration))
              "the prompt inventory includes durable image notes"))
           (handler-case
               (progn
                 (lisp-image-publish-manifest
                  configuration
                  identifier
                  +pristine-lisp-image-identifier+
                  "A duplicate image."
                  core)
                 (test-assert nil "saved image identifiers are immutable"))
             (lisp-image-error ()
               (test-assert t "saved image identifiers are immutable")))
           (handler-case
               (progn
                 (lisp-image--validate-identifier "pristine")
                 (test-assert nil "the pristine image name is reserved"))
             (lisp-image-error ()
               (test-assert t "the pristine image name is reserved"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-lisp-worker-protocol () null)
(defun test-lisp-worker-protocol ()
  "Test portable worker request execution and condition reporting."
  (let ((success
          (worker-handle-request
           '(:request :id 1 :operation :eval :arguments (:form "(+ 20 22)"))))
        (failure
          (worker-handle-request
           '(:request :id 2 :operation :eval :arguments (:form "(/ 1 0)")))))
    (test-assert (eq (getf (rest success) :status) :ok)
                 "the worker evaluates a valid request")
    (test-assert (equal (getf (rest success) :values) '("42"))
                 "the worker returns rendered values")
    (test-assert (eq (getf (rest failure) :status) :error)
                 "the worker turns evaluation conditions into protocol errors")
    (test-assert (non-empty-string-p (getf (rest failure) :message))
                 "worker protocol errors carry a readable condition report"))
  (let ((previous-command (uiop:getenv "AUTOLITH_SBCL")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_SBCL" "/tmp/autolith-test-sbcl" 1)
           (test-assert (string= (lisp-worker-sbcl-command)
                                 "/tmp/autolith-test-sbcl")
                        "the disposable worker honors the configured SBCL")
           (sb-posix:setenv "AUTOLITH_SBCL" "" 1)
           (test-assert (string= (lisp-worker-sbcl-command) "sbcl")
                        "the disposable worker falls back to PATH"))
      (if previous-command
          (sb-posix:setenv "AUTOLITH_SBCL" previous-command 1)
          (sb-posix:unsetenv "AUTOLITH_SBCL"))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (worker (lisp-worker-create configuration)))
    (unwind-protect
         (let ((response (lisp-worker-request worker :eval '(:form "(+ 40 2)"))))
           (test-assert (eq (getf (rest response) :status) :ok)
                        "the disposable worker starts through its direct active loader")
           (test-assert (equal (getf (rest response) :values) '("42"))
                        "the launched worker completes its isolated protocol request"))
      (lisp-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pool (lisp-worker-pool-create configuration)))
    (unwind-protect
         (let* ((alpha (lisp-worker-pool-start pool "alpha" "pristine"))
                (beta (lisp-worker-pool-start pool "beta" "pristine")))
           (lisp-worker-request alpha :eval '(:form "(defparameter *pool-value* 41)"))
           (let ((alpha-result
                   (lisp-worker-request alpha :eval '(:form "(1+ *pool-value*)")))
                 (beta-result
                   (lisp-worker-request beta :eval '(:form "(boundp '*pool-value*)"))))
             (test-assert (equal (getf (rest alpha-result) :values) '("42"))
                          "one named REPL retains its own heap state")
             (test-assert (equal (getf (rest beta-result) :values) '("NIL"))
                          "named REPLs do not share heap state"))
           (let* ((registry (make-default-tool-registry))
                  (conversation
                    (conversation-create configuration :identifier "repl-routing"))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker pool
                                          :conversation conversation))
                  (result
                    (tool-execute
                     (tool-registry-find registry "lisp" "eval")
                     context
                     (json-object "form" "(1+ *pool-value*)"
                                  "repl" "alpha"))))
             (test-assert (and (tool-result-success-p result)
                               (search "42" (tool-result-content result)))
                          "lisp.eval routes requests to the named REPL"))
           (test-assert (search "alpha  running  image pristine"
                                (lisp-worker-pool-render pool))
                        "the worker pool lists each active REPL and image")
           (handler-case
               (progn
                 (lisp-worker-pool-start pool "alpha" "another-image")
                 (test-assert nil
                              "an existing REPL never switches images implicitly"))
             (worker-error ()
               (test-assert t
                            "an existing REPL never switches images implicitly")))
           (lisp-worker-pool-reset pool "alpha" "pristine")
           (let ((result
                   (lisp-worker-request
                    (lisp-worker-pool-worker pool "alpha")
                    :eval
                    '(:form "(boundp '*pool-value*)"))))
             (test-assert (equal (getf (rest result) :values) '("NIL"))
                          "reset replaces only the selected REPL heap"))
           (lisp-worker-pool-stop pool "beta")
           (test-assert (not (search "beta" (lisp-worker-pool-render pool)))
                        "stopping one REPL leaves it out of the pool"))
      (lisp-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-lisp-worker-image-snapshot () null)
(defun test-lisp-worker-image-snapshot ()
  "Test saving a modified REPL core and starting an independent clone from it."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pool (lisp-worker-pool-create configuration)))
    (unwind-protect
         (let ((source (lisp-worker-pool-start pool "source" "pristine")))
           (lisp-worker-request
            source
            :eval
            '(:form "(defparameter *saved-worker-marker* 9001)"))
           (let ((image
                   (lisp-worker-save-image
                    configuration
                    source
                    "diddled"
                    "Carries a marker proving the modified SBCL heap was retained.")))
             (test-assert
              (and (string= (lisp-image-identifier image) "diddled")
                   (lisp-image--plausible-core-p
                    (lisp-image-core-pathname image)))
              "saving a named REPL publishes a plausible immutable core")
             (test-assert (lisp-worker-running-p source)
                          "saving an image leaves the parent REPL running")
             (let* ((clone (lisp-worker-pool-start pool "clone" "diddled"))
                    (clone-result
                      (lisp-worker-request
                       clone
                       :eval
                       '(:form "*saved-worker-marker*")))
                    (pristine
                      (lisp-worker-pool-start pool "control" "pristine"))
                    (pristine-result
                      (lisp-worker-request
                       pristine
                       :eval
                       '(:form "(boundp '*saved-worker-marker*)"))))
               (test-assert
                (equal (getf (rest clone-result) :values) '("9001"))
                "a REPL started from the saved image inherits its modified heap")
               (test-assert
                (equal (getf (rest pristine-result) :values) '("NIL"))
                "a pristine comparison REPL excludes saved-image modifications")
               (test-assert
                (search "clone  running  image diddled"
                        (lisp-worker-pool-render pool))
                "the pool identifies which REPL uses the modified image"))))
      (lisp-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
