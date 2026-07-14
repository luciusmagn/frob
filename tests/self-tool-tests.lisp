(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-self-target () integer)
(defun test-self-target ()
  "Return the baseline value used by active-image mutation tests."
  0)

(-> test-self-tools () null)
(defun test-self-tools ()
  "Test active definition installation, inspection, and form-aware persistence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (merge-pathnames "definitions.lisp" root))
         (previous-function (symbol-function 'test-self-target)))
    (unwind-protect
         (progn
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return the installed test value.\" 42)")
           (test-assert (= (test-self-target) 42)
                        "self definition installation mutates the active image")
           (test-assert
            (search "Return the installed test value."
                    (self-inspect-symbol 'test-self-target))
            "active-image inspection exposes function documentation")
           (let* ((source-root (merge-pathnames "source/" root))
                  (source-pathname (merge-pathnames "src/sample.lisp" source-root))
                  (source-configuration
                    (test-configuration-for-source-root source-root)))
             (ensure-directories-exist source-pathname)
             (with-open-file (stream source-pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (format stream
                       "(in-package #:autolith)~%~%(defun test-self-target () ~
                        \"Tracked source documentation.\" 0)~%"))
             (let* ((definitions
                      (self-tracked-definitions source-configuration
                                                'test-self-target))
                    (rendered
                      (self-render-tracked-definitions definitions
                                                       'test-self-target)))
               (test-assert (= (length definitions) 1)
                            "tracked source inspection finds the complete definition")
               (test-assert (search "src/sample.lisp" rendered)
                            "tracked source inspection reports its repository path")
               (test-assert (search "Tracked source documentation." rendered)
                            "tracked source inspection returns exact definition text")))
           (test-assert
            (equal (definition-signature
                    '(defmethod sample-operation ((left string) right) left))
                   (definition-signature
                    '(defmethod sample-operation ((value string) ignored) value)))
            "method identity ignores parameter names while retaining specializers")
           (test-assert
            (definition-form-p '(defun (setf sample-value) (value object)
                                  (declare (ignore object))
                                  value))
            "definition identity accepts SETF function names")
           (test-assert (definition-form-p '(defparameter *sample-value* 42))
                        "durable definitions include mutable global parameters")
           (test-assert
            (handler-case
                (progn
                  (self-validate-commit-paths configuration
                                              (json-array
                                               "script/build-recovery"))
                  nil)
              (tool-error ()
                t))
            "normal self commits cannot replace the pristine recovery builder")
           (let ((original
                   (make-condition 'simple-error
                                   :format-control "original failure"
                                   :format-arguments nil)))
             (test-assert
              (handler-case
                  (progn
                    (self-restore-definition
                     "(defun test-self-target () 0)"
                     original
                     :installer
                     (lambda (definition source)
                       (declare (ignore definition source))
                       (error "restoration failure")))
                    nil)
                (active-image-corruption (condition)
                  (and (eq (active-image-corruption-original-condition condition)
                           original)
                       (typep
                        (active-image-corruption-restoration-condition condition)
                        'serious-condition))))
              "a restoration failure preserves both conditions and escapes tool handling"))
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (format stream
                     "; preserve this comment~%~%(defun first-definition () 1)~%~%(defun test-self-target () 0)~%"))
           (source-replace-definition
            pathname
            "(defun test-self-target () \"Persisted documentation.\" 84)")
           (let ((updated (uiop:read-file-string pathname)))
             (test-assert (search "; preserve this comment" updated)
                          "form-aware replacement preserves preceding comments")
             (test-assert (search "Persisted documentation." updated)
                          "form-aware replacement writes the complete definition")
             (test-assert (search "(defun first-definition () 1)" updated)
                          "form-aware replacement preserves neighboring forms")))
      (setf (symbol-function 'test-self-target) previous-function)
      (remhash (definition-key '(defun test-self-target () 0))
               *exploratory-definitions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-self-restart-selection () null)
(defun test-self-restart-selection ()
  "Test restart discovery and selection through the active-image tools."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "restarts"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (registry (make-default-tool-registry))
                (eval-tool (tool-registry-find registry "self" "eval")))
           (labels ((run (&rest arguments)
                      "Execute self.eval with ARGUMENTS through the registry."
                      (tool-registry-execute-call
                       registry
                       (json-object "namespace" "self"
                                    "name" "eval"
                                    "arguments" (json-encode
                                                 (apply #'json-object
                                                        arguments)))
                       context)))
             (declare (ignorable eval-tool))
             (let ((result (run "form"
                                "(cerror \"Keep going anyway.\" \"Deliberate stop.\")")))
               (test-assert (not (tool-result-success-p result))
                            "correctable conditions fail without a restart")
               (test-assert (search "Available restarts"
                                    (tool-result-content result))
                            "failures enumerate the available restarts")
               (test-assert (search "CONTINUE" (tool-result-content result))
                            "the continue restart is offered")
               (test-assert (not (search "  ABORT"
                                         (tool-result-content result)))
                            "the abort restart is never offered"))
             (test-assert (tool-result-success-p
                           (run "form"
                                "(cerror \"Keep going anyway.\" \"Deliberate stop.\")"
                                "restart" "CONTINUE"))
                          "selecting continue completes the operation")
             (let ((result (run "form"
                                "(restart-case (error \"Needs a value.\") (use-value (value) value))"
                                "restart" "USE-VALUE"
                                "restart-value" "(* 6 7)")))
               (test-assert (and (tool-result-success-p result)
                                 (search "42" (tool-result-content result)))
                            "value restarts receive the evaluated value"))
             (test-assert (not (tool-result-success-p
                                (run "form"
                                     "(cerror \"Keep going.\" \"Stop.\")"
                                     "restart" "NO-SUCH-RESTART")))
                          "unknown restart names still fail with the menu")
             (test-assert (tool-result-success-p
                           (run "form"
                                "(define-constant +self-restart-trial+ 1 :test #'=)"))
                          "defining a fresh constant succeeds")
             (test-assert (not (tool-result-success-p
                                (run "form"
                                     "(define-constant +self-restart-trial+ 2 :test #'=)")))
                          "conflicting constant redefinition asks for a restart")
             (test-assert (tool-result-success-p
                           (run "form"
                                "(define-constant +self-restart-trial+ 2 :test #'=)"
                                "restart" "CONTINUE"))
                          "continue redefines the constant deliberately")
             (test-assert (= (symbol-value '+self-restart-trial+) 2)
                          "the constant carries the deliberately chosen value")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-durable-self-mutation () null)
(defun test-durable-self-mutation ()
  "Test checked live installation, overlay persistence, and startup replay."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-durable-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration (test-configuration-for-source-root source-root))
         (outside-workspace
           (uiop:ensure-directory-pathname
            (merge-pathnames "unrelated-workspace/"
                             (uiop:temporary-directory))))
         (source-pathname (merge-pathnames "src/definitions.lisp" source-root))
         (previous-function (symbol-function 'test-self-target))
         (active-check-count 0)
         (source-check-count 0)
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              (incf active-check-count)
              "active checks passed")
            :source-callback
            (lambda (checked-configuration paths)
              (declare (ignore checked-configuration paths))
              (incf source-check-count)
              "source checks passed"))))
    (unwind-protect
         (progn
           (ensure-directories-exist source-pathname)
           (with-open-file (stream source-pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (format stream
                     "(in-package #:autolith)~%~%(defun test-self-target () \"Return the durable baseline.\" 0)~%"))
           (self-git-command configuration '("init" "--quiet"))
           (self-git-command configuration '("config" "user.name" "Autolith Test"))
           (self-git-command configuration
                             '("config" "user.email" "autolith-test@example.invalid"))
           (self-git-command configuration '("add" "src/definitions.lisp"))
           (self-git-command configuration
                             '("commit" "--quiet" "-m" "Create baseline"))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "durable-mutation"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :mutation-checker checker))
                  (outside-configuration
                    (make-instance
                     'configuration
                     :source-root source-root
                     :working-directory outside-workspace
                     :data-root (configuration-data-root configuration)
                     :state-root (configuration-state-root configuration)
                     :cache-root (configuration-cache-root configuration)
                     :codex-auth-path
                     (configuration-codex-auth-path configuration)
                     :model (configuration-model configuration)
                     :reasoning-effort
                     (configuration-reasoning-effort configuration)
                     :provider-endpoint
                     (configuration-provider-endpoint configuration)))
                  (outside-context
                    (make-instance 'tool-context
                                   :configuration outside-configuration
                                   :worker nil
                                   :conversation conversation
                                   :mutation-checker checker))
                  (failing-active-context
                    (make-instance
                     'tool-context
                     :configuration configuration
                     :worker nil
                     :conversation conversation
                     :mutation-checker
                     (make-instance
                      'callback-mutation-checker
                      :active-callback
                      (lambda (checked-configuration definition-source)
                        (declare (ignore checked-configuration
                                         definition-source))
                        (error "Injected active check failure."))
                      :source-callback
                      (lambda (checked-configuration paths)
                        (declare (ignore checked-configuration paths))
                        "unused"))))
                  (registry (make-default-tool-registry))
                  (persist-tool (tool-registry-find registry
                                                    "self"
                                                    "persist-definition"))
                  (commit-tool (tool-registry-find registry "self" "commit"))
                  (overlay (overlay-pathname
                            configuration
                            (definition-key '(defun test-self-target () 0)))))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     persist-tool
                     failing-active-context
                     (json-object
                      "definition"
                      "(defun test-self-target () \"Return a rejected value.\" 13)"))
                    nil)
                (error ()
                  t))
              "a failing active check rejects durable persistence")
             (test-assert (= (test-self-target) 0)
                          "a rejected definition restores the previous behavior")
             (test-assert (not (uiop:file-exists-p overlay))
                          "a rejected definition writes no overlay")
             (let ((result
                     (tool-execute
                      persist-tool
                      context
                      (json-object
                       "definition"
                       "(defun test-self-target () \"Return the durable value.\" 84)"))))
               (test-assert (tool-result-success-p result)
                            "durable persistence succeeds after active checks")
               (test-assert (search "overlay" (tool-result-content result))
                            "the persistence result names the overlay"))
             (test-assert (= (test-self-target) 84)
                          "durable persistence installs the live definition")
             (test-assert (= active-check-count 1)
                          "durable persistence runs active checks exactly once")
             (test-assert (uiop:file-exists-p overlay)
                          "durable persistence writes the overlay file")
             (test-assert (search "Return the durable value."
                                  (uiop:read-file-string overlay))
                          "the overlay carries the complete definition")
             (test-assert (search "Return the durable baseline."
                                  (uiop:read-file-string source-pathname))
                          "the tracked source is never modified")
             (let ((mutation
                     (loop for value being the hash-values of *durable-mutations*
                           when (and (string= (durable-mutation-target value)
                                              (definition-key
                                               '(defun test-self-target () 0)))
                                     (eq (durable-mutation-phase value)
                                         :durable))
                             return value)))
               (test-assert mutation
                            "overlay persistence is durable immediately")
               (let ((identifier (durable-mutation-identifier mutation)))
                 (clrhash *durable-mutations*)
                 (durable-mutations-load configuration)
                 (let ((reloaded (gethash identifier *durable-mutations*)))
                   (test-assert (and reloaded
                                     (eq (durable-mutation-phase reloaded)
                                         :durable))
                                "durable overlay state replays from the journal"))))
             (tool-execute
              persist-tool
              context
              (json-object
               "definition"
               "(defun test-self-target () \"Return the second value.\" 85)"))
             (test-assert (= (test-self-target) 85)
                          "overlay persistence replaces the live definition")
             (let ((overlay-source (uiop:read-file-string overlay)))
               (test-assert (and (search "Return the second value."
                                         overlay-source)
                                 (not (search "Return the durable value."
                                              overlay-source)))
                            "the overlay holds exactly the newest definition"))
             (setf (symbol-function 'test-self-target) previous-function)
             (test-assert (null (overlay-load-all configuration))
                          "overlay loading reports no failures")
             (test-assert (= (test-self-target) 85)
                          "startup overlay loading restores definitions")
             (let ((broken (merge-pathnames
                            "broken.lisp"
                            (configuration-overlay-root configuration))))
               (with-open-file (stream broken
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string "(defun test-broken-overlay (" stream))
               (let ((failures (overlay-load-all configuration)))
                 (test-assert (= (length failures) 1)
                              "a broken overlay is reported as one failure")
                 (test-assert (= (test-self-target) 85)
                              "later overlays still load past a broken one")
                 (delete-file broken)))
             (eval '(define-constant +overlay-constant-trial+ 1 :test #'=))
             (overlay-write configuration
                            "(alexandria:define-constant +overlay-constant-trial+)"
                            (format nil "(define-constant ~
                                         +overlay-constant-trial+ 2 ~
                                         :test #'=)"))
             (test-assert (null (overlay-load-all configuration))
                          "constant overlays replay without re-asking")
             (test-assert (= (symbol-value '+overlay-constant-trial+) 2)
                          "constant overlays continue deliberately at startup")
             (with-open-file (stream source-pathname
                                     :direction :output
                                     :if-exists :append
                                     :external-format :utf-8)
               (format stream "~%;; A user-made repository change.~%"))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     commit-tool
                     outside-context
                     (json-object
                      "title" "Misroute an unrelated workspace commit"
                      "paths" (json-array "src/definitions.lisp")))
                    nil)
                (tool-error (condition)
                  (search "current workspace" (autolith-error-message condition))))
              "self.commit refuses Autolith source commits from another workspace")
             (test-assert (= source-check-count 0)
                          "a refused cross-workspace commit runs no source checks")
             (test-assert
              (tool-result-success-p
               (tool-execute
                commit-tool
                context
                (json-object
                 "title" "Record a user-directed change"
                 "paths" (json-array "src/definitions.lisp"))))
              "self.commit still serves explicit user-directed commits")
             (test-assert (= source-check-count 1)
                          "self.commit runs clean-source checks exactly once")))
      (setf (symbol-function 'test-self-target) previous-function)
      (let ((test-identifiers nil))
        (maphash
         (lambda (identifier mutation)
           (when (string= (durable-mutation-target mutation)
                          (definition-key '(defun test-self-target () 0)))
             (push identifier test-identifiers)))
         *durable-mutations*)
        (dolist (identifier test-identifiers)
          (remhash identifier *durable-mutations*)))
      (uiop:delete-directory-tree source-root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
