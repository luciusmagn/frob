(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-self-target () integer)
(defun test-self-target ()
  "Return the baseline value used by active-image mutation tests."
  0)

(defvar *test-self-setting* :baseline
  "The mutable binding used by private image-commit replay tests.")

(-> test-self-tools () null)
(defun test-self-tools ()
  "Test active definition installation, inspection, and form-aware persistence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (merge-pathnames "definitions.lisp" root))
         (previous-function (symbol-function 'test-self-target))
         (implementation-package (find-package '#:sb-ext))
         (implementation-name
           (format nil "AUTOLITH-ACTIVE-IMAGE-TEST-~A"
                   (string-upcase (make-identifier))))
         (implementation-source
           nil)
         (implementation-key nil)
         (implementation-record nil))
    (unwind-protect
         (progn
           (setf implementation-source
                 (format nil "(defun ~A () 4242)" implementation-name))
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return the installed test value.\" 42)")
           (test-assert (= (test-self-target) 42)
                        "self definition installation mutates the active image")
           (let ((records
                   (remove-if-not
                    (lambda (record)
                      (and (eq (first record) :mutation)
                           (eq (getf (rest record) :kind) :definition)
                           (string= (getf (rest record) :target)
                                    (definition-key
                                     '(defun test-self-target () 0)))))
                    (mutation-journal-read-records configuration))))
             (test-assert (= (length records) 2)
                          "definition installation journals two state records")
             (test-assert
              (and (non-empty-string-p (getf (rest (first records)) :id))
                   (string= (getf (rest (first records)) :id)
                            (getf (rest (second records)) :id)))
              "definition journal states share one stable mutation identifier"))
           (test-assert
            (search "Return the installed test value."
                    (self-inspect-symbol 'test-self-target))
            "active-image inspection exposes function documentation")
           (test-assert (sb-ext:package-locked-p implementation-package)
                        "the selected SBCL implementation package begins locked")
           (self-install-definition configuration
                                    implementation-source
                                    :package implementation-package)
           (multiple-value-bind (symbol status)
               (find-symbol implementation-name implementation-package)
             (declare (ignore status))
             (setf implementation-key
                   (definition-key (list 'defun symbol nil 4242)))
             (test-assert (and symbol
                               (fboundp symbol)
                               (= (funcall symbol) 4242))
                          "self definition installation can instrument an SBCL package"))
           (test-assert (sb-ext:package-locked-p implementation-package)
                        "active SBCL instrumentation restores the package lock")
           (setf implementation-record
                 (find-if
                  (lambda (candidate)
                    (and (eq (first candidate) :mutation)
                         (eq (getf (rest candidate) :kind) :definition)
                         (string= (or (getf (rest candidate) :package) "")
                                  "SB-EXT")
                         (eq (getf (rest candidate) :result) :installed)))
                  (mutation-journal-read-records configuration)))
           (test-assert implementation-record
                        "active implementation mutations journal their package")
           (let ((script (merge-pathnames "implementation-replay.lisp" root)))
             (image-commit-write-script
              script
              "implementation-replay"
              "Replay implementation instrumentation"
              (list (image-commit--record->entry implementation-record)))
             (self-call-with-package-unlocked
              implementation-package
              (lambda ()
                (let ((symbol (find-symbol implementation-name
                                           implementation-package)))
                  (when symbol
                    (when (fboundp symbol)
                      (fmakunbound symbol))
                    (unintern symbol implementation-package)))))
             (load script)
             (let ((symbol (find-symbol implementation-name
                                        implementation-package)))
               (test-assert (and symbol
                                 (fboundp symbol)
                                 (= (funcall symbol) 4242)
                                 (sb-ext:package-locked-p
                                  implementation-package))
                            "private replay reconstructs a locked-package definition")))
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
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "self-sbcl-source"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
                  (result
                    (tool-execute
                     (tool-registry-find (make-default-tool-registry)
                                         "self"
                                         "source")
                     context
                     (json-object "symbol" "CL:MAPCAR"
                                  "kind" "function"))))
             (test-assert
              (and (tool-result-success-p result)
                   (search "src/code/list.lisp"
                           (tool-result-content result)))
              "self.source falls back to matching active SBCL source"))
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
      (when implementation-key
        (remhash implementation-key *exploratory-definitions*))
      (self-call-with-package-unlocked
       implementation-package
       (lambda ()
         (let ((symbol (find-symbol implementation-name
                                    implementation-package)))
           (when symbol
             (when (fboundp symbol)
               (fmakunbound symbol))
             (unintern symbol implementation-package)))))
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
  "Test private live-mutation commits, replay, and legacy overlay migration."
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
         (previous-setting *test-self-setting*)
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*)
         (active-check-count 0)
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              (incf active-check-count)
              "active checks passed"))))
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
           (self-git-command configuration
                             '("config" "user.name" "Autolith Test"))
           (self-git-command
            configuration
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
                        (error "Injected active check failure.")))))
                  (registry (make-default-tool-registry))
                  (persist-tool (tool-registry-find registry
                                                    "self"
                                                    "persist-definition"))
                  (set-tool (tool-registry-find registry "self" "set"))
                  (diff-tool (tool-registry-find registry "self" "diff"))
                  (commit-tool (tool-registry-find registry "self" "commit"))
                  (broken (merge-pathnames
                           "broken.lisp"
                           (configuration-overlay-root configuration))))
             (overlay-write
              configuration
              "(defun test-legacy-image-target)"
              "(defun test-legacy-image-target () \"Return migrated state.\" 9)")
             (eval '(define-constant +overlay-constant-trial+ 1 :test #'=))
             (overlay-write
              configuration
              "(alexandria:define-constant +overlay-constant-trial+)"
              (format nil
                      "(define-constant +overlay-constant-trial+ 2 :test #'=)"))
             (ensure-directories-exist broken)
             (with-open-file (stream broken
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (write-string "(defun test-broken-overlay (" stream))
             (setf *image-state-initialized-p* nil
                   *active-image-commit-identifier* nil
                   *active-image-history-commit* nil
                   *active-image-lineage-identifier* nil)
             (let ((failures (image-state-load configuration)))
               (test-assert (= (length failures) 1)
                            "legacy startup reports one broken overlay")
               (test-assert (= (test-legacy-image-target) 9)
                            "legacy startup loads valid definitions past failures")
               (test-assert (= (symbol-value '+overlay-constant-trial+) 2)
                            "legacy constant overlays continue deliberately"))
             (delete-file broken)
             (test-assert (null (image-commit-current configuration))
                          "legacy overlays begin without a private image commit")
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
              "a failing active check rejects private persistence")
             (test-assert (= (test-self-target) 0)
                          "a rejected definition restores the previous behavior")
             (test-assert
              (null (image-commit--pointer-identifier configuration))
              "a rejected definition publishes no private commit")
             (let* ((result
                      (tool-execute
                       persist-tool
                       context
                       (json-object
                        "definition"
                        "(defun test-self-target () \"Return the durable value.\" 84)")))
                    (first-commit (image-commit-current configuration)))
               (test-assert (tool-result-success-p result)
                            "durable definition persistence succeeds")
               (test-assert (search "private image commit"
                                    (tool-result-content result))
                            "the persistence result identifies private storage")
               (test-assert first-commit
                            "durable persistence selects a private commit")
               (test-assert
                (image-history--commit-p
                 (image-commit-history-commit first-commit))
                "durable persistence records a private Git commit")
               (test-assert
                (uiop:directory-exists-p
                 (merge-pathnames
                  ".git/"
                  (configuration-mutation-history-root configuration)))
                "durable persistence initializes private Git history")
               (test-assert
                (string=
                 (image-commit-history-commit first-commit)
                 (string-trim
                  '(#\Space #\Tab #\Newline #\Return)
                  (image-history--git-command
                   configuration '("rev-parse" "HEAD"))))
                "the selected snapshot names the committed Git history state")
               (test-assert
                (uiop:subpathp (image-commit-script-pathname first-commit)
                               (configuration-image-commit-root configuration))
                "the reconstruction script stays under private Autolith data")
               (test-assert
                (= (logand #o777
                           (sb-posix:stat-mode
                            (sb-posix:stat
                             (namestring
                              (image-commit-script-pathname first-commit)))))
                   #o444)
                "published private replay scripts are read-only")
               (test-assert
                (search "Return the durable value."
                        (uiop:read-file-string
                         (image-commit-script-pathname first-commit)))
                "the private script contains the complete definition")
               (test-assert
                (search "Return migrated state."
                        (uiop:read-file-string
                         (image-commit-script-pathname first-commit)))
                "the first private commit migrates legacy overlays")
               (test-assert (= active-check-count 1)
                            "durable persistence checks the active image once")
               (let ((first-identifier (image-commit-identifier first-commit)))
                 (tool-execute
                  persist-tool
                  context
                  (json-object
                   "definition"
                   "(defun test-self-target () \"Return the second value.\" 85)"))
                 (let* ((second-commit (image-commit-current configuration))
                        (script (uiop:read-file-string
                                 (image-commit-script-pathname second-commit))))
                   (test-assert
                    (string= (or (image-commit-parent-identifier second-commit) "")
                             first-identifier)
                    "private definition commits form an immutable lineage")
                   (test-assert
                    (and (search "Return the second value." script)
                         (not (search "Return the durable value." script)))
                    "a full replay snapshot retains only the effective definition"))))
             (test-assert (= (test-self-target) 85)
                          "private persistence installs the latest definition")
             (test-assert (= active-check-count 2)
                          "each durable definition is checked exactly once")
             (test-assert
              (search "Return the durable baseline."
                      (uiop:read-file-string source-pathname))
              "private persistence never modifies tracked source")
             (let ((mutation
                     (loop for value being the hash-values
                             of *durable-mutations*
                           when (and
                                 (string=
                                  (durable-mutation-target value)
                                  (definition-key
                                   '(defun test-self-target () 0)))
                                 (eq (durable-mutation-phase value) :durable))
                             return value)))
               (test-assert mutation
                            "private definition persistence becomes durable")
               (let ((identifier (durable-mutation-identifier mutation)))
                 (clrhash *durable-mutations*)
                 (durable-mutations-load configuration)
                 (test-assert
                  (eq (durable-mutation-phase
                       (gethash identifier *durable-mutations*))
                      :durable)
                  "durable private state replays from the journal")))
             (let ((failed-result
                     (handler-case
                         (progn
                           (tool-execute
                            set-tool
                            context
                            (json-object
                             "symbol" "*test-self-setting*"
                             "value" "(error \"Rejected setting.\")"))
                           nil)
                       (error ()
                         t))))
               (test-assert failed-result
                            "a failed self.set operation escapes as a failure"))
             (tool-execute
              set-tool
              context
              (json-object
               "symbol" "*test-self-setting*"
               "value" ":committed-setting"))
             (self-install-definition
              configuration
              "(defun test-self-target () \"Return the staged value.\" 86)")
             (let* ((set-records
                      (remove-if-not
                       (lambda (record)
                         (and (eq (first record) :mutation)
                              (eq (getf (rest record) :kind) :set)
                              (string= (getf (rest record) :target)
                                       "AUTOLITH::*TEST-SELF-SETTING*")))
                       (mutation-journal-read-records configuration)))
                    (failed-id (getf (rest (first set-records)) :id))
                    (installed-id (getf (rest (third set-records)) :id)))
               (test-assert (= (length set-records) 4)
                            "failed and successful sets each journal two states")
               (test-assert
                (and (string= failed-id
                              (getf (rest (second set-records)) :id))
                     (string= installed-id
                              (getf (rest (fourth set-records)) :id))
                     (not (string= failed-id installed-id)))
                "each set operation keeps one distinct stable identifier"))
             (let ((diff
                     (tool-execute diff-tool context (json-object))))
               (test-assert
                (and (tool-result-success-p diff)
                     (search "Return the staged value."
                             (tool-result-content diff))
                     (search ":committed-setting"
                             (tool-result-content diff)))
                "self.diff shows pending reconstructible image mutations"))
             (test-assert (= (length (image-commit-pending-records configuration))
                             2)
                          "only successful uncommitted mutations are staged")
             (with-open-file (stream source-pathname
                                     :direction :output
                                     :if-exists :append
                                     :external-format :utf-8)
               (format stream "~%;; A user-made repository change.~%"))
             (let* ((head-before
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD"))))
                    (parent-before
                      (image-commit-identifier
                       (image-commit-current configuration)))
                    (result
                      (tool-execute
                       commit-tool
                       outside-context
                       (json-object
                        "title" "Persist staged live mutations")))
                    (committed (image-commit-current configuration))
                    (script
                      (uiop:read-file-string
                       (image-commit-script-pathname committed)))
                    (head-after
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD")))))
               (test-assert (tool-result-success-p result)
                            "self.commit persists staged live mutations")
               (test-assert
                (string= (or (image-commit-parent-identifier committed) "")
                         parent-before)
                "self.commit advances the active private lineage")
               (test-assert
                (and (search "Return the staged value." script)
                     (search ":committed-setting" script))
                "self.commit writes a complete executable replay script")
               (test-assert
                (uiop:subpathp (image-commit-manifest-pathname committed)
                               (configuration-data-root configuration))
                "self.commit writes only beneath private Autolith data")
               (test-assert
                (uiop:subpathp
                 (configuration-current-image-commit-path configuration)
                 (configuration-state-root configuration))
                "self.commit selects its result beneath private Autolith state")
               (test-assert (string= head-before head-after)
                            "self.commit never changes workspace Git history")
               (test-assert
                (string=
                 (image-commit-history-commit committed)
                 (string-trim
                  '(#\Space #\Tab #\Newline #\Return)
                  (image-history--git-command
                   configuration '("rev-parse" "HEAD"))))
                "self.commit advances private Git history")
               (test-assert
                (>= (parse-integer
                     (image-history--git-command
                      configuration '("rev-list" "--count" "HEAD"))
                     :junk-allowed t)
                    3)
                "each durable snapshot receives a private Git commit")
               (let* ((pointer
                        (read-portable-form
                         (configuration-current-image-commit-path
                          configuration)))
                      (properties (rest pointer)))
                 (test-assert
                  (and (= (getf properties :version) 2)
                       (string=
                        (getf properties :history-commit)
                        (image-commit-history-commit committed)))
                  "the atomic selection binds image and Git commit identities"))
               (test-assert
                (search "A user-made repository change."
                        (uiop:read-file-string source-pathname))
                "self.commit leaves tracked workspace changes untouched"))
             (test-assert (= active-check-count 3)
                          "self.commit checks the active image exactly once")
             (test-assert
              (null (image-commit-pending-records configuration))
              "self.commit consumes every successful staged mutation")
             (let* ((committed (image-commit-current configuration))
                    (identifier (image-commit-identifier committed))
                    (canonical-directory
                      (image-commit-directory committed))
                    (history-directory
                      (image-history--artifact-directory
                       configuration identifier)))
               (uiop:delete-directory-tree
                history-directory :validate t :if-does-not-exist :ignore)
               (uiop:delete-directory-tree
                canonical-directory :validate t :if-does-not-exist :ignore)
               (setf (symbol-function 'test-self-target) previous-function
                     *test-self-setting* :baseline
                     *image-state-initialized-p* nil
                     *active-image-commit-identifier* nil
                     *active-image-history-commit* nil
                     *active-image-lineage-identifier* nil)
               (test-assert (null (image-state-load configuration))
                            "private startup replay loads without failures")
               (test-assert
                (and (probe-file
                      (merge-pathnames "manifest.sexp" canonical-directory))
                     (probe-file
                      (merge-pathnames "reconstruct.lisp"
                                       canonical-directory)))
                "startup restores deleted replay artifacts from Git objects"))
             (test-assert (= (test-self-target) 86)
                          "startup replay reconstructs committed definitions")
             (test-assert (eq *test-self-setting* :committed-setting)
                          "startup replay reconstructs committed global state")
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     commit-tool
                     context
                     (json-object "title" "Commit nothing"))
                    nil)
                (image-commit-error ()
                  t))
              "self.commit refuses an empty private commit")))
      (setf (symbol-function 'test-self-target) previous-function
            *test-self-setting* previous-setting
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (when (fboundp 'test-legacy-image-target)
        (fmakunbound 'test-legacy-image-target))
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
