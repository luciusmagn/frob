(in-package #:autolith)

;;;; -- In-Process Task Orchestration Tests --

(defvar *task-test-command-decision* nil
  "The command decision observed by the task child executor test.")

(defvar *task-test-effect-count* 0
  "The number of deliberately observable trailing tool executions.")

(defvar *task-test-reader-evaluated-p* nil
  "Whether unsafe reader evaluation ran while parsing a native role file.")

(defclass task-test-authorization-tool (tool)
  ()
  (:documentation "Ask the supplied tool context to authorize one harmless command."))

(defmethod tool-execute
    ((tool task-test-authorization-tool)
     (context tool-context)
     (arguments hash-table))
  "Record and return the command decision supplied through CONTEXT."
  (declare (ignore tool arguments))
  (setf *task-test-command-decision*
        (tool-context-authorize-command
         context
         "true"
         (configuration-working-directory
          (tool-context-configuration context))))
  (tool-success (string-downcase *task-test-command-decision*)))

(defclass task-test-effect-tool (tool)
  ()
  (:documentation "Record whether a call after terminal yield was executed."))

(defclass task-test-abort-tool (tool)
  ()
  (:documentation "Signal the task cancellation control condition on execution."))

(defclass task-test-default-deny-tool (tool)
  ()
  (:documentation "Represent an ordinary extension that children must not inherit."))

(defclass task-test-child-safe-tool (task-test-default-deny-tool)
  ()
  (:documentation "Represent an extension that deliberately opts into child use."))

(defclass task-test-blocking-tool (task-test-child-safe-tool)
  ((lock
    :initform (make-lock "Autolith blocking tool test")
    :reader task-test-blocking-tool-lock
    :documentation "The lock protecting the test barrier.")
   (condition-variable
    :initform (make-condition-variable)
    :reader task-test-blocking-tool-condition-variable
    :documentation "The condition coordinating tool entry and release.")
   (started-p
    :initform nil
    :accessor task-test-blocking-tool-started-p
    :type boolean
    :documentation "True after a child enters the ordinary tool call.")
   (released-p
    :initform nil
    :accessor task-test-blocking-tool-released-p
    :type boolean
    :documentation "True when the test permits normal tool completion."))
  (:documentation "Block an ordinary child-safe tool call at a test barrier."))

(defclass task-test-publication-barrier ()
  ((lock
    :initform (make-lock "Autolith publication print test")
    :reader task-test-publication-barrier-lock
    :documentation "The lock protecting the publication barrier.")
   (condition-variable
    :initform (make-condition-variable)
    :reader task-test-publication-barrier-condition-variable
    :documentation "The condition coordinating artifact printing.")
   (reached-p
    :initform nil
    :accessor task-test-publication-barrier-reached-p
    :type boolean
    :documentation "True while terminal publication is printing the artifact.")
   (released-p
    :initform nil
    :accessor task-test-publication-barrier-released-p
    :type boolean
    :documentation "True when artifact printing may finish.")
   (failure
    :initarg :failure
    :initform nil
    :reader task-test-publication-barrier-failure
    :type (option keyword)
    :documentation "The optional ordinary error or task abort signalled on release."))
  (:documentation "A readable test value that pauses terminal artifact output."))

(defmethod tool-child-safe-p ((tool task-test-child-safe-tool))
  "Permit this test extension to cross the child capability boundary."
  (declare (ignore tool))
  t)

(defmethod tool-execute
    ((tool task-test-blocking-tool)
     (context tool-context)
     (arguments hash-table))
  "Wait at a test barrier until cancellation unwinds or the test releases it."
  (declare (ignore context arguments))
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-started-p tool) t)
    (task--condition-broadcast
     (task-test-blocking-tool-condition-variable tool))
    (loop until (task-test-blocking-tool-released-p tool)
          do (condition-wait
              (task-test-blocking-tool-condition-variable tool)
              (task-test-blocking-tool-lock tool))))
  (tool-success "blocking tool released"))

(defmethod print-object
    ((barrier task-test-publication-barrier) stream)
  "Pause artifact printing until BARRIER is released, then print one keyword."
  (with-lock-held ((task-test-publication-barrier-lock barrier))
    (setf (task-test-publication-barrier-reached-p barrier) t)
    (task--condition-broadcast
     (task-test-publication-barrier-condition-variable barrier))
    (loop until (task-test-publication-barrier-released-p barrier)
          do (condition-wait
              (task-test-publication-barrier-condition-variable barrier)
              (task-test-publication-barrier-lock barrier))))
  (case (task-test-publication-barrier-failure barrier)
    (:error
     (error "Deliberate artifact publication failure."))
    (:abort
     (error 'task-aborted
            :message "Deliberate post-claim task abort."
            :reason :test-cancel))
    (otherwise
     (write-string ":TASK-TEST-PUBLICATION-BARRIER" stream))))

(defclass task-test-provider (model-provider)
  ((lock
    :initform (make-lock "Autolith task test provider")
    :reader task-test-provider-lock
    :documentation "The lock protecting deterministic request counters.")
   (mode
    :initarg :mode
   :reader task-test-provider-mode
   :type keyword
   :documentation
    "The :CONCURRENT, :NESTED, :NESTED-CANCEL, :BLOCKING-TOOL, :ASYNC-WAIT, or :MANIFEST script.")
   (active-count
    :initform 0
    :accessor task-test-provider-active-count
    :type (integer 0)
    :documentation "The provider requests currently executing.")
   (maximum-active-count
    :initform 0
    :accessor task-test-provider-maximum-active-count
    :type (integer 0)
    :documentation "The largest observed concurrent request count.")
   (request-count
    :initform 0
    :accessor task-test-provider-request-count
    :type (integer 0)
    :documentation "The total scripted requests observed.")
   (threads
    :initform nil
    :accessor task-test-provider-threads
    :type list
    :documentation "The distinct reusable workers that reached the provider."))
  (:documentation "A thread-safe provider for scheduler integration tests."))

(defmethod provider-with-configuration
    ((provider task-test-provider) (configuration configuration))
  "Share PROVIDER across test children while ignoring CONFIGURATION."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
    ((provider task-test-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return a deterministic yield or nested task call for PROVIDER."
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (let ((request-number nil))
    (with-lock-held ((task-test-provider-lock provider))
      (incf (task-test-provider-request-count provider))
      (incf (task-test-provider-active-count provider))
      (pushnew (current-thread) (task-test-provider-threads provider) :test #'eq)
      (setf request-number (task-test-provider-request-count provider)
            (task-test-provider-maximum-active-count provider)
            (max (task-test-provider-maximum-active-count provider)
                 (task-test-provider-active-count provider))))
    (unwind-protect
         (progn
           (when (eq (task-test-provider-mode provider) :concurrent)
             (sleep 0.05))
           (funcall event-callback
                    (make-instance 'assistant-delta-event :text "task test"))
           (agent-test-result
            (format nil "task-test-~D" request-number)
            (list
             (cond
               ((and (member (task-test-provider-mode provider)
                             '(:nested :nested-cancel)
                             :test #'eq)
                     (= request-number 1))
                (agent-test-call
                 :call-id "nested-task"
                 :namespace "task"
                 :name "run"
                 :arguments
                 (json-encode
                  (json-object "agent" "task"
                               "task" "Return the nested leaf result."))))
               ((or (and (eq (task-test-provider-mode provider)
                             :blocking-tool)
                         (= request-number 1))
                    (and (eq (task-test-provider-mode provider)
                             :nested-cancel)
                         (= request-number 2)))
                (agent-test-call
                 :call-id "blocking-tool"
                 :namespace "test"
                 :name "block"
                 :arguments "{}"))
               ((and (eq (task-test-provider-mode provider) :async-wait)
                     (= request-number 1))
                (agent-test-call
                 :call-id "spawn-detached-leaf"
                 :namespace "task"
                 :name "run"
                 :arguments
                 (json-encode
                  (json-object "name" "saturation-leaf"
                               "agent" "task"
                               "task" "Return the detached leaf result."
                               "async" t))))
               ((and (eq (task-test-provider-mode provider) :async-wait)
                     (= request-number 2))
                (agent-test-call
                 :call-id "wait-detached-leaf"
                 :namespace "job"
                 :name "wait"
                 :arguments
                 (json-encode
                  (json-object "id" "saturation-leaf-2"
                               "timeout-seconds" 1))))
               ((and (eq (task-test-provider-mode provider) :manifest)
                     (= request-number +task-maximum-batch-size+))
                (agent-test-call
                 :call-id (format nil "yield-~D" request-number)
                 :namespace "yield"
                 :name "submit"
                 :arguments
                 (json-encode
                  (json-object
                   "status" "failed"
                   "text" "The final manifest child failed."
                   "error" "AUTOLITH-LAST-MANIFEST-CHILD-FAILED"))))
               (t
                (agent-test-call
                 :call-id (format nil "yield-~D" request-number)
                 :namespace "yield"
                 :name "submit"
                 :arguments
                 (json-encode
                  (json-object
                   "status" "success"
                   "text"
                   (if (and (eq (task-test-provider-mode provider) :manifest)
                            (= request-number 1))
                       (make-string 100000 :initial-element #\X)
                       (format nil "result ~D" request-number))))))))))
      (with-lock-held ((task-test-provider-lock provider))
        (decf (task-test-provider-active-count provider))))))

(defmethod tool-execute
    ((tool task-test-effect-tool)
     (context tool-context)
     (arguments hash-table))
  "Increment the observable effect count for a real execution."
  (declare (ignore tool context arguments))
  (incf *task-test-effect-count*)
  (tool-success "effect executed"))

(defmethod tool-execute
    ((tool task-test-abort-tool)
     (context tool-context)
     (arguments hash-table))
  "Signal a deliberate task abort through ordinary registry dispatch."
  (declare (ignore tool context arguments))
  (error 'task-aborted
         :message "Task test was cancelled."
         :reason :test-cancel))

(-> task-tests--write-text (pathname string) pathname)
(defun task-tests--write-text (pathname contents)
  "Write CONTENTS to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string contents stream))
  pathname)

(-> task-tests--wait-until (function (real 0)) boolean)
(defun task-tests--wait-until (predicate timeout-seconds)
  "Wait up to TIMEOUT-SECONDS for PREDICATE to become true."
  (let ((deadline
          (+ (get-internal-real-time)
             (* timeout-seconds internal-time-units-per-second))))
    (loop
      when (funcall predicate)
        return t
      when (>= (get-internal-real-time) deadline)
        return nil
      do (sleep 0.001))))

(-> task-tests--write-native-form (pathname t) pathname)
(defun task-tests--write-native-form (pathname form)
  "Write exactly one readable native FORM to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (let ((*print-readably* t)
          (*print-pretty* t)
          (*print-circle* t))
      (prin1 form stream)
      (terpri stream)))
  pathname)

(-> task-tests--agent-definition-error (pathname keyword)
    task-agent-definition-error)
(defun task-tests--agent-definition-error (pathname source)
  "Parse PATHNAME and return its expected typed role diagnostic."
  (handler-case
      (progn
        (task-parse-agent-file pathname source)
        (error "Expected a task-agent-definition-error for ~A." pathname))
    (task-agent-definition-error (condition)
      condition)))

(-> task-tests--role-form (string string string &rest t) list)
(defun task-tests--role-form (name description instructions &rest properties)
  "Return a minimal native role form extended by PROPERTIES."
  (append (list :name name
                :description description
                :instructions instructions)
          properties))

(-> task-tests--yield-fixture
    (configuration task-agent-definition string)
    list)
(defun task-tests--yield-fixture (configuration definition identifier)
  "Return an isolated child, yield registry, and completion fixture."
  (let* ((orchestrator (task-orchestrator-create))
         (parent-registry (make-instance 'tool-registry))
         (parent-conversation
           (conversation-create
            configuration
            :identifier (format nil "~A-parent" identifier)))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation parent-conversation
            :tool-registry parent-registry
            :worker nil))
         (job
           (make-instance
            'task-job
            :orchestrator orchestrator
            :identity (list :id identifier :index 1)
            :execution-identifier (make-identifier)
            :definition definition
            :item (list :task "Exercise the terminal yield contract.")
            :parent-agent parent
            :root-conversation-identifier
            (conversation-identifier parent-conversation)
            :owner-identifiers nil
            :detached-p nil))
         (completion (make-instance 'task-completion))
         (registry
           (task-child-tool-registry
            parent-registry definition orchestrator 1))
         (conversation
           (conversation-create
            configuration
            :identifier (format nil "~A-child" identifier)))
         (child
           (make-instance
            'task-child-agent
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation conversation
            :tool-registry registry
            :worker nil
            :definition definition
            :identity (task-job-identity job)
            :depth 1
            :completion completion
            :orchestrator orchestrator
            :job job))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry
                          :agent child)))
    (list :registry registry
          :context context
          :completion completion
          :job job
          :child child
          :conversation conversation)))

(-> task-tests--execute-yield (list string) tool-result)
(defun task-tests--execute-yield (fixture arguments)
  "Execute provider JSON ARGUMENTS through FIXTURE's actual tool registry."
  (tool-registry-execute-call
   (getf fixture :registry)
   (json-object "namespace" "yield"
                "name" "submit"
                "arguments" arguments)
   (getf fixture :context)))

(-> task-tests--read-exact-native-value (string) t)
(defun task-tests--read-exact-native-value (source)
  "Read and return exactly one safe native value from SOURCE."
  (with-input-from-string (stream source)
    (let ((*read-eval* nil)
          (*readtable* (copy-readtable nil))
          (end (gensym "END")))
      (let ((value (read stream nil end)))
        (when (eq value end)
          (error "Expected one readable native value."))
        (unless (eq (read stream nil end) end)
          (error "Expected exactly one readable native value."))
        value))))

(-> task-tests--primary-agent
    (configuration string &optional tool-registry)
    agent)
(defun task-tests--primary-agent
    (configuration identifier &optional (registry (make-instance 'tool-registry)))
  "Return a primary test agent with conversation IDENTIFIER."
  (agent-create
   :configuration configuration
   :provider (make-instance 'model-provider)
   :conversation (conversation-create configuration :identifier identifier)
   :tool-registry registry
   :worker nil))

(-> task-tests--register-job
    (task-orchestrator agent task-agent-definition
     &key (:name (option string))
          (:owner-identifiers list)
          (:root-conversation-identifier (option string))
          (:detached-p boolean))
    task-job)
(defun task-tests--register-job
    (orchestrator parent definition
     &key name
       (owner-identifiers nil owner-identifiers-supplied-p)
       root-conversation-identifier
       (detached-p t))
  "Register one inert queued job with scheduler accounting for focused tests."
  (let ((root
          (or root-conversation-identifier
              (task-parent-root-conversation-identifier parent)))
        (owners
          (if owner-identifiers-supplied-p
              owner-identifiers
              (task-parent-owner-identifiers parent))))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (let* ((identity
               (task-orchestrator--create-identity
                orchestrator name
                (task-agent-definition-name definition)))
             (job
               (make-instance
                'task-job
                :orchestrator orchestrator
                :identity identity
                :execution-identifier (make-identifier)
                :definition definition
                :item (list :name name
                            :agent (task-agent-definition-name definition)
                            :task
                            (format nil "Hold ~A for a scheduler test."
                                    (or name "this unnamed job"))
                            :context nil
                            :async detached-p)
                :parent-agent parent
                :root-conversation-identifier root
                :owner-identifiers owners
                :parent-call-id nil
                :detached-p detached-p
                :command-authorization-function nil)))
        (setf (gethash (getf identity :id)
                       (task-orchestrator-jobs orchestrator))
              job)
        (incf (task-orchestrator-live-count orchestrator))
        job))))

(-> task-tests--child-viewer
    (configuration task-job
     &key (:depth (integer 1)) (:registry (option tool-registry)))
    task-child-agent)
(defun task-tests--child-viewer
    (configuration job &key (depth 1) registry)
  "Return a non-running child agent whose identity is JOB."
  (let ((identifier (getf (task-job-identity job) :id)))
    (make-instance
     'task-child-agent
     :configuration configuration
     :provider (make-instance 'model-provider)
     :conversation
     (conversation-create
      configuration
      :identifier (format nil "~A-viewer" identifier))
     :tool-registry (or registry (make-instance 'tool-registry))
     :worker nil
     :definition (task-job-definition job)
     :identity (task-job-identity job)
     :depth depth
     :completion (make-instance 'task-completion)
     :orchestrator (task-job-orchestrator job)
     :job job)))

(-> task-tests--terminal-result
    (task-job &key (:status keyword) (:output string)
                   (:error (option string)))
    list)
(defun task-tests--terminal-result
    (job &key (status :success) (output "done") error)
  "Return one portable terminal RESULT for JOB."
  (let ((identifier (getf (task-job-identity job) :id)))
    (list :id identifier
          :name identifier
          :agent (task-agent-definition-name (task-job-definition job))
          :agent-source (task-agent-definition-source
                         (task-job-definition job))
          :assignment (getf (task-job-item job) :task)
          :status status
          :output output
          :error error
          :yielded-p t
          :structured-output-present-p nil
          :structured-output nil
          :label nil
          :request-count 1
          :usage nil
          :duration-ms 1
          :model (configuration-model
                  (agent-configuration (task-job-parent-agent job)))
          :conversation-file nil
          :detached (task-job-detached-p job))))

(-> task-tests--job-tool-error-report
    (task-orchestrator agent &key (:operation string) (:identifier string))
    string)
(defun task-tests--job-tool-error-report
    (orchestrator viewer &key operation identifier)
  "Return the expected direct job-tool error report for VIEWER."
  (let* ((tool
           (make-instance
            'task-job-tool
            :orchestrator orchestrator
            :namespace "job"
            :name operation
            :description "Exercise nondisclosing job lookup."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration (agent-configuration viewer)
                          :worker nil
                          :conversation (agent-conversation viewer)
                          :registry (agent-tool-registry viewer)
                          :agent viewer))
         (arguments
           (if (string= operation "wait")
               (json-object "id" identifier "timeout-seconds" 0)
               (json-object "id" identifier))))
    (handler-case
        (progn
          (tool-execute tool context arguments)
          (error "Expected job.~A lookup of ~A to fail."
                 operation identifier))
      (task-error (condition)
        (princ-to-string condition)))))


;;;; -- Native Role Contract Tests --

(-> test-task-agent-native-reader () null)
(defun test-task-agent-native-reader ()
  "Test exact, safe, diagnostic-rich parsing of native role files."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (directory     (merge-pathnames "agents/" root)))
    (unwind-protect
         (progn
           (let* ((pathname
                    (merge-pathnames "native.sexp" directory))
                  (definition
                    (task-parse-agent-file
                     (task-tests--write-native-form
                      pathname
                      (task-tests--role-form
                       "native" "Native role" "Use native Lisp data."
                       :tools '("fs.read")
                       :blocking-p t))
                     :project)))
             (test-assert
              (and (string= (task-agent-definition-name definition) "native")
                   (string= (task-agent-definition-instructions definition)
                            "Use native Lisp data.")
                   (equal (task-agent-definition-tools definition)
                          '("fs.read"))
                   (task-agent-definition-blocking-p definition)
                   (eq (task-agent-definition-source definition) :project)
                   (equal (task-agent-definition-pathname definition)
                          pathname))
              "one .sexp form creates a complete native role definition"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "deterministic.sexp" directory)
                     "(:name \"deterministic\" :description \"Deterministic reader\" :instructions \"Ignore ambient reader state.\" :output (:type :number :enum (10 1.5)))"))
                  (definition
                    (let ((*read-base* 16)
                          (*read-suppress* t)
                          (*read-default-float-format* 'single-float)
                          (*package* (find-package '#:common-lisp-user)))
                      (task-parse-agent-file pathname :project)))
                  (enum
                    (getf (task-agent-definition-output definition) :enum)))
             (test-assert
              (and (= (first enum) 10)
                   (typep (second enum) 'double-float))
              "native role parsing ignores ambient package, base, suppression, and float bindings"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "block-comment.sexp" directory)
                     "(:name \"block-comment\" #| outer #| nested |# comment |# :description \"Commented role\" :instructions \"Accept standard block comments.\")"))
                  (definition
                    (task-parse-agent-file pathname :project)))
             (test-assert
              (string= (task-agent-definition-name definition)
                       "block-comment")
              "native role parsing accepts nested standard block comments"))
           (let* ((pathname
                    (merge-pathnames "deeply-nested.sexp" directory))
                  (source
                    (concatenate
                     'string
                     (make-string 129 :initial-element #\()
                     "nil"
                     (make-string 129 :initial-element #\))))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text pathname source)
                     :project)))
             (test-assert
              (search "nesting"
                      (string-downcase
                       (princ-to-string
                        (task-agent-definition-error-cause condition))))
              "native role parsing rejects source nesting beyond its hard bound"))
           (setf *task-test-reader-evaluated-p* nil)
           (let* ((pathname
                    (merge-pathnames "reader-eval.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      "(:name \"reader-eval\" :description \"Unsafe\" :instructions #.(progn (setf *task-test-reader-evaluated-p* t) \"executed\"))")
                     :project)))
             (test-assert (not *task-test-reader-evaluated-p*)
                          "the native role reader binds *READ-EVAL* to NIL")
             (test-assert
              (typep (task-agent-definition-error-line condition)
                     '(integer 1))
              "reader failures retain a one-based source line"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "fresh-readtable.sexp" directory)
                     "!"))
                  (condition
                    (let ((*readtable* (copy-readtable nil)))
                      (set-macro-character
                       #\!
                       (lambda (stream character)
                         (declare (ignore stream character))
                         (task-tests--role-form
                          "fresh-readtable" "Inherited macro"
                          "This must not be accepted."))
                       nil
                       *readtable*)
                      (task-tests--agent-definition-error
                       pathname :project))))
             (test-assert
              (search "Non-keyword symbol"
                      (princ-to-string
                      (task-agent-definition-error-cause condition)))
               "the native role reader starts from a fresh standard readtable"))
           (let ((bare-name "AUTOLITH-TASK-READER-BARE-LEAK-71D21A")
                 (qualified-name
                   "AUTOLITH-TASK-READER-QUALIFIED-LEAK-71D21A")
                 (keyword-name
                   "AUTOLITH-TASK-READER-KEYWORD-LEAK-71D21A"))
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "reader-pollution sentinels begin absent from global packages")
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "bare-symbol.sexp" directory)
               "(:name \"bare-symbol\" :description \"Bare symbol\" :instructions \"Reject and forget it.\" :tools (autolith-task-reader-bare-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "qualified-symbol.sexp" directory)
               "(:name \"qualified-symbol\" :description \"Qualified symbol\" :instructions \"Reject before interning it.\" :tools (autolith::autolith-task-reader-qualified-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "unknown-keyword.sexp" directory)
               "(:name \"unknown-keyword\" :description \"Unknown keyword\" :instructions \"Reject before interning it.\" :autolith-task-reader-keyword-leak-71d21a t)")
              :project)
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "malformed role symbols never pollute project or keyword packages"))
           (let* ((pathname
                    (merge-pathnames "utf8-bound.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      (make-string 65537 :initial-element #\é))
                     :project)))
             (test-assert
              (search "byte bound"
                      (princ-to-string
                       (task-agent-definition-error-cause condition)))
              "the native role file limit counts consumed UTF-8 bytes"))
           (dolist
               (case
                '(("extra"
                   "(:name \"extra\" :description \"Extra\" :instructions \"First\")\n(:second t)"
                   nil t)
                  ("incomplete"
                   "(:name \"incomplete\" :description \"Incomplete\" :instructions"
                   nil t)
                  ("shared"
                   "(:name \"shared\" :description #1=\"Shared\" :instructions #1#)"
                   nil nil)
                  ("circular"
                   "(:name \"circular\" :description \"Circular\" :instructions \"Reject cycles.\" :tools #1=(\"fs.read\" . #1#))"
                   nil nil)
                  ("dotted"
                   "(:name \"dotted\" :description \"Dotted\" :instructions \"Reject tails.\" . :tail)"
                   nil t)
                  ("unknown"
                   "(:name \"unknown\" :description \"Unknown\" :instructions \"Reject fields.\" :type :string)"
                   :type t)
                  ("duplicate"
                   "(:name \"duplicate\" :description \"First\" :instructions \"Reject duplicates.\" :description \"Second\")"
                   :description t)))
             (destructuring-bind
                 (name contents expected-field expected-line-p)
                 case
               (let* ((pathname
                        (merge-pathnames
                         (format nil "~A.sexp" name)
                         directory))
                      (condition
                        (task-tests--agent-definition-error
                         (task-tests--write-text pathname contents)
                         :project)))
                 (test-assert
                  (and (typep condition 'task-agent-definition-error)
                       (equal (task-agent-definition-error-pathname condition)
                              pathname)
                       (eq (task-agent-definition-error-source condition)
                           :project)
                       (string=
                        (task-agent-definition-error-definition-name condition)
                        name)
                       (task-agent-definition-error-cause condition)
                       (if expected-field
                           (eq (task-agent-definition-error-field condition)
                               expected-field)
                           t)
                       (if expected-line-p
                           (typep (task-agent-definition-error-line condition)
                                  '(integer 1))
                           t))
                   (format nil
                           "~A native role input returns complete typed diagnostic metadata"
                           name))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-agent-discovery-precedence () null)
(defun test-task-agent-discovery-precedence ()
  "Test project, user, and bundled role precedence remains fail-closed."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (configuration
           (configuration--clone configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (user-directory
           (merge-pathnames "agents/"
                            (configuration-config-root configuration))))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" project-directory)
            (task-tests--role-form
             "scout" "Project scout" "Project instructions."))
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" user-directory)
            (task-tests--role-form
             "scout" "User scout" "User instructions."))
           (task-tests--write-native-form
            (merge-pathnames "reviewer.sexp" user-directory)
            (task-tests--role-form
             "reviewer" "User reviewer" "Review as configured by the user."))
           (task-tests--write-text
            (merge-pathnames "sonic.sexp" project-directory)
            "(:name \"sonic\" :description \"Missing instructions\")")
           (task-tests--write-native-form
            (merge-pathnames "sonic.sexp" user-directory)
            (task-tests--role-form
             "sonic" "User sonic" "This lower role must stay blocked."))
           (dolist (filename '("dupe.sexp" "DUPE.sexp"))
             (task-tests--write-native-form
              (merge-pathnames filename project-directory)
              (task-tests--role-form
               "dupe" "Duplicate role" "Reject normalized duplicates.")))
           (task-tests--write-native-form
            (merge-pathnames "dupe.sexp" user-directory)
            (task-tests--role-form
             "dupe" "Lower duplicate" "This lower role must stay blocked."))
           (multiple-value-bind (definitions diagnostics)
               (task-discover-agents configuration)
             (let ((scout
                     (task-find-agent-definition definitions "scout"))
                   (reviewer
                     (task-find-agent-definition definitions "reviewer"))
                   (librarian
                     (task-find-agent-definition definitions "librarian"))
                   (sonic-diagnostic
                     (task-find-agent-diagnostic diagnostics "sonic"))
                   (dupe-diagnostic
                     (task-find-agent-diagnostic diagnostics "dupe")))
               (test-assert
                (and scout
                     (eq (task-agent-definition-source scout) :project)
                     (string= (task-agent-definition-instructions scout)
                              "Project instructions."))
                "project .sexp roles override user and bundled roles")
               (test-assert
                (and reviewer
                     (eq (task-agent-definition-source reviewer) :user))
                "user .sexp roles override bundled roles")
               (test-assert
                (and librarian
                     (eq (task-agent-definition-source librarian) :bundled))
                "unclaimed roles retain their bundled definitions")
               (test-assert
                (and (null (task-find-agent-definition definitions "sonic"))
                     sonic-diagnostic
                     (eq (task-agent-definition-error-source sonic-diagnostic)
                         :project)
                     (eq (task-agent-definition-error-field sonic-diagnostic)
                         :instructions))
                "a malformed higher-precedence role blocks only its own name")
               (test-assert
                (and (task-find-agent-definition definitions "scout")
                     (task-find-agent-definition definitions "reviewer")
                     (task-find-agent-definition definitions "librarian"))
                "one blocked role does not suppress unrelated definitions")
               (test-assert
                (and (null (task-find-agent-definition definitions "dupe"))
                     dupe-diagnostic
                     (eq (task-agent-definition-error-source dupe-diagnostic)
                         :project)
                     (eq (task-agent-definition-error-field dupe-diagnostic)
                         :name)
                     (search "same normalized role name"
                             (princ-to-string
                              (task-agent-definition-error-cause
                               dupe-diagnostic))))
                "case-normalized duplicate filenames fail closed before parsing"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-agents-tool () null)
(defun test-task-agents-tool ()
  "Test native role discovery, policy filtering, diagnostics, and secrecy."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (hidden-broken-path
           (merge-pathnames "hidden-broken.sexp" project-directory))
         (secret
           "AUTOLITH-TASK-AGENT-INSTRUCTION-SENTINEL-71D21A")
         (registry
           (task-augment-tool-registry (make-default-tool-registry))))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "allowed.sexp" project-directory)
            (task-tests--role-form
             "allowed" "An explicitly spawnable role." secret))
           (task-tests--write-native-form
            (merge-pathnames "denied.sexp" project-directory)
            (task-tests--role-form
             "denied" "A role outside the child policy."
             "This instruction must not matter."))
           (task-tests--write-text
            (merge-pathnames "blocked.sexp" project-directory)
            "(:name \"blocked\" :description \"Missing instructions\")")
           (task-tests--write-text
            hidden-broken-path
            "(:name \"hidden-broken\" :description \"Private malformed role\" :instructions 177771)")
           (let* ((primary
                    (task-tests--primary-agent
                     configuration "agents-primary" registry))
                  (tool (tool-registry-find registry "task" "agents")))
             (test-assert tool
                          "the default registry exposes task.agents")
             (let ((orchestrator (task-agents-tool-orchestrator tool)))
               (labels
                   ((invoke (selected-tool viewer offset limit)
                      "Execute task.agents and return its result and native form."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry (agent-tool-registry viewer)
                                :agent viewer))
                             (result
                               (tool-execute
                                selected-tool context
                                (json-object "offset" offset "limit" limit)))
                             (form
                               (task-tests--read-exact-native-value
                                (tool-result-content result))))
                        (values result form)))

                    (entry (form name kind)
                      "Return the native entry named NAME with KIND from FORM."
                      (find-if
                       (lambda (record)
                         (and (eq (getf record :kind) kind)
                              (string= (getf record :name) name)))
                       (getf (rest form) :entries)))

                    (field-present-p (record field)
                      "Return true when FIELD occurs as a key in RECORD."
                      (loop for tail on record by #'cddr
                            thereis (eq (first tail) field)))

                    (run-report (viewer selected-registry agent-name)
                      "Invoke task.run as VIEWER and return its failure report."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry selected-registry
                                :agent viewer))
                             (result
                               (tool-registry-execute-call
                                selected-registry
                                (json-object
                                 "namespace" "task"
                                 "name" "run"
                                 "arguments"
                                 (json-encode
                                  (json-object
                                   "agent" agent-name
                                   "task" "Exercise spawn-policy secrecy.")))
                                context)))
                        (test-assert
                         (not (tool-result-success-p result))
                         "a disallowed role request fails through registry dispatch")
                        (tool-result-content result))))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 +task-agent-page-maximum+)
                   (let ((allowed (entry form "allowed" :agent))
                         (denied (entry form "denied" :agent))
                         (blocked (entry form "blocked" :diagnostic)))
                     (test-assert
                      (and (equal form (tool-result-details result))
                           (eq (first form) :task-agents)
                           allowed denied blocked
                           (eq (getf allowed :source) :project)
                           (getf allowed :pathname)
                           (eq (getf blocked :field) :instructions))
                      "primary task.agents returns exact native role and diagnostic metadata")
                     (test-assert
                      (every
                       (lambda (field) (field-present-p allowed field))
                       '(:description :source :pathname :models
                         :reasoning-effort :tools :spawns
                         :output-contract-p :blocking-p))
                      "role discovery exposes stable policy and source fields")
                     (test-assert
                      (and (not (field-present-p allowed :instructions))
                           (null (search secret (tool-result-content result))))
                      "task.agents never exposes role instructions")))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 1)
                   (declare (ignore result))
                   (test-assert
                    (and (= (getf (rest form) :offset) 0)
                         (= (getf (rest form) :count) 1)
                         (> (getf (rest form) :total) 1)
                         (= (getf (rest form) :next-offset) 1)
                         (= (length (getf (rest form) :entries)) 1))
                    "task.agents paginates native discovery without clipping forms"))
                 (let* ((definition
                          (task-agent-definition-create
                           :name "spawn-parent"
                           :description "Permit two role names."
                           :instructions "Exercise child discovery policy."
                           :tools :all
                           :spawns '("allowed" "blocked")
                           :source :test))
                        (job
                          (task-tests--register-job
                           orchestrator primary definition
                           :name "spawn-parent"))
                        (child-registry
                          (task-child-tool-registry
                           registry definition orchestrator 1))
                        (child
                          (task-tests--child-viewer
                           configuration job :registry child-registry))
                        (child-tool
                          (tool-registry-find child-registry "task" "agents")))
                   (test-assert
                    child-tool
                    "a child allowed to delegate inherits task.agents")
                   (multiple-value-bind (result form)
                       (invoke child-tool child 0 +task-agent-page-maximum+)
                     (let ((entries (getf (rest form) :entries)))
                       (test-assert
                        (and
                         (equal
                          (sort (mapcar (lambda (entry)
                                          (getf entry :name))
                                        entries)
                                #'string<)
                          '("allowed" "blocked"))
                         (entry form "allowed" :agent)
                         (entry form "blocked" :diagnostic)
                         (null (entry form "denied" :agent))
                         (null (search secret (tool-result-content result))))
                        "child task.agents shows only spawnable roles and reserved-name diagnostics")))
                   (let ((unknown-report
                           (run-report child child-registry
                                       "unlisted-request"))
                         (malformed-report
                           (run-report child child-registry
                                       "hidden-broken"))
                         (expected
                           "task.run failed: The current agent may not spawn the requested role."))
                     (test-assert
                      (and (string= unknown-report expected)
                           (null (search "Available agents" unknown-report))
                           (null (search "allowed" unknown-report))
                           (null (search "denied" unknown-report)))
                      "disallowed unknown roles cannot enumerate discovered roles")
                     (test-assert
                      (and (string= malformed-report expected)
                           (null
                            (search (namestring hidden-broken-path)
                                    malformed-report))
                           (null (search "hidden-broken.sexp"
                                         malformed-report))
                           (null (search "177771" malformed-report))
                           (null (search "instructions" malformed-report
                                         :test #'char-equal)))
                      "disallowed malformed roles reveal neither pathname nor parse cause")))
                 (dolist
                     (case
                      (list
                       (list
                        "no-spawn"
                        (task-agent-definition-create
                         :name "no-spawn"
                         :description "Permit no descendants."
                         :instructions "Do not delegate."
                         :spawns nil
                         :source :test)
                        1)
                       (list
                        "max-depth"
                        (task-agent-definition-create
                         :name "max-depth"
                         :description "Reach the configured depth."
                         :instructions "Do not exceed the depth limit."
                         :spawns :all
                         :source :test)
                        (task-orchestrator-maximum-depth orchestrator))))
                   (destructuring-bind (name definition depth) case
                     (let* ((job
                              (task-tests--register-job
                               orchestrator primary definition :name name))
                            (child-registry
                              (task-child-tool-registry
                               registry definition orchestrator depth))
                            (child
                              (task-tests--child-viewer
                               configuration job
                               :depth depth
                               :registry child-registry)))
                       (test-assert
                        (null
                         (tool-registry-find
                          child-registry "task" "agents"))
                        (format nil
                                "~A child does not inherit task.agents"
                                name))
                       (multiple-value-bind (result form)
                           (invoke tool child 0 +task-agent-page-maximum+)
                         (declare (ignore result))
                         (test-assert
                          (and (zerop (getf (rest form) :total))
                               (null (getf (rest form) :entries)))
                          (format nil
                                  "~A child has no discoverable spawn targets"
                                  name))))))))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-tool-default-argument-types () null)
(defun test-task-tool-default-argument-types ()
  "Test that explicit JSON false and null never become omitted task defaults."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (primary
           (task-tests--primary-agent
            configuration "task-default-types" registry))
         (conversation (agent-conversation primary))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry
                          :agent primary))
         (orchestrator
           (task-run-tool-orchestrator
            (tool-registry-find registry "task" "run")))
         (definition
           (task-agent-definition-create
            :name "default-types"
            :description "Exercise explicit invalid default values."
            :instructions "Remain terminal while job.wait validates."
            :source :test))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "default-types"))
         (job-result
           (task-tests--terminal-result
            job :status :success :output "already terminal")))
    (unwind-protect
         (progn
           (task-job--publish-terminal job :completed job-result)
           (labels ((rejected-p (namespace name arguments)
                      "Return true when actual registry dispatch rejects ARGUMENTS."
                      (not
                       (tool-result-success-p
                        (tool-registry-execute-call
                         registry
                         (json-object "namespace" namespace
                                      "name" name
                                      "arguments" arguments)
                         context)))))
             (dolist
                 (case
                  '(("task" "agents" "{\"offset\":false}")
                    ("task" "agents" "{\"offset\":null}")
                    ("task" "agents" "{\"limit\":false}")
                    ("task" "agents" "{\"limit\":null}")
                    ("job" "list" "{\"offset\":false}")
                    ("job" "list" "{\"offset\":null}")
                    ("job" "list" "{\"limit\":false}")
                    ("job" "list" "{\"limit\":null}")))
               (destructuring-bind (namespace name arguments) case
                 (test-assert
                  (rejected-p namespace name arguments)
                  (format nil
                          "~A.~A rejects explicit non-integer default input ~A"
                          namespace name arguments))))
             (dolist (value '("false" "null"))
               (test-assert
                (rejected-p
                 "job" "wait"
                 (format nil
                         "{\"id\":~A,\"timeout-seconds\":~A}"
                         (json-encode (getf (task-job-identity job) :id))
                         value))
                (format nil
                        "job.wait rejects explicit timeout-seconds ~A"
                        value)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-native-output-contracts () null)
(defun test-task-native-output-contracts ()
  "Test recursive native output schemas and exact JSON boundary conversion."
  (let* ((schema
           (task-output-schema-normalize
            '(:type :object
              :properties
              (("enabled" (:type :boolean))
               ("nothing" (:type :null))
               ("items"
                (:type :array
                 :items
                 (:type :object
                  :properties
                  (("name" (:type :string))
                   ("score" (:type :number)))
                  :required ("name")
                  :additional-properties nil)
                 :min-items 1
                 :max-items 2)))
              :required ("enabled" "nothing" "items")
              :additional-properties nil)
            :source :programmatic
            :definition-name "recursive"))
         (provider-schema (task-output-schema->json schema))
         (candidate
           (task-json-decode
            "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"score\":1.5}]}")))
    (test-assert (task-output-schema-valid-p candidate schema)
                 "recursive provider JSON satisfies its native output DSL")
    (test-assert
     (and (string= (json-get provider-schema "type") "object")
          (eq (json-get provider-schema "additionalProperties") false)
          (vectorp (json-get provider-schema "required"))
          (string=
           (json-get
            (json-get
             (json-get (json-get provider-schema "properties") "items")
             "items")
            "type")
           "object"))
     "native recursive schemas convert to JSON only at the provider boundary")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":null,\"nothing\":false,\"items\":[{\"name\":\"one\"}]}")
       schema))
     "recursive validation keeps JSON false distinct from JSON null")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"extra\":1}]}")
       schema))
     "recursive validation enforces nested additional-property policy"))
  (let* ((enum-schema
           (task-output-schema-normalize
            '(:enum (nil :null t))
            :source :programmatic
            :definition-name "enum"))
         (provider-enum
           (json-get (task-output-schema->json enum-schema) "enum")))
    (test-assert
     (and (= (length provider-enum) 3)
          (eq (aref provider-enum 0) false)
          (eq (aref provider-enum 1) :null)
          (eq (aref provider-enum 2) t))
     "native NIL and :NULL become distinct JSON false and null enum values"))
  (dolist
      (case
       '(((:type :array) :items)
         ((:type :object
           :properties (("known" (:type :string)))
           :required ("missing"))
          :required)
         ((:type :object
           :properties
           (("same" (:type :string))
            ("same" (:type :integer))))
          :properties)
         ((:type :boolean :enum (nil :null)) :enum)))
    (destructuring-bind (invalid-schema expected-field) case
      (let ((condition
              (handler-case
                  (progn
                    (task-output-schema-normalize
                     invalid-schema
                     :source :programmatic
                     :definition-name "invalid-output")
                    nil)
                (task-agent-definition-error (error)
                  error))))
        (test-assert
         (and condition
              (eq (task-agent-definition-error-field condition)
                  expected-field)
              (eq (task-agent-definition-error-source condition)
                  :programmatic)
              (string=
               (task-agent-definition-error-definition-name condition)
               "invalid-output"))
         (format nil "invalid recursive output field ~S has a typed diagnostic"
                 expected-field)))))
  (let* ((provider-value
           (task-json-decode
            "{\"z\":null,\"a\":[false,true,{\"quote\":\"a\\\"b\\n\"}],\"n\":2.5}"))
         (native-value (task-json->sexp provider-value))
         (entries (rest native-value))
         (array (second (assoc "a" entries :test #'string=))))
    (test-assert
     (and (eq (first native-value) :object)
          (equal (mapcar #'first entries) '("a" "n" "z"))
          (eq (first array) :array)
          (null (second array))
          (eq (third array) t)
          (eq (second (assoc "z" entries :test #'string=)) :null))
     "provider JSON becomes sorted tagged readable s-expression data")
    (test-assert
     (equal native-value
            (task-json->sexp (task-sexp->json native-value)))
     "tagged objects, arrays, false, null, numbers, and escaped strings round trip")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json
            '(:object ("duplicate" 1) ("duplicate" 2)))
           nil)
       (task-error ()
         t))
     "tagged native objects reject duplicate keys during reconstruction")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json '("untagged" nil :null))
           nil)
       (task-error ()
         t))
     "untagged lists cannot cross the durable task result boundary")
    (test-assert
     (handler-case
         (progn
           (task-json-decode
            "{\"complete\":true} false"
            :tool-name "yield.submit")
           nil)
       (task-error (condition)
         (string= (tool-error-tool-name condition) "yield.submit")))
     "task JSON decoding rejects trailing values with canonical tool metadata"))
  nil)

(-> test-task-yield-contract () null)
(defun test-task-yield-contract ()
  "Test exact yield semantics through provider JSON argument decoding."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((definition
                    (task-agent-definition-create
                     :name "boolean-output"
                     :description "Return a boolean."
                     :instructions "Yield one explicit boolean."
                     :output '(:type :boolean)
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-false"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"false value\",\"data\":false}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :success)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) false)
                   (null
                    (task-json->sexp
                     (task-completion-data completion))))
              "registry decoding preserves an explicitly supplied JSON false")
             (let ((durable
                     (task--assemble-child-result
                      (getf fixture :job)
                      (agent-test-result "yield-false-result" nil)
                      (getf fixture :child)
                      (getf fixture :conversation)
                      completion)))
               (test-assert
                (and (getf durable :structured-output-present-p)
                     (task--plist-key-present-p durable :structured-output)
                     (null (getf durable :structured-output)))
                "durable task results tag false as NIL with an explicit presence bit"))
             (test-assert
              (not
               (tool-result-success-p
                (task-tests--execute-yield
                 fixture
                 "{\"status\":\"success\",\"data\":true}")))
              "yield.submit rejects every call after the exact terminal yield"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "null-output"
                     :description "Return null."
                     :instructions "Yield one explicit null."
                     :output '(:type :null)
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-null"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"data\":null}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) :null)
                   (eq (task-json->sexp
                        (task-completion-data completion))
                       :null))
              "registry decoding preserves JSON null separately from false"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "optional-output"
                     :description "Return optional data."
                     :instructions "Yield a concise result."
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-absent"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"no structured data\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (not (task-completion-data-present-p completion))
                   (null (task-completion-data completion)))
              "absent yield data remains distinct from explicit false"))
           (dolist
               (case
                '(("required-missing"
                   (:type :boolean)
                   "{\"status\":\"success\"}")
                  ("success-error"
                   nil
                   "{\"status\":\"success\",\"error\":\"impossible\"}")
                  ("success-empty"
                   nil
                   "{\"status\":\"success\",\"text\":\" \\t\\n \"}")
                  ("unknown-field"
                   nil
                   "{\"status\":\"success\",\"text\":\"done\",\"legacy\":true}")
                  ("failed-with-data"
                   nil
                   "{\"status\":\"failed\",\"error\":\"blocked\",\"data\":false}")
                  ("failed-empty-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\"\"}")
                  ("failed-blank-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\" \\t \"}")
                  ("aborted-no-error"
                   nil
                   "{\"status\":\"aborted\"}")
                  ("status-case"
                   nil
                   "{\"status\":\"Success\"}")
                  ("non-string-text"
                   nil
                   "{\"status\":\"success\",\"text\":null}")))
             (destructuring-bind (name output arguments) case
               (let* ((definition
                        (task-agent-definition-create
                         :name name
                         :description "Exercise one invalid terminal yield."
                         :instructions "Follow the exact yield contract."
                         :output output
                         :source :test))
                      (fixture
                        (task-tests--yield-fixture
                         configuration definition name))
                      (result
                        (task-tests--execute-yield fixture arguments)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (not
                        (task-completion-called-p
                         (getf fixture :completion))))
                  (format nil "yield contract rejects ~A without terminal mutation"
                          name)))))
           (let* ((definition
                    (task-agent-definition-create
                     :name "bounded-label"
                     :description "Exercise the terminal label bound."
                     :instructions "Yield one bounded label."
                     :source :test))
                  (oversized-fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-oversized-label"))
                  (oversized-result
                    (task-tests--execute-yield
                     oversized-fixture
                     (json-encode
                      (json-object
                       "status" "success"
                       "text" "done"
                       "label"
                       (make-string
                        (1+ +task-result-label-maximum-characters+)
                        :initial-element #\L))))))
             (test-assert
              (and (not (tool-result-success-p oversized-result))
                   (not
                    (task-completion-called-p
                     (getf oversized-fixture :completion))))
              "yield.submit rejects labels beyond the terminal retention bound")
             (let* ((fixture
                      (task-tests--yield-fixture
                       configuration definition "yield-bounded-label"))
                    (label
                      (make-string
                       +task-result-label-maximum-characters+
                       :initial-element #\L))
                    (result
                      (task-tests--execute-yield
                       fixture
                       (json-encode
                        (json-object "status" "success"
                                     "text" "done"
                                     "label" label))))
                    (durable
                      (task--assemble-child-result
                       (getf fixture :job)
                       (agent-test-result "bounded-label-result" nil)
                       (getf fixture :child)
                       (getf fixture :conversation)
                       (getf fixture :completion))))
               (test-assert
                (and (tool-result-success-p result)
                     (task-job--publish-terminal
                      (getf fixture :job) :completed durable)
                     (string=
                      (getf (task-job-result (getf fixture :job)) :label)
                      label))
                "a maximum-length yield label survives terminal compaction")))
           (let* ((definition
                    (task-agent-definition-create
                     :name "failed-result"
                     :description "Report a failure."
                     :instructions "Yield one explained failure."
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-failed"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"failed\",\"error\":\"dependency unavailable\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :failed)
                   (string= (task-completion-error completion)
                            "dependency unavailable")
                   (not (task-completion-data-present-p completion)))
              "an explained failed yield is an accepted terminal result")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> task-tests--child-registry
    (task-agent-definition task-orchestrator)
    tool-registry)
(defun task-tests--child-registry (definition orchestrator)
  "Return a child registry with authorization, yield, and trailing-effect tools."
  (let ((registry
          (task-child-tool-registry
           (make-instance 'tool-registry)
           definition
           orchestrator
           1)))
    (tool-registry-register
     registry
     (make-instance 'task-test-authorization-tool
                    :namespace "test"
                    :name "authorize"
                    :description "Authorize a harmless command."
                    :parameters (tool-object-schema (json-object) nil)))
    (tool-registry-register
     registry
     (make-instance 'task-test-effect-tool
                    :namespace "test"
                    :name "effect"
                    :description "Record an observable test effect."
                    :parameters (tool-object-schema (json-object) nil)))
    registry))

(-> test-task-abort-control-condition () null)
(defun test-task-abort-control-condition ()
  "Test that registry dispatch preserves the internal cancellation unwind."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration))
         (registry      (make-instance 'tool-registry))
         (tool
           (make-instance
            'task-test-abort-tool
            :namespace "test"
            :name "abort"
            :description "Signal a task cancellation."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry)))
    (unwind-protect
         (progn
           (tool-registry-register registry tool)
           (test-assert
            (handler-case
                (progn
                  (tool-registry-execute-call
                   registry
                   (json-object "namespace" "test"
                                "name" "abort"
                                "arguments" "{}")
                   context)
                  nil)
              (task-aborted (condition)
                (and (eq (task-aborted-reason condition) :test-cancel)
                     (string= (task-aborted-message condition)
                              "Task test was cancelled.")))
              (condition ()
                nil))
            "tool registry dispatch propagates task-aborted as control flow"))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, agent discovery, and yields."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((registry (make-default-tool-registry))
                  (initial-count (length (tool-registry-tools registry))))
             (task-augment-tool-registry registry)
             (test-assert
              (= (length (tool-registry-tools registry))
                 (+ initial-count 6))
              "task augmentation adds two task and four job tools")
             (dolist (name '("run" "agents"))
               (test-assert (tool-registry-find registry "task" name)
                            (format nil
                                    "task augmentation registers task.~A"
                                    name)))
             (dolist (name '("list" "get" "wait" "cancel"))
               (test-assert (tool-registry-find registry "job" name)
                            (format nil "task augmentation registers job.~A" name)))
             (test-assert (eq registry (task-augment-tool-registry registry))
                          "task augmentation is idempotent")
             (let* ((orchestrator
                      (task-run-tool-orchestrator
                       (tool-registry-find registry "task" "run")))
                    (definition
                      (task-find-agent-definition
                       (task-bundled-agent-definitions)
                       "task"))
                    (child-registry
                      (task-child-tool-registry
                       registry definition orchestrator 1)))
               (test-assert
                (tool-registry-find child-registry "search" "content")
                "general task children inherit native repository search")
               (test-assert
                (null (tool-registry-find child-registry "self" "status"))
                "task children never inherit active-image tools")))
           (let* ((registry (make-default-tool-registry))
                  (local-definition
                    (task-agent-definition-create
                     :name "local-grant"
                     :description "Use one available local tool."
                     :instructions "Read one file."
                     :tools '("fs.read")
                     :source :test))
                  (hosted-definition
                    (task-agent-definition-create
                     :name "hosted-grant"
                     :description "Use hosted provider search."
                     :instructions "Search one authoritative source."
                     :tools '("web_search")
                     :source :test))
                  (missing-definition
                    (task-agent-definition-create
                     :name "missing-grant"
                     :description "Request one unavailable local tool."
                     :instructions "Exercise fail-closed grant validation."
                     :tools '("missing.operation")
                     :source :test)))
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     local-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "available child-safe local grants validate against the registry")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     hosted-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "web_search remains a recognized hosted provider grant")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     missing-definition registry)
                    nil)
                (task-agent-definition-error (condition)
                  (eq (task-agent-definition-error-field condition) :tools)))
              "unavailable local tool grants fail closed with typed metadata"))
           (let* ((parent-registry (make-instance 'tool-registry))
                  (definition
                    (task-agent-definition-create
                     :name "extension-boundary"
                     :description "Exercise extension capability defaults."
                     :instructions "Use only explicitly child-safe extensions."
                     :tools :all
                     :source :test))
                  (orchestrator (task-orchestrator-create)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-default-deny-tool
                             :namespace "extension"
                             :name "denied"
                             :description "Remain unavailable to children."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-child-safe-tool
                             :namespace "extension"
                             :name "allowed"
                             :description "Opt into child availability."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (let ((child-registry
                     (task-child-tool-registry
                      parent-registry definition orchestrator 1)))
               (test-assert
                (null
                 (tool-registry-find child-registry "extension" "denied"))
                "ordinary extension tools default closed for child agents")
               (test-assert
                (tool-registry-find child-registry "extension" "allowed")
                "a class-specific child-safe method opts an extension in")))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Inspect the parser."
                                            "agent" "SCOUT"
                                            "async" t)))))
             (test-assert (string= (getf item :agent) "scout")
                          "task normalization canonicalizes agent names")
             (test-assert (getf item :async)
                          "task normalization preserves detached execution"))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Stay synchronous."
                                            "async" false)))))
             (test-assert (null (getf item :async))
                          "JSON false remains false for task async policy"))
           (let* ((registry
                    (task-augment-tool-registry
                     (make-default-tool-registry)))
                  (conversation
                    (conversation-create
                     configuration :identifier "task-null-dispatch"))
                  (parent
                    (agent-create
                     :configuration configuration
                     :provider (make-instance 'model-provider)
                     :conversation conversation
                     :tool-registry registry
                     :worker nil))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :registry registry
                                   :agent parent))
                  (tool
                    (tool-registry-find registry "task" "run"))
                  (orchestrator (task-run-tool-orchestrator tool)))
             (unwind-protect
                  (let ((result
                          (tool-registry-execute-call
                           registry
                           (json-object
                            "namespace" "task"
                            "name" "run"
                            "arguments"
                            "{\"task\":\"Reject null async.\",\"async\":null}")
                           context)))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (null
                           (task-orchestrator-list-jobs orchestrator)))
                     "registry task.run decoding rejects JSON null before job admission"))
               (tool-registry-close-runtime-state registry)))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject removed fields."
                                "isolated" false))
                  nil)
              (task-error () t))
            "task normalization rejects the removed isolated field")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject bad booleans."
                                "async" "false"))
                  nil)
              (task-error () t))
            "task normalization rejects non-boolean async values")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "tasks"
                                (json-array
                                 (json-object "task" "First")
                                 (json-object "task" "Second"))))
                  nil)
              (task-error ()
                t))
            "batch task normalization requires shared context")
           (dolist
               (case
                (list
                 (list
                  (json-object
                   "name" "forbidden-top-level-name"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level name")
                 (list
                  (json-object
                   "agent" "scout"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level agent")
                 (list
                 (json-object
                   "context" "Shared batch context."
                   "tasks"
                   (json-array
                    (json-object "task" "First" "legacy" t)))
                  "batch items reject unknown fields")
                 (list
                  (json-object
                   "context" "Shared batch context."
                   "tasks" "this string is not a task array")
                  "batch tasks reject strings despite their vector representation")))
             (test-assert
              (handler-case
                  (progn
                    (task-normalize-arguments (first case))
                    nil)
                (task-error ()
                  t))
              (second case)))
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.sexp" agent-directory))
                  (project-configuration
                    (configuration--clone configuration :working-directory root)))
             (task-tests--write-native-form
              agent-path
              (task-tests--role-form
               "scout" "Project scout" "Project instructions."))
             (let ((definition
                     (task-find-agent-definition
                      (task-discover-agents project-configuration)
                      "scout")))
               (test-assert (eq (task-agent-definition-source definition) :project)
                            "project agents override bundled definitions")
               (test-assert (string= (task-agent-definition-instructions definition)
                                     "Project instructions.")
                            "agent discovery retains native role instructions")))
           (let* ((immutable
                    (configuration--clone configuration :immutable-p t))
                  (definition
                    (task-agent-definition-create
                     :name "inheritance"
                     :description "Exercise configuration inheritance."
                     :instructions "Preserve inherited runtime configuration."
                     :tools :all
                     :models '("@parent")
                     :source ':test))
                  (child-configuration
                    (task-configuration-for-definition immutable definition)))
             (test-assert
              (and (configuration-immutable-p child-configuration)
                   (equal (configuration-config-root child-configuration)
                          (configuration-config-root immutable))
                   (equal (configuration-data-root child-configuration)
                          (configuration-data-root immutable))
                   (equal (configuration-state-root child-configuration)
                          (configuration-state-root immutable))
                   (equal (configuration-cache-root child-configuration)
                          (configuration-cache-root immutable))
                   (equal (configuration-provider-endpoint child-configuration)
                          (configuration-provider-endpoint immutable)))
              "task model selection preserves every parent runtime boundary"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "structured"
                     :description "Yield structured data."
                     :instructions "Yield data matching the native output contract."
                     :output '(:type :object
                               :properties (("answer" (:type :string)))
                               :required ("answer"))
                     :source :test))
                  (completion (make-instance 'task-completion))
                  (orchestrator (task-orchestrator-create))
                  (parent (agent-create
                           :configuration configuration
                           :provider (make-instance 'model-provider)
                           :conversation (conversation-create configuration)
                           :tool-registry (make-instance 'tool-registry)
                           :worker nil))
                  (job (make-instance 'task-job
                                      :orchestrator orchestrator
                                      :identity (list :id "yield-test" :index 1)
                                      :execution-identifier (make-identifier)
                                      :definition definition
                                      :item (list :task "Yield")
                                      :parent-agent parent
                                      :root-conversation-identifier
                                      (conversation-identifier
                                       (agent-conversation parent))
                                      :owner-identifiers nil
                                      :detached-p nil))
                  (child (make-instance 'task-child-agent
                                        :configuration configuration
                                        :provider (make-instance 'model-provider)
                                        :conversation (conversation-create configuration)
                                        :tool-registry (make-instance 'tool-registry)
                                        :worker nil
                                        :definition definition
                                        :identity (task-job-identity job)
                                        :depth 1
                                        :completion completion
                                        :orchestrator orchestrator
                                        :job job))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker nil
                                          :conversation nil
                                          :agent child))
                  (tool (make-instance 'task-yield-tool
                                       :namespace "yield"
                                       :name "submit"
                                       :description ""
                                       :parameters (json-object))))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute tool context
                                  (json-object "status" "success"
                                               "data" (json-object "wrong" "shape")))
                    nil)
                (task-yield-error ()
                  t))
              "yield validation rejects data outside the output contract")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-task-child-shared-agent-loop () null)
(defun test-task-child-shared-agent-loop ()
  "Test child yield uses the ordinary provider and tool execution path."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "runtime"
            :description "Exercise the shared child runtime."
            :instructions "Yield after checking authorization."
            :source ':test))
         (orchestrator (task-orchestrator-create))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation (conversation-create configuration)
            :tool-registry (make-instance 'tool-registry)
            :worker nil))
         (job
           (make-instance 'task-job
                          :orchestrator orchestrator
                          :identity (list :id "runtime-child" :index 1)
                          :execution-identifier (make-identifier)
                          :definition definition
                          :item (list :task "Exercise the shared loop.")
                          :parent-agent parent
                          :root-conversation-identifier
                          (conversation-identifier
                           (agent-conversation parent))
                          :owner-identifiers nil
                          :detached-p nil))
         (completion (make-instance 'task-completion))
         (conversation
           (conversation-create configuration :identifier "task-shared-loop"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "child-yield"
              (list
               (agent-test-call :call-id "authorize"
                                :namespace "test"
                                :name "authorize")
               (agent-test-call
                :call-id "yield"
                :namespace "yield"
                :name "submit"
                :arguments
                "{\"status\":\"success\",\"text\":\"done\"}")
               (agent-test-call :call-id "effect"
                                :namespace "test"
                                :name "effect"))))))
         (child
           (make-instance 'task-child-agent
                          :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry
                          (task-tests--child-registry definition orchestrator)
                          :worker nil
                          :definition definition
                          :identity (task-job-identity job)
                          :depth 1
                          :completion completion
                          :orchestrator orchestrator
                          :job job))
         (observer
           (callback-agent-observer-create
            :command-authorization-callback
            (lambda (command directory)
              (declare (ignore command directory))
              ':sandboxed))))
    (unwind-protect
         (let ((*task-test-command-decision* nil)
               (*task-test-effect-count* 0))
           (agent-run-user-turn child "Run the shared loop." :observer observer)
           (test-assert (task-completion-called-p completion)
                        "yield.submit completes a child through the shared loop")
           (test-assert (eq *task-test-command-decision* ':sandboxed)
                        "child tools receive the ordinary command authorization path")
           (test-assert (zerop *task-test-effect-count*)
                        "calls after terminal yield are not executed")
           (let* ((records (conversation--read-records
                            (conversation-pathname conversation)))
                  (results (remove-if-not
                            (lambda (record)
                              (eq (first record) :tool-result))
                            records)))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :status))
                             results)
                     '(:ok :ok :error))
              "yield retains its result and rejects every trailing call")
             (test-assert
              (and (every (lambda (record)
                            (typep (getf (rest record) :cpu-microseconds)
                                   '(integer 0)))
                          (subseq results 0 2))
                   (null (getf (rest (third results)) :cpu-microseconds)))
              "executed child calls retain timings while rejected calls omit them")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)


;;;; -- Scheduler Boundary Tests --

(-> task-tests--release-blocking-tool (task-test-blocking-tool) null)
(defun task-tests--release-blocking-tool (tool)
  "Permit TOOL to return normally when a cancellation test is unwinding."
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-released-p tool) t)
    (task--condition-broadcast
     (task-test-blocking-tool-condition-variable tool)))
  nil)

(-> test-task-running-cancellation () null)
(defun test-task-running-cancellation ()
  "Test prompt cancellation while a child executes an ordinary tool call."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait until the cancellation test releases this call."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode :blocking-tool))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "running-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "blocked-running-child"
                           "agent" "task"
                           "task" "Enter the blocking ordinary tool."
                           "async" t))
             (let ((job (first (task-orchestrator-list-jobs orchestrator))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held
                       ((task-test-blocking-tool-lock blocking-tool))
                     (task-test-blocking-tool-started-p blocking-tool)))
                 2)
                "the child reaches its ordinary tool call before cancellation")
               (let ((started-at (get-internal-real-time)))
                 (multiple-value-bind (accepted-p descendants)
                     (task-job-cancel job :user)
                   (declare (ignore descendants))
                   (multiple-value-bind (snapshot terminal-p)
                       (task-job-await job 2)
                     (test-assert
                      (and accepted-p
                           terminal-p
                           (eq (getf snapshot :state) :aborted)
                           (eq (getf (getf snapshot :result) :status)
                               :aborted)
                           (< (task--milliseconds-between
                               started-at (get-internal-real-time))
                              1000))
                      "running cancellation unwinds an ordinary tool call promptly"))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held ((task-orchestrator-lock orchestrator))
                     (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator)))))
                 2)
                "cancelled ordinary tool execution releases scheduler accounting")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist :ignore))))
  nil)

(-> test-task-nested-parent-cancellation () null)
(defun test-task-nested-parent-cancellation ()
  "Test parent cancellation while its synchronous descendant is running."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait inside a nested synchronous child."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode :nested-cancel))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "nested-parent-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "nested-cancel-parent"
                           "agent" "task"
                           "task" "Delegate synchronously, then wait."
                           "async" t))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held
                     ((task-test-blocking-tool-lock blocking-tool))
                   (task-test-blocking-tool-started-p blocking-tool)))
               2)
              "the synchronous descendant reaches its blocking tool")
             (let* ((jobs (task-orchestrator-list-jobs orchestrator))
                    (parent (first jobs))
                    (descendant (second jobs))
                    (descendant-id
                      (getf (task-job-identity descendant) :id)))
               (test-assert (= (length jobs) 2)
                            "nested cancellation observes parent and descendant")
               (multiple-value-bind (accepted-p descendants)
                   (task-job-cancel parent :user)
                 (test-assert
                  (and accepted-p
                       (equal descendants (list descendant-id)))
                  "parent cancellation reaches the live synchronous descendant"))
               (dolist (job jobs)
                 (multiple-value-bind (snapshot terminal-p)
                     (task-job-await job 2)
                   (test-assert
                    (and terminal-p
                         (eq (getf snapshot :state) :aborted)
                         (eq (getf (getf snapshot :result) :status)
                             :aborted))
                    "parent and synchronous descendant both terminalize as aborted")))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held ((task-orchestrator-lock orchestrator))
                     (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator))
                          (null (task-orchestrator-queue orchestrator)))))
                 2)
                "nested parent cancellation leaves no orphan or live-count leak")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist :ignore))))
  nil)

(-> task-tests--lock-held-by-another-p (t) boolean)
(defun task-tests--lock-held-by-another-p (lock)
  "Return true when LOCK cannot be acquired immediately by this thread."
  (if (bordeaux-threads:acquire-lock lock nil)
      (progn
        (bordeaux-threads:release-lock lock)
        nil)
      t))

(-> test-task-admission-cancellation-barrier () null)
(defun test-task-admission-cancellation-barrier ()
  "Test that nested admission and parent cancellation form one atomic boundary."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "admission-race"
            :description "Exercise admission and cancellation ordering."
            :instructions "Remain queued for the scheduler race."
            :spawns :all
            :source :test))
         (primary
           (task-tests--primary-agent configuration "admission-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-orchestrator-create))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition :name "race-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entries
                    (list
                     (list
                      :definition definition
                      :item
                      (list :name "racing-descendant"
                            :agent "admission-race"
                            :task "Be admitted before cancellation scans."
                            :context nil
                            :async t)
                      :detached t)))
                  (admitted nil)
                  (admission-condition nil)
                  (cancel-started-p nil)
                  (cancel-accepted-p nil)
                  (cancelled-descendants nil)
                  (admission-thread nil)
                  (cancel-thread nil)
                  (orchestrator-lock-held-p nil))
             (unwind-protect
                  (progn
                    (bordeaux-threads:acquire-lock
                     (task-orchestrator-lock orchestrator))
                    (setf orchestrator-lock-held-p t
                          admission-thread
                          (make-thread
                           (lambda ()
                             (handler-case
                                 (setf admitted
                                       (first
                                        (multiple-value-list
                                         (task-orchestrator-start-jobs
                                          orchestrator viewer entries
                                          nil nil))))
                               (condition (condition)
                                 (setf admission-condition condition))))
                           :name "Autolith admission race"))
                    (test-assert
                     (task-tests--wait-until
                      (lambda ()
                        (task-tests--lock-held-by-another-p
                         (task-job-lock parent)))
                      2)
                     "nested admission holds the parent lifecycle lock before commit")
                    (setf cancel-thread
                          (make-thread
                           (lambda ()
                             (setf cancel-started-p t)
                             (multiple-value-setq
                                 (cancel-accepted-p cancelled-descendants)
                               (task-job-cancel parent :user)))
                           :name "Autolith cancellation race"))
                    (test-assert
                     (task-tests--wait-until
                      (lambda () cancel-started-p)
                      2)
                     "competing cancellation reaches the parent barrier")
                    (bordeaux-threads:release-lock
                     (task-orchestrator-lock orchestrator))
                    (setf orchestrator-lock-held-p nil)
                    (join-thread admission-thread)
                    (setf admission-thread nil)
                    (join-thread cancel-thread)
                    (setf cancel-thread nil)
                    (let* ((child (first admitted))
                           (child-id
                             (and child
                                  (getf (task-job-identity child) :id))))
                      (test-assert
                       (and (null admission-condition)
                            (= (length admitted) 1)
                            cancel-accepted-p
                            (equal cancelled-descendants (list child-id))
                            (eq (task-job-state parent) :aborted)
                            (eq (task-job-state child) :aborted)
                            (member
                             (getf (task-job-identity parent) :id)
                             (task-job-owner-identifiers child)
                             :test #'string=)
                            (zerop
                             (task-orchestrator-live-count orchestrator)))
                       "admission that wins the barrier is visible to cascading cancellation")))
               (when orchestrator-lock-held-p
                 (bordeaux-threads:release-lock
                  (task-orchestrator-lock orchestrator)))
               (when admission-thread
                 (join-thread admission-thread))
               (when cancel-thread
                 (join-thread cancel-thread))))
           (let* ((orchestrator (task-orchestrator-create))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "cancel-first-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entry
                    (list
                     :definition definition
                     :item
                     (list :name "too-late"
                           :agent "admission-race"
                           :task "This child must not be admitted."
                           :context nil
                           :async t)
                     :detached t)))
             (task-job-cancel parent :user)
             (test-assert
              (handler-case
                  (progn
                    (task-orchestrator-start-jobs
                     orchestrator viewer (list entry) nil nil)
                    nil)
                (task-aborted ()
                  t))
              "admission loses atomically when parent cancellation wins")
             (test-assert
              (and (= (hash-table-count
                       (task-orchestrator-jobs orchestrator))
                      1)
                   (zerop (task-orchestrator-live-count orchestrator))
                   (= (task-orchestrator-next-index orchestrator) 1))
              "cancel-first admission consumes no identity or live capacity")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> task-tests--release-publication-barrier
    (task-test-publication-barrier)
    null)
(defun task-tests--release-publication-barrier (barrier)
  "Permit BARRIER's artifact printer to finish or signal its test failure."
  (with-lock-held ((task-test-publication-barrier-lock barrier))
    (setf (task-test-publication-barrier-released-p barrier) t)
    (task--condition-broadcast
     (task-test-publication-barrier-condition-variable barrier)))
  nil)

(-> test-task-publication-coherence () null)
(defun test-task-publication-coherence ()
  "Test coherent snapshots, forced failure, and terminal role compaction."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (secret
           "AUTOLITH-TERMINAL-ROLE-INSTRUCTION-SENTINEL-71D21A")
         (definition
           (task-agent-definition-create
            :name "publication"
            :description "Exercise terminal publication."
            :instructions secret
            :tools :all
            :spawns :all
            :models '("@task")
            :reasoning-effort :high
            :source :test))
         (primary
           (task-tests--primary-agent configuration "publication-primary")))
    (unwind-protect
         (progn
           (let* ((job
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "coherent-publication"))
                  (barrier (make-instance 'task-test-publication-barrier))
                  (result
                    (task-tests--terminal-result
                     job :status :success :output "published"))
                  (publication-result nil)
                  (publication-condition nil))
             (setf (getf result :publication-barrier) barrier
                   (getf result :portable-integer) 42)
             (let ((thread
                     (make-thread
                      (lambda ()
                        (let ((*print-base* 2)
                              (*print-radix* nil)
                              (*print-case* :downcase))
                          (handler-case
                              (setf publication-result
                                    (task-job--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition)))))
                      :name "Autolith coherent publication")))
               (unwind-protect
                    (progn
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "terminal publication reaches its post-claim artifact phase")
                      (dotimes (index 32)
                        (declare (ignore index))
                        (let ((snapshot (task-job-snapshot job)))
                          (test-assert
                           (and (eq (getf snapshot :state) :queued)
                                (null (getf snapshot :result)))
                           "a concurrent snapshot never exposes a partial terminal result")))
                      (test-assert
                       (with-lock-held ((task-job-lock job))
                         (task-job-publication-claimed-p job))
                       "snapshot sampling occurs while terminal publication is claimed"))
                 (task-tests--release-publication-barrier barrier))
               (join-thread thread))
             (let* ((snapshot (task-job-snapshot job))
                    (summary (task-job-definition-summary job))
                    (result-summary
                      (getf (getf snapshot :result) :agent-definition))
                    (artifact
                      (task-tests--read-exact-native-value
                       (uiop:read-file-string
                        (getf (getf snapshot :result) :output-path)))))
               (test-assert
                (and publication-result
                     (null publication-condition)
                     (eq (getf snapshot :state) :completed)
                     (eq (getf (getf snapshot :result) :status) :success)
                     (null (task-job-publication-claimed-p job))
                     (task-job-retained-p job)
                     (zerop (task-orchestrator-live-count orchestrator))
                     (listp artifact)
                     (= (getf artifact :portable-integer) 42))
                "publication moves atomically from a public live snapshot to terminal state")
               (test-assert
                (and (null (task-job-definition job))
                     summary
                     (equal summary result-summary)
                     (null (member :instructions summary :test #'eq))
                     (null
                      (search
                       secret
                       (task--write-readable-sexp
                        (list summary result-summary)))))
                "terminal jobs discard full role definitions and instruction text")))
           (dolist (failure '(:error :abort))
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "publication-~A" failure)))
                    (barrier
                      (make-instance
                       'task-test-publication-barrier
                       :failure failure))
                    (result
                      (task-tests--terminal-result
                       job
                       :status :success
                       :output (make-string 3000 :initial-element #\O)))
                    (publication-result nil)
                    (publication-condition nil))
               (setf (getf result :publication-barrier) barrier
                     (getf result :structured-output-present-p) t
                     (getf result :structured-output)
                     (make-string 3000 :initial-element #\S))
               (let ((thread
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf publication-result
                                    (task-job--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition))))
                        :name
                        (format nil "Autolith publication ~A" failure))))
                 (unwind-protect
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "failing publication reaches the post-claim barrier")
                   (task-tests--release-publication-barrier barrier))
                 (join-thread thread))
               (let* ((snapshot (task-job-snapshot job))
                      (terminal-result (getf snapshot :result))
                      (output-path (getf terminal-result :output-path)))
                 (test-assert
                  (and publication-result
                       (null publication-condition)
                       (eq (getf snapshot :state) :failed)
                       (eq (getf terminal-result :status) :failed)
                       (task-job-retained-p job)
                       (null (task-job-publication-claimed-p job))
                       (null (task-job-definition job))
                       (not
                        (and (null output-path)
                             (or
                              (eq (getf terminal-result :output-storage)
                                  :artifact)
                              (eq
                               (getf terminal-result
                                     :structured-output-storage)
                               :artifact)
                              (eq (getf terminal-result :error-storage)
                                  :artifact))))
                       (zerop
                        (task-orchestrator-live-count orchestrator)))
                  (format nil
                          "post-claim ~A forces one coherent terminal failure"
                          failure))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-terminal-wakeup-ordering () null)
(defun test-task-terminal-wakeup-ordering ()
  "Test that terminal waiters wake before arbitrary lifecycle listeners run."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "wakeup-order"
            :description "Exercise terminal wakeup ordering."
            :instructions "Publish one result."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "wakeup-primary"))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "wakeup-job"))
         (result
           (task-tests--terminal-result
            job :status :success :output "wake the waiter"))
         (ready-lock (make-lock "Autolith waiter readiness"))
         (ready-condition (make-condition-variable))
         (waiter-ready-p nil)
         (waiter-returned-p nil)
         (listener-lock (make-lock "Autolith blocking lifecycle listener"))
         (listener-condition (make-condition-variable))
         (listener-reached-p nil)
         (listener-released-p nil)
         (waiter nil)
         (publisher nil))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (declare (ignore payload))
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (listener-lock)
           (setf listener-reached-p t)
           (task--condition-broadcast listener-condition)
           (loop until listener-released-p
                 do (condition-wait listener-condition listener-lock))))))
    (unwind-protect
         (progn
           (setf waiter
                 (make-thread
                  (lambda ()
                    (with-lock-held ((task-job-lock job))
                      (with-lock-held (ready-lock)
                        (setf waiter-ready-p t)
                        (task--condition-broadcast ready-condition))
                      (loop until
                            (task-job--terminal-state-p
                             (task-job-state job))
                            do (condition-wait
                                (task-job-condition-variable job)
                                (task-job-lock job))))
                    (setf waiter-returned-p t))
                  :name "Autolith terminal waiter"))
           (test-assert
            (task-tests--wait-until (lambda () waiter-ready-p) 2)
            "the terminal waiter is parked before publication")
           (setf publisher
                 (make-thread
                  (lambda ()
                    (task-job--publish-terminal job :completed result))
                  :name "Autolith listener-blocked publisher"))
           (test-assert
            (task-tests--wait-until (lambda () listener-reached-p) 2)
            "terminal publication reaches the blocking lifecycle listener")
           (let ((woke-before-listener-release-p
                   (task-tests--wait-until
                    (lambda () waiter-returned-p)
                    0.5)))
             (with-lock-held (listener-lock)
               (setf listener-released-p t)
               (task--condition-broadcast listener-condition))
             (join-thread publisher)
             (setf publisher nil)
             (join-thread waiter)
             (setf waiter nil)
             (test-assert
              woke-before-listener-release-p
              "terminal publication wakes waiters before invoking listeners")))
      (with-lock-held (listener-lock)
        (setf listener-released-p t)
        (task--condition-broadcast listener-condition))
      (when publisher
        (join-thread publisher))
      (when waiter
        (with-lock-held ((task-job-lock job))
          (task--condition-broadcast (task-job-condition-variable job)))
        (join-thread waiter))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-job-visibility () null)
(defun test-task-job-visibility ()
  "Test conversation ownership, child ancestry, and opaque job lookup errors."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (missing-orchestrator (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "visibility"
            :description "Exercise task-tree visibility."
            :instructions "Inspect only descendant task jobs."
            :source :test))
         (primary-a
           (task-tests--primary-agent configuration "visibility-primary-a"))
         (primary-b
           (task-tests--primary-agent configuration "visibility-primary-b")))
    (unwind-protect
         (let* ((root-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "root-a"))
                (viewer-a
                  (task-tests--child-viewer configuration root-a))
                (descendant-a
                  (task-tests--register-job
                   orchestrator viewer-a definition :name "descendant-a"))
                (sibling-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "sibling-a"))
                (sibling-viewer
                  (task-tests--child-viewer configuration sibling-a))
                (sibling-descendant
                  (task-tests--register-job
                   orchestrator sibling-viewer definition
                   :name "sibling-descendant"))
                (foreign
                  (task-tests--register-job
                   orchestrator primary-b definition :name "foreign"))
                (root-a-id (getf (task-job-identity root-a) :id))
                (descendant-a-id
                  (getf (task-job-identity descendant-a) :id))
                (sibling-a-id (getf (task-job-identity sibling-a) :id))
                (sibling-descendant-id
                  (getf (task-job-identity sibling-descendant) :id))
                (foreign-id (getf (task-job-identity foreign) :id)))
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator primary-a))
             (list root-a-id descendant-a-id
                   sibling-a-id sibling-descendant-id))
            "a primary sees only jobs owned by its conversation")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator primary-b))
             (list foreign-id))
            "another primary cannot see the first conversation's task tree")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator viewer-a))
             (list descendant-a-id))
            "a child sees descendants but not itself, its parent, or siblings")
           (dolist (operation '("get" "wait" "cancel"))
             (let ((invisible-report
                     (task-tests--job-tool-error-report
                      orchestrator primary-a
                      :operation operation
                      :identifier foreign-id))
                   (missing-report
                     (task-tests--job-tool-error-report
                      missing-orchestrator primary-a
                      :operation operation
                      :identifier foreign-id)))
               (test-assert
                (and (string= invisible-report missing-report)
                     (search "No visible task job" invisible-report))
                (format nil
                        "job.~A does not disclose whether an invisible identifier exists"
                        operation))))
           (test-assert
            (and (eq (task-job-state foreign) :queued)
                 (null (task-job-cancellation-reason foreign)))
            "invisible get, wait, and cancel attempts cannot mutate the job"))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-job-list-pagination () null)
(defun test-task-job-list-pagination ()
  "Test content-aware native pagination at the maximum job.list page size."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (agent-name
           (concatenate 'string "a" (make-string 63 :initial-element #\z)))
         (requested-name (make-string 64 :initial-element #\n))
         (definition
           (task-agent-definition-create
            :name agent-name
            :description "Exercise the largest job listing page."
            :instructions "Remain queued for pagination."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "job-list-primary"))
         (tool
           (make-instance
            'task-job-tool
            :orchestrator orchestrator
            :namespace "job"
            :name "list"
            :description "List test jobs."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation (agent-conversation primary)
                          :registry (agent-tool-registry primary)
                          :agent primary)))
    (unwind-protect
         (progn
           (dotimes (index +task-job-page-maximum+)
             (declare (ignore index))
             (task-tests--register-job
              orchestrator primary definition :name requested-name))
           (let* ((result
                    (tool-execute
                     tool context
                     (json-object
                      "offset" 0
                      "limit" +task-job-page-maximum+)))
                  (content (tool-result-content result))
                  (form (task-tests--read-exact-native-value content))
                  (properties (rest form)))
             (test-assert
              (and (eq (first form) :job-list)
                   (= (getf properties :count)
                      +task-job-page-maximum+)
                   (= (getf properties :total)
                      +task-job-page-maximum+)
                   (null (getf properties :next-offset))
                   (<= (length content) +task-tool-content-limit+))
              "a maximum-size job.list request returns a bounded native page"))
           (let ((offset 0)
                 (identifiers nil)
                 (page-count 0))
             (loop
               (let* ((result
                        (tool-execute
                         tool context
                         (json-object
                          "offset" offset
                          "limit" 17)))
                      (content (tool-result-content result))
                      (form
                        (task-tests--read-exact-native-value content))
                      (properties (rest form))
                      (jobs (getf properties :jobs))
                      (next-offset (getf properties :next-offset)))
                 (incf page-count)
                 (test-assert
                  (and (eq (first form) :job-list)
                       (= (getf properties :offset) offset)
                       (= (getf properties :count) (length jobs))
                       (= (getf properties :total)
                          +task-job-page-maximum+)
                       (plusp (length jobs))
                       (<= (length content) +task-tool-content-limit+))
                  "each maximum-size job.list request returns one bounded native page")
                 (setf identifiers
                       (nconc identifiers
                              (mapcar (lambda (job) (getf job :id)) jobs)))
                 (if next-offset
                     (setf offset next-offset)
                     (return))))
             (test-assert
              (and (> page-count 1)
                   (= (length identifiers) +task-job-page-maximum+)
                   (= (length
                       (remove-duplicates identifiers :test #'string=))
                      +task-job-page-maximum+))
              "job.list pagination returns every oversized summary exactly once")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-refresh-after-delayed-close () null)
(defun test-task-refresh-after-delayed-close ()
  "Test reopening after a timed-out close's final worker exits later."
  (let* ((orchestrator (task-orchestrator-create))
         (barrier-lock (make-lock "Autolith delayed close test"))
         (barrier (make-condition-variable))
         (started-p nil)
         (released-p nil)
         (thread
           (make-thread
            (lambda ()
              (with-lock-held (barrier-lock)
                (setf started-p t)
                (task--condition-broadcast barrier)
                (loop until released-p
                      do (condition-wait barrier barrier-lock))))
            :name "Autolith delayed closing worker")))
    (unwind-protect
         (progn
           (test-assert
            (task-tests--wait-until (lambda () started-p) 2)
            "the delayed closing worker reaches its barrier")
           (with-lock-held ((task-orchestrator-lock orchestrator))
             (setf (task-orchestrator-worker-threads orchestrator)
                   (list thread)
                   (task-orchestrator-lifecycle-state orchestrator) :closing
                   (task-orchestrator-close-owner orchestrator) nil
                   (task-orchestrator-shutdown-p orchestrator) t
                   (task-orchestrator-active-count orchestrator) 1))
           (test-assert
            (handler-case
                (progn
                  (task-orchestrator-refresh orchestrator)
                  nil)
              (task-error ()
                t))
            "refresh refuses a timed-out close while its delayed worker is live")
           (with-lock-held (barrier-lock)
             (setf released-p t)
             (task--condition-broadcast barrier))
           (join-thread thread)
           (task-orchestrator-refresh orchestrator)
           (test-assert
            (with-lock-held ((task-orchestrator-lock orchestrator))
              (and (eq (task-orchestrator-lifecycle-state orchestrator)
                       :open)
                   (null (task-orchestrator-close-owner orchestrator))
                   (not (task-orchestrator-shutdown-p orchestrator))
                   (zerop (task-orchestrator-active-count orchestrator))
                   (plusp
                    (length
                     (task-orchestrator-worker-threads orchestrator)))
                   (every
                    #'thread-alive-p
                    (task-orchestrator-worker-threads orchestrator))))
            "refresh reaps the delayed death and reopens a fresh worker pool"))
      (with-lock-held (barrier-lock)
        (setf released-p t)
        (task--condition-broadcast barrier))
      (when (thread-alive-p thread)
        (join-thread thread))
      (ignore-errors (task-orchestrator-close orchestrator))))
  nil)

(-> test-task-terminal-cancellation-and-publication () null)
(defun test-task-terminal-cancellation-and-publication ()
  "Test terminal-parent cascades and exactly-once terminal publication."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "lifecycle"
            :description "Exercise terminal lifecycle invariants."
            :instructions "Publish one terminal result."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "lifecycle-primary"))
         (events nil)
         (event-lock (make-lock "Autolith task lifecycle event test")))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (event-lock)
           (push (copy-tree payload) events)))))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name "terminal-parent"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status :success :output "parent complete"))
                (parent-id (getf (task-job-identity parent) :id))
                (viewer
                  (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (descendant-id
                  (getf (task-job-identity descendant) :id))
                (unrelated
                  (task-tests--register-job
                   orchestrator primary definition :name "unrelated")))
           (test-assert
            (task-job--publish-terminal parent :completed parent-result)
            "the parent publishes its first terminal result")
           (multiple-value-bind (parent-accepted-p descendants)
               (task-job-cancel parent :terminate)
             (test-assert
              (and (null parent-accepted-p)
                   (equal descendants (list descendant-id)))
              "cancelling a terminal parent still cancels its live descendants"))
           (test-assert
            (and (eq (task-job-state parent) :completed)
                 (eq (task-job-state descendant) :aborted)
                 (eq (task-job-state unrelated) :queued)
                 (null (task-job-cancellation-reason unrelated)))
            "terminal-parent cancellation preserves parent and unrelated states")
           (let ((event-count (length events))
                 (terminal-count
                   (length (task-orchestrator-terminal-identifiers
                            orchestrator))))
             (multiple-value-bind (parent-accepted-p descendants)
                 (task-job-cancel parent :terminate)
               (test-assert
                (and (null parent-accepted-p)
                     (null descendants)
                     (= (length events) event-count)
                     (= (length
                         (task-orchestrator-terminal-identifiers orchestrator))
                        terminal-count))
                "duplicate cancellation publishes no second result or event")))
           (dolist (job (list parent descendant))
             (let* ((result (task-job-result job))
                    (pathname (getf result :output-path))
                    (artifact
                      (and pathname
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname)))))
               (test-assert
                (and pathname
                     (probe-file pathname)
                     (listp artifact)
                     (eq (getf artifact :status)
                         (getf result :status)))
                "each cancellation result artifact is exactly one readable s-expression")))
           (let* ((race-job
                    (task-tests--register-job
                     orchestrator primary definition :name "publication-race"))
                  (result
                    (task-tests--terminal-result
                     race-job :status :success :output "race winner"))
                  (barrier-lock
                    (make-lock "Autolith terminal publication barrier"))
                  (barrier (make-condition-variable))
                  (ready 0)
                  (released-p nil)
                  (claims nil)
                  (claim-lock
                    (make-lock "Autolith terminal publication claims")))
             (labels ((publish ()
                        (with-lock-held (barrier-lock)
                          (incf ready)
                          (task--condition-broadcast barrier)
                          (loop until released-p
                                do (condition-wait barrier barrier-lock)))
                        (let ((claimed-p
                                (task-job--publish-terminal
                                 race-job :completed result)))
                          (with-lock-held (claim-lock)
                            (push claimed-p claims)))))
               (let ((first-thread
                       (make-thread #'publish
                                    :name "Autolith publication race one"))
                     (second-thread
                       (make-thread #'publish
                                    :name "Autolith publication race two")))
                 (with-lock-held (barrier-lock)
                   (loop until (= ready 2)
                         do (condition-wait barrier barrier-lock))
                   (setf released-p t)
                   (task--condition-broadcast barrier))
                 (join-thread first-thread)
                 (join-thread second-thread)))
             (let* ((race-id (getf (task-job-identity race-job) :id))
                    (terminal-occurrences
                      (count race-id
                             (task-orchestrator-terminal-identifiers
                              orchestrator)
                             :test #'string=))
                    (event-occurrences
                      (count race-id events
                             :test #'string=
                             :key (lambda (event) (getf event :id)))))
               (test-assert
                (and (= (count t claims) 1)
                     (= (count nil claims) 1)
                     (= terminal-occurrences 1)
                     (= event-occurrences 1)
                     (probe-file
                      (getf (task-job-result race-job) :output-path)))
                "concurrent duplicate publication claims one artifact and event"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-retention-and-admission () null)
(defun test-task-retention-and-admission ()
  "Test exact terminal retention and atomic live-job admission limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "capacity"
            :description "Exercise scheduler capacity."
            :instructions "Remain inert until the test changes state."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "capacity-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-orchestrator-create))
                  (live
                    (task-tests--register-job
                     orchestrator primary definition :name "live-sentinel"))
                  (identifiers nil))
             (dotimes (index (1+ +task-terminal-retention-limit+))
               (let* ((job
                        (task-tests--register-job
                         orchestrator primary definition
                         :name (format nil "retained-~2,'0D" index)))
                      (identifier (getf (task-job-identity job) :id))
                      (result
                        (task-tests--terminal-result
                         job
                         :status :success
                         :output (format nil "terminal result ~D" index))))
                 (push identifier identifiers)
                 (test-assert
                  (task-job--publish-terminal job :completed result)
                  "each retained job publishes exactly once")))
             (setf identifiers (nreverse identifiers))
             (let ((first-id (first identifiers))
                   (last-id (car (last identifiers)))
                   (live-id (getf (task-job-identity live) :id)))
               (test-assert
                (and (= (length
                         (task-orchestrator-terminal-identifiers orchestrator))
                        +task-terminal-retention-limit+)
                     (= (hash-table-count
                         (task-orchestrator-jobs orchestrator))
                        (1+ +task-terminal-retention-limit+))
                     (= (hash-table-count
                         (task-orchestrator-names orchestrator))
                        (1+ +task-terminal-retention-limit+))
                     (= (task-orchestrator-live-count orchestrator) 1)
                     (null
                      (gethash first-id
                               (task-orchestrator-jobs orchestrator)))
                     (null
                      (gethash first-id
                               (task-orchestrator-names orchestrator)))
                     (gethash last-id
                              (task-orchestrator-jobs orchestrator))
                     (eq (gethash live-id
                                  (task-orchestrator-jobs orchestrator))
                         live))
                "retention keeps exactly 64 newest terminals without evicting live jobs")
               (test-assert
                (every
                 (lambda (identifier)
                   (let* ((job
                            (gethash identifier
                                     (task-orchestrator-jobs orchestrator)))
                          (pathname
                            (and job
                                 (getf (task-job-result job) :output-path))))
                     (and job
                          (task-job-terminal-p job)
                          pathname
                          (listp
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname))))))
                 (task-orchestrator-terminal-identifiers orchestrator))
                "every retained identifier maps to one terminal job and readable artifact")))
           (let* ((orchestrator (task-orchestrator-create))
                  (entries
                    (lambda (offset count)
                      (loop for index from offset below (+ offset count)
                            collect
                            (list
                             :definition definition
                             :item
                             (list :name (format nil "live-~2,'0D" index)
                                   :agent "capacity"
                                   :task "Remain queued."
                                   :context nil
                                   :async t)
                             :detached t)))))
             (dotimes (batch 4)
               (task-orchestrator-start-jobs
                orchestrator primary
                (funcall entries (* batch 16) 16)
                nil nil))
             (let ((next-index (task-orchestrator-next-index orchestrator))
                   (job-count
                     (hash-table-count
                      (task-orchestrator-jobs orchestrator)))
                   (name-count
                     (hash-table-count
                      (task-orchestrator-names orchestrator)))
                   (queue-count (length (task-orchestrator-queue orchestrator)))
                   (live-count (task-orchestrator-live-count orchestrator)))
               (test-assert
                (= live-count +task-maximum-live-jobs+)
                "the scheduler admits exactly 64 simultaneous live jobs")
               (test-assert
                (handler-case
                    (progn
                      (task-orchestrator-start-jobs
                       orchestrator primary
                       (funcall entries 64 1)
                       nil nil)
                      nil)
                  (task-error ()
                    t))
                "admission beyond 64 live jobs fails")
               (test-assert
                (and (= (task-orchestrator-next-index orchestrator)
                        next-index)
                     (= (hash-table-count
                         (task-orchestrator-jobs orchestrator))
                        job-count)
                     (= (hash-table-count
                         (task-orchestrator-names orchestrator))
                        name-count)
                     (= (length (task-orchestrator-queue orchestrator))
                        queue-count)
                     (= (task-orchestrator-live-count orchestrator)
                        live-count))
                "failed live admission consumes no identity or scheduler state"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-evicted-identity-retention () null)
(defun test-task-evicted-identity-retention ()
  "Test that evicted generated identities remain unique across live ancestry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "identity-retention"
            :description "Exercise retained task ancestry."
            :instructions "Keep descendant ownership unambiguous."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "identity-primary")))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name nil))
                (parent-id (getf (task-job-identity parent) :id))
                (viewer (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status :success :output "parent terminal")))
           (task-job--publish-terminal parent :completed parent-result)
           (dotimes (index +task-terminal-retention-limit+)
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "identity-filler-~D" index)))
                    (result
                      (task-tests--terminal-result
                       job :status :success :output "filler terminal")))
               (task-job--publish-terminal job :completed result)))
           (let* ((replacement
                    (task-tests--register-job
                     orchestrator primary definition :name nil))
                  (replacement-id
                    (getf (task-job-identity replacement) :id)))
             (test-assert
              (and (null
                    (gethash parent-id
                             (task-orchestrator-jobs orchestrator)))
                   (null
                    (gethash parent-id
                             (task-orchestrator-names orchestrator)))
                   (member parent-id
                           (task-job-owner-identifiers descendant)
                           :test #'string=)
                   (not (string= replacement-id parent-id))
                   (> (getf (task-job-identity replacement) :index)
                      (getf (task-job-identity parent) :index))
                   (= (task-orchestrator-live-count orchestrator) 2))
              "eviction never permits a generated ancestor identity to be reused")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> task-tests--run-scheduler-case (task-test-provider json-object) list)
(defun task-tests--run-scheduler-case (provider arguments)
  "Execute one real task tool case and return observations after clean shutdown."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (task-augment-tool-registry
                    (make-default-tool-registry)))
         (conversation (conversation-create configuration))
         (parent
          (agent-create :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry registry
                        :worker nil))
         (tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator tool))
         (context
          (make-instance 'tool-context
                         :configuration configuration
                         :worker nil
                         :conversation conversation
                         :registry registry
                         :agent parent
                         :call-id "task-scheduler-test"))
         (started-at (get-internal-real-time)))
    (unwind-protect
         (let* ((result (tool-execute tool context arguments))
                (idle-p
                  (task-tests--wait-until
                   (lambda ()
                     (with-lock-held ((task-orchestrator-lock orchestrator))
                       (zerop
                        (task-orchestrator-active-count orchestrator))))
                   1))
                (jobs (task-orchestrator-list-jobs orchestrator))
                (details (tool-result-details result))
                (content (tool-result-content result))
                (native-content
                  (task-tests--read-exact-native-value content))
                (artifact-forms
                  (mapcar
                   (lambda (job)
                     (task-tests--read-exact-native-value
                      (uiop:read-file-string
                       (getf (task-job-result job) :output-path))))
                   jobs))
                (workers
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (copy-list
                    (task-orchestrator-worker-threads orchestrator)))))
           (list :success-p (tool-result-success-p result)
                 :content content
                 :native-content native-content
                 :details details
                 :artifact-forms artifact-forms
                 :job-count (length jobs)
                 :all-terminal-p (every #'task-job-terminal-p jobs)
                 :heavy-references-cleared-p
                 (every (lambda (job)
                          (and (null (task-job-parent-agent job))
                               (null
                                (task-job-command-authorization-function job))
                               (null (task-job-thread job))))
                        jobs)
                 :worker-count (length workers)
                 :workers-alive-p (every #'thread-alive-p workers)
                 :provider-worker-count
                 (length (task-test-provider-threads provider))
                 :provider-maximum-active
                 (task-test-provider-maximum-active-count provider)
                 :provider-request-count
                 (task-test-provider-request-count provider)
                 :scheduler-idle-p idle-p
                 :active-count
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (task-orchestrator-active-count orchestrator))
                 :live-count
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (task-orchestrator-live-count orchestrator))
                 :artifacts-exist-p
                 (every (lambda (job)
                          (let ((path (getf (task-job-result job)
                                           :output-path)))
                            (and path (probe-file path))))
                        jobs)
                 :private-transcripts-p
                 (every (lambda (job)
                          (let ((path (getf (task-job-result job)
                                           :conversation-file)))
                            (and path (search "/tasks/" path))))
                        jobs)
                 :public-conversation-count
                 (length (conversation-list configuration))
                 :duration-ms
                 (task--milliseconds-between started-at
                                             (get-internal-real-time))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore))))

(-> test-task-run-native-manifest () null)
(defun test-task-run-native-manifest ()
  "Test fair bounded native manifests for the largest synchronous batch."
  (let ((previous-concurrency
          (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "5000" 1)
           (let* ((provider
                    (make-instance 'task-test-provider :mode :manifest))
                  (tasks
                    (coerce
                     (loop for index from 1 to +task-maximum-batch-size+
                           collect
                           (json-object
                            "name" (format nil "manifest-~2,'0D" index)
                            "agent" "task"
                            "task"
                            (format nil
                                    "Return manifest result ~D."
                                    index)))
                     'vector))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "context" "Exercise fair native aggregation."
                      "tasks" tasks)))
                  (content (getf observation :content))
                  (form (getf observation :native-content))
                  (results (getf (rest form) :results))
                  (artifacts (getf observation :artifact-forms))
                  (first-result (first results))
                  (last-result (car (last results)))
                  (last-result-value (getf last-result :result)))
             (test-assert
              (and (not (getf observation :success-p))
                   (equal form (getf observation :details))
                   (eq (first form) :task-run)
                   (null (getf (rest form) :succeeded-p))
                   (<= (length content) +task-tool-content-limit+))
              "task.run returns one exact bounded native manifest on partial failure")
             (test-assert
              (and (= (length results) +task-maximum-batch-size+)
                   (= (length artifacts) +task-maximum-batch-size+)
                   (= (length
                       (remove-duplicates
                        (mapcar (lambda (result) (getf result :id)) results)
                        :test #'string=))
                      +task-maximum-batch-size+)
                   (every
                    (lambda (result)
                      (let ((artifact (getf (getf result :result) :artifact)))
                        (and (non-empty-string-p (getf result :id))
                             (getf result :execution-id)
                             (member (getf result :state)
                                     '(:completed :failed :aborted)
                                     :test #'eq)
                             (stringp (getf artifact :path))
                             (eq (getf artifact :format) :sexp)
                             (getf artifact :available-p))))
                    results))
              "the manifest retains every child identity, state, and artifact descriptor")
             (test-assert
              (and
               (equal (getf first-result :id) "manifest-01-1")
               (equal (getf last-result :id) "manifest-16-16")
               (eq (getf first-result :state) :completed)
               (eq (getf last-result :state) :failed)
               (eq (getf last-result-value :status) :failed)
               (string=
                (getf last-result-value :error)
                "AUTOLITH-LAST-MANIFEST-CHILD-FAILED"))
              "a huge first result cannot hide the final failed child")
             (test-assert
              (and (> (length (getf (first artifacts) :output)) 90000)
                   (string=
                    (getf (car (last artifacts)) :error)
                    "AUTOLITH-LAST-MANIFEST-CHILD-FAILED")
                   (every #'listp artifacts))
              "every child artifact remains exactly one readable native result")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS"
                          previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-scheduler () null)
(defun test-task-scheduler ()
  "Test bounded reusable workers, private artifacts, and nested help-join."
  (let ((previous-concurrency (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "2" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000" 1)
           (let* ((provider (make-instance 'task-test-provider
                                           :mode :concurrent))
                  (tasks
                   (coerce
                    (loop for index from 1 to 4
                          collect (json-object
                                   "agent" "task"
                                   "task" (format nil "Return result ~D." index)))
                    'vector))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "context" "Independent scheduler checks."
                                 "tasks" tasks))))
             (test-assert (getf observation :success-p)
                          "a concurrent task batch succeeds")
             (test-assert (= (getf observation :job-count) 4)
                          "the scheduler retains every admitted job")
             (test-assert (getf observation :all-terminal-p)
                          "synchronous scheduler jobs are terminal on return")
             (test-assert (getf observation :heavy-references-cleared-p)
                          "terminal jobs release live agent capabilities")
             (test-assert (= (getf observation :worker-count) 2)
                          "the configured pool contains only two workers")
             (test-assert (getf observation :workers-alive-p)
                          "reusable workers remain live until registry shutdown")
             (test-assert (= (getf observation :provider-worker-count) 2)
                          "four children reuse the bounded worker pair")
             (test-assert (= (getf observation :provider-maximum-active) 2)
                          "the pool executes up to its configured concurrency")
             (test-assert (getf observation :artifacts-exist-p)
                          "every terminal child publishes one unique artifact")
             (test-assert (getf observation :private-transcripts-p)
                          "child transcripts live in the private task tree")
             (test-assert (zerop (getf observation
                                       :public-conversation-count))
                          "private child transcripts stay out of conversation lists"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (let* ((provider (make-instance 'task-test-provider :mode :nested))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "agent" "task"
                                 "task" "Delegate once, then return."))))
             (test-assert (getf observation :success-p)
                          "a nested synchronous task succeeds at concurrency one")
             (test-assert (= (getf observation :job-count) 2)
                          "nested execution retains parent and leaf jobs")
             (test-assert (= (getf observation :provider-request-count) 3)
                          "nested help-join resumes the parent after the leaf")
             (test-assert (= (getf observation :provider-worker-count) 1)
                          "nested synchronous work reuses its parent's worker")
             (test-assert (< (getf observation :duration-ms) 1000)
                          "nested help-join avoids a concurrency-one deadlock"))
           (let* ((provider
                    (make-instance 'task-test-provider :mode :async-wait))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "agent" "task"
                      "task"
                      "Spawn one detached task, wait for it, then return.")))
                  (artifacts (getf observation :artifact-forms)))
             (test-assert
              (and (getf observation :success-p)
                   (= (getf observation :job-count) 2)
                   (getf observation :all-terminal-p)
                   (= (getf observation :provider-request-count) 4)
                   (= (getf observation :provider-worker-count) 1)
                   (getf observation :scheduler-idle-p)
                   (zerop (getf observation :active-count))
                   (zerop (getf observation :live-count))
                   (every (lambda (artifact)
                            (eq (getf artifact :status) :success))
                          artifacts)
                   (< (getf observation :duration-ms) 1000))
              "a child can await detached work at concurrency one without starvation"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "999" 1)
           (let ((orchestrator (task-orchestrator-create)))
             (test-assert
              (= (task-orchestrator-maximum-concurrency orchestrator)
                 +task-maximum-concurrency+)
              "environment concurrency cannot exceed the hard pool cap")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)
