(in-package #:frob)

;;;; -- Agent Events --

(define-constant +default-maximum-provider-steps+ 64
  :documentation "The final, tools-disabled provider step available to one user turn.")

(define-constant +default-provider-step-warning+ 48
  :documentation "The provider step at which Frob starts reminding the model to finish.")

(define-constant +default-maximum-tool-calls+ 256
  :documentation "The maximum number of individual tool calls executed in one user turn.")

(defclass agent-observer ()
  ()
  (:documentation "A presentation sink for incremental agent output and lifecycle status."))

(defclass callback-agent-observer (agent-observer)
  ((text-callback
    :initarg :text-callback
    :initform nil
    :reader callback-agent-observer-text-callback
    :type (option function)
    :documentation "The optional function called with each assistant text delta.")
   (reasoning-callback
    :initarg :reasoning-callback
    :initform nil
    :reader callback-agent-observer-reasoning-callback
    :type (option function)
    :documentation "The optional function called with each visible reasoning delta.")
   (status-callback
    :initarg :status-callback
    :initform nil
    :reader callback-agent-observer-status-callback
    :type (option function)
    :documentation "The optional function called with a status keyword and portable details."))
  (:documentation "An agent observer implemented by ordinary terminal-facing callbacks."))

(defclass agent ()
  ((configuration
    :initarg :configuration
    :reader agent-configuration
    :type configuration
    :documentation "The paths and model choices governing this agent.")
   (provider
    :initarg :provider
    :reader agent-provider
    :type model-provider
    :documentation "The replaceable streaming model provider.")
   (conversation
    :initarg :conversation
    :reader agent-conversation
    :type conversation
    :documentation "The durable conversation owned by this agent.")
   (tool-registry
    :initarg :tool-registry
    :reader agent-tool-registry
    :type tool-registry
    :documentation "The namespaced tool schemas and dispatch table.")
   (worker
    :initarg :worker
    :reader agent-worker
    :type t
    :documentation "The disposable Lisp worker supplied to lisp.* calls.")
   (maximum-provider-steps
    :initarg :maximum-provider-steps
    :reader agent-maximum-provider-steps
    :type (integer 1)
    :documentation "The final tools-disabled provider step for one user message.")
   (provider-step-warning
    :initarg :provider-step-warning
    :reader agent-provider-step-warning
    :type (option (integer 1))
    :documentation "The step that starts model-visible budget reminders, or NIL.")
   (maximum-tool-calls
    :initarg :maximum-tool-calls
    :reader agent-maximum-tool-calls
    :type (integer 1)
    :documentation "The maximum individual tool calls executed for one user message.")
   (turn-lock
    :initform (make-lock "Frob agent turn")
    :reader agent-turn-lock
    :documentation "The lock preventing concurrent mutation of conversation turn state."))
  (:documentation "A model-driven conversation loop with namespaced Common Lisp tools."))


;;;; -- Agent Conditions --

(define-condition agent-loop-error (frob-error)
  ((conversation-id
    :initarg :conversation-id
    :reader agent-loop-error-conversation-id
    :type (option string)
    :documentation "The conversation whose turn could not continue.")
   (request-number
    :initarg :request-number
    :reader agent-loop-error-request-number
    :type (option integer)
    :documentation "The provider request number within the user turn, if known."))
  (:documentation "A malformed response or invariant violation in the main agent loop."))

(define-condition agent-turn-budget-exhausted (frob-error)
  ((maximum-provider-steps
    :initarg :maximum-provider-steps
    :reader agent-turn-budget-exhausted-maximum-provider-steps
    :type (integer 1)
    :documentation "The configured final provider step for this turn.")
   (provider-step
    :initarg :provider-step
    :reader agent-turn-budget-exhausted-provider-step
    :type (integer 1)
    :documentation "The provider step on which the turn stopped.")
   (tool-calls
    :initarg :tool-calls
    :reader agent-turn-budget-exhausted-tool-calls
    :type (integer 0)
    :documentation "The individual tool calls executed before exhaustion.")
   (reason
    :initarg :reason
    :reader agent-turn-budget-exhausted-reason
    :type keyword
    :documentation "The deterministic budget rule that stopped the turn."))
  (:documentation "A long turn reached a configured spending guard without corrupting Frob."))


;;;; -- Observer Protocol --

(-> agent-observer-text (agent-observer string) null)
(defgeneric agent-observer-text (observer text)
  (:documentation "Present one incremental assistant TEXT fragment through OBSERVER."))

(-> agent-observer-reasoning (agent-observer string) null)
(defgeneric agent-observer-reasoning (observer text)
  (:documentation "Present one visible reasoning TEXT fragment through OBSERVER."))

(-> agent-observer-status (agent-observer keyword list) null)
(defgeneric agent-observer-status (observer status details)
  (:documentation "Present STATUS and portable DETAILS through OBSERVER."))

(defmethod agent-observer-text ((observer agent-observer) (text string))
  "Ignore assistant TEXT for the default silent OBSERVER."
  (declare (ignore observer text))
  nil)

(defmethod agent-observer-reasoning ((observer agent-observer) (text string))
  "Ignore reasoning TEXT for the default silent OBSERVER."
  (declare (ignore observer text))
  nil)

(defmethod agent-observer-status
    ((observer agent-observer) status details)
  "Ignore STATUS and DETAILS for the default silent OBSERVER."
  (declare (type keyword status)
           (type list details))
  (declare (ignore observer status details))
  nil)

(defmethod agent-observer-text ((observer callback-agent-observer) (text string))
  "Send assistant TEXT to OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-text-callback observer)))
    (when callback
      (funcall callback text)))
  nil)

(defmethod agent-observer-reasoning
    ((observer callback-agent-observer) (text string))
  "Send reasoning TEXT to OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-reasoning-callback observer)))
    (when callback
      (funcall callback text)))
  nil)

(defmethod agent-observer-status
    ((observer callback-agent-observer) status details)
  "Send STATUS and DETAILS to OBSERVER's configured callback."
  (declare (type keyword status)
           (type list details))
  (let ((callback (callback-agent-observer-status-callback observer)))
    (when callback
      (funcall callback status details)))
  nil)


;;;; -- Construction and Turn Entry --

(-> callback-agent-observer-create
    (&key
     (:text-callback (option function))
     (:reasoning-callback (option function))
     (:status-callback (option function)))
    callback-agent-observer)
(defun callback-agent-observer-create
    (&key text-callback reasoning-callback status-callback)
  "Create an observer backed by optional presentation callbacks."
  (make-instance 'callback-agent-observer
                 :text-callback text-callback
                 :reasoning-callback reasoning-callback
                 :status-callback status-callback))

(-> agent-create
    (&key
     (:configuration configuration)
     (:provider (option model-provider))
     (:conversation (option conversation))
     (:tool-registry (option tool-registry))
     (:worker t)
     (:maximum-provider-steps integer)
     (:provider-step-warning (option integer))
     (:maximum-tool-calls integer))
    agent)
(defun agent-create
    (&key
       configuration
       provider
       conversation
       tool-registry
       worker
       (maximum-provider-steps +default-maximum-provider-steps+)
       (provider-step-warning +default-provider-step-warning+)
       (maximum-tool-calls +default-maximum-tool-calls+))
  "Create an agent, filling unspecified provider, conversation, registry, and worker roles."
  (unless (typep configuration 'configuration)
    (error 'configuration-error
           :message "AGENT-CREATE requires a CONFIGURATION instance."))
  (unless (typep maximum-provider-steps '(integer 1))
    (error 'configuration-error
           :message "The maximum provider steps must be a positive integer."))
  (unless (or (null provider-step-warning)
              (and (typep provider-step-warning '(integer 1))
                   (< provider-step-warning maximum-provider-steps)))
    (error 'configuration-error
           :message "The provider step warning must precede the final step."))
  (unless (typep maximum-tool-calls '(integer 1))
    (error 'configuration-error
           :message "The maximum tool calls must be a positive integer."))
  (make-instance 'agent
                 :configuration configuration
                 :provider (or provider (provider-create configuration))
                 :conversation (or conversation
                                   (conversation-create configuration))
                 :tool-registry (or tool-registry
                                    (make-default-tool-registry))
                 :worker (or worker (lisp-worker-create configuration))
                 :maximum-provider-steps maximum-provider-steps
                 :provider-step-warning provider-step-warning
                 :maximum-tool-calls maximum-tool-calls))

(-> agent-run-user-turn
    (agent string &key (:observer agent-observer)
           (:goal-context (option string)))
    provider-result)
(defgeneric agent-run-user-turn (agent content &key observer goal-context)
  (:documentation
   "Persist user CONTENT, run all bounded model and tool rounds, and return the final provider result."))

(defmethod agent-run-user-turn
    ((agent agent) (content string)
     &key (observer (make-instance 'agent-observer)) goal-context)
  "Run one serialized user turn through AGENT while presenting events to OBSERVER."
  (unless (non-empty-string-p content)
    (error 'agent-loop-error
           :message "A user turn requires non-empty content."
           :conversation-id (conversation-identifier (agent-conversation agent))
           :request-number nil))
  (with-lock-held ((agent-turn-lock agent))
    (let ((conversation (agent-conversation agent)))
      (conversation-append-user-message conversation content)
      (unwind-protect
           (agent--run-provider-loop agent observer goal-context)
        (setf (conversation-turn-state conversation) nil)))))


;;;; -- Provider and Persistence Flow --

(-> agent--portable-value (t) t)
(defun agent--portable-value (value)
  "Convert provider VALUE into portable readable conversation metadata."
  (cond
    ((hash-table-p value)
     (sort
      (loop for key being the hash-keys of value
              using (hash-value child)
            collect (list key (agent--portable-value child)))
      #'string<
      :key #'first))
    ((vectorp value)
     (loop for child across value
           collect (agent--portable-value child)))
    ((listp value)
     (mapcar #'agent--portable-value value))
    (t
     value)))

(-> agent--provider-event-callback (agent-observer) function)
(defun agent--provider-event-callback (observer)
  "Return a provider callback that forwards streaming presentation events to OBSERVER."
  (lambda (event)
    (typecase event
      (assistant-delta-event
       (agent-observer-text observer (assistant-delta-event-text event)))
      (reasoning-delta-event
       (agent-observer-reasoning observer (reasoning-delta-event-text event)))
      (provider-event
       nil)
      (t
       nil))))

(-> agent--persist-provider-result
    (agent provider-result integer)
    null)
(defun agent--persist-provider-result (agent result request-number)
  "Persist RESULT's authoritative items and portable completion metadata in wire order."
  (let ((conversation (agent-conversation agent)))
    (dolist (item (provider-result-output-items result))
      (unless (json-object-p item)
        (error 'agent-loop-error
               :message "The provider returned a completed item that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number request-number))
      (conversation-append-provider-item conversation item))
    (conversation-append-provider-metadata
     conversation
     (list :request-number request-number
           :response-id (provider-result-response-id result)
           :usage (agent--portable-value (provider-result-usage result)))))
  nil)

(-> agent--validate-tool-call-identifiers
    (agent list hash-table integer)
    null)
(defun agent--validate-tool-call-identifiers
    (agent calls seen-call-identifiers request-number)
  "Validate CALLS and reserve their unique call identifiers for this user turn."
  (let ((round-identifiers (make-hash-table :test #'equal))
        (conversation (agent-conversation agent)))
    (dolist (call calls)
      (unless (json-object-p call)
        (error 'agent-loop-error
               :message "The provider returned a tool call that is not a JSON object."
               :conversation-id (conversation-identifier conversation)
               :request-number request-number))
      (let ((call-id (json-get call "call_id")))
        (unless (non-empty-string-p call-id)
          (error 'agent-loop-error
                 :message "The provider returned a function call without a call_id."
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (when (or (gethash call-id seen-call-identifiers)
                  (gethash call-id round-identifiers))
          (error 'agent-loop-error
                 :message (format nil "The provider repeated function call identifier ~S."
                                  call-id)
                 :conversation-id (conversation-identifier conversation)
                 :request-number request-number))
        (setf (gethash call-id round-identifiers) t)))
    (maphash (lambda (call-id present-p)
               (declare (ignore present-p))
               (setf (gethash call-id seen-call-identifiers) t))
             round-identifiers))
  nil)

(-> agent--execute-tool-calls
    (agent list agent-observer integer)
    null)
(defun agent--execute-tool-calls (agent calls observer tool-round)
  "Execute CALLS sequentially and append one correlated result for every call."
  (let ((context
          (make-instance 'tool-context
                         :configuration (agent-configuration agent)
                         :worker (agent-worker agent)
                         :conversation (agent-conversation agent))))
    (dolist (call calls)
      (let* ((call-id (json-get call "call_id"))
             (tool-name (function-call-canonical-name call)))
        (agent-observer-status
         observer
         :tool-call-started
         (list :tool-round tool-round
               :call-id call-id
               :tool tool-name))
        (let ((result
                (tool-registry-execute-call
                 (agent-tool-registry agent)
                 call
                 context)))
          (conversation-append-tool-result
           (agent-conversation agent)
           call-id
           tool-name
           (tool-result-content result)
           (tool-result-success-p result))
          (agent-observer-status
           observer
           :tool-call-completed
           (list :tool-round tool-round
                 :call-id call-id
                 :tool tool-name
                 :success-p (tool-result-success-p result)
                 :output (tool-result-content result)))))))
  nil)

(-> agent--reject-tool-calls
    (agent list agent-observer
     &key (:tool-round integer) (:message string))
    null)
(defun agent--reject-tool-calls
    (agent calls observer &key tool-round message)
  "Append one explicit MESSAGE failure output for every rejected call in CALLS."
  (dolist (call calls)
    (let* ((call-id (json-get call "call_id"))
           (tool-name (function-call-canonical-name call)))
      (conversation-append-tool-result
       (agent-conversation agent)
       call-id
       tool-name
       message
       nil)
      (agent-observer-status
       observer
       :tool-call-completed
       (list :tool-round tool-round
             :call-id call-id
             :tool tool-name
             :success-p nil
             :output message))))
  nil)

(-> agent--budget-state (agent integer) turn-budget-state)
(defun agent--budget-state (agent provider-step)
  "Return AGENT's budget phase for PROVIDER-STEP."
  (cond
    ((= provider-step (agent-maximum-provider-steps agent))
     :finalization)
    ((and (agent-provider-step-warning agent)
          (>= provider-step (agent-provider-step-warning agent)))
     :warning)
    (t
     :normal)))

(-> agent--signal-budget-exhausted
    (agent integer integer keyword string)
    null)
(defun agent--signal-budget-exhausted
    (agent provider-step tool-calls reason message)
  "Signal deterministic turn-budget REASON for AGENT with a visible MESSAGE."
  (error 'agent-turn-budget-exhausted
         :message message
         :maximum-provider-steps (agent-maximum-provider-steps agent)
         :provider-step provider-step
         :tool-calls tool-calls
         :reason reason))

(-> agent--run-provider-loop
    (agent agent-observer (option string))
    provider-result)
(defun agent--run-provider-loop (agent observer goal-context)
  "Run bounded provider and tool rounds until AGENT's turn completes."
  (let ((seen-call-identifiers (make-hash-table :test #'equal))
        (request-number 0)
        (tool-rounds 0)
        (tool-calls 0))
    (loop
      (incf request-number)
      (let ((budget-state (agent--budget-state agent request-number)))
        (when (eq budget-state :warning)
          (agent-observer-status
           observer
           :turn-budget-warning
           (list :provider-step request-number
                 :maximum-provider-steps
                 (agent-maximum-provider-steps agent))))
        (agent-observer-status
         observer
         :provider-request-started
         (list :request-number request-number
               :tool-rounds tool-rounds
               :turn-budget-state budget-state))
        (let* ((conversation (agent-conversation agent))
               (provider-tools
                 (if (eq budget-state :finalization)
                     #()
                     (tool-registry-provider-schemas
                      (agent-tool-registry agent))))
               (result
                 (provider-stream-turn
                  (agent-provider agent)
                  conversation
                  provider-tools
                  (agent--provider-event-callback observer)
                  :turn-budget-state budget-state
                  :goal-context goal-context))
               (calls (provider-result-tool-calls result)))
          (agent--validate-tool-call-identifiers
           agent calls seen-call-identifiers request-number)
          (agent--persist-provider-result agent result request-number)
          (setf (conversation-turn-state conversation)
                (provider-result-turn-state result))
          (agent-observer-status
           observer
           :provider-request-completed
           (list :request-number request-number
                 :response-id (provider-result-response-id result)
                 :usage (agent--portable-value (provider-result-usage result))
                 :output-item-count (length (provider-result-output-items result))
                 :tool-call-count (length calls)
                 :turn-completion (provider-result-turn-completion result)
                 :turn-budget-state budget-state))
          (when (eq budget-state :finalization)
            (when calls
              (let ((message
                      "Tools were disabled on the final turn step, so this call was not executed."))
                (agent--reject-tool-calls
                 agent calls observer
                 :tool-round (1+ tool-rounds)
                 :message message)
                (agent--signal-budget-exhausted
                 agent
                 request-number
                 tool-calls
                 :tools-requested-during-finalization
                 "The model requested tools during the text-only final turn step.")))
            (agent-observer-status
             observer
             :turn-completed
             (list :provider-requests request-number
                   :tool-rounds tool-rounds
                   :tool-calls tool-calls
                   :budget-finalization-p t
                   :response-id (provider-result-response-id result)))
            (return result))
          (when (and (null calls)
                     (not (eq (provider-result-turn-completion result) :continue)))
            (agent-observer-status
             observer
             :turn-completed
             (list :provider-requests request-number
                   :tool-rounds tool-rounds
                   :tool-calls tool-calls
                   :response-id (provider-result-response-id result)))
            (return result))
          (cond
            ((null calls)
             (agent-observer-status
              observer
              :provider-follow-up
              (list :request-number request-number)))
            ((> (+ tool-calls (length calls))
                (agent-maximum-tool-calls agent))
             (let ((message
                     (format nil
                             "This call was not executed because the turn reached its ~:D-call tool budget."
                             (agent-maximum-tool-calls agent))))
               (agent--reject-tool-calls
                agent calls observer
                :tool-round (1+ tool-rounds)
                :message message)
               (agent--signal-budget-exhausted
                agent
                request-number
                tool-calls
                :tool-call-limit
                (format nil
                        "The turn reached its ~:D-call tool budget."
                        (agent-maximum-tool-calls agent)))))
            (t
             (incf tool-rounds)
             (incf tool-calls (length calls))
             (agent--execute-tool-calls agent calls observer tool-rounds))))))))
