(in-package #:frob)

;;;; -- Agent Events --

(define-constant +default-maximum-tool-rounds+ 8
  :documentation "The maximum number of model-requested tool batches in one user turn.")

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
   (maximum-tool-rounds
    :initarg :maximum-tool-rounds
    :reader agent-maximum-tool-rounds
    :type (integer 1)
    :documentation "The maximum number of tool batches executed for one user message.")
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

(define-condition agent-tool-round-limit-exceeded (agent-loop-error)
  ((maximum
    :initarg :maximum
    :reader agent-tool-round-limit-exceeded-maximum
    :type (integer 1)
    :documentation "The configured maximum number of executable tool batches.")
   (tool-round
    :initarg :tool-round
    :reader agent-tool-round-limit-exceeded-tool-round
    :type (integer 1)
    :documentation "The first rejected tool round."))
  (:documentation "The model requested another tool batch after the safe turn limit."))


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
     (:maximum-tool-rounds integer))
    agent)
(defun agent-create
    (&key
       configuration
       provider
       conversation
       tool-registry
       worker
       (maximum-tool-rounds +default-maximum-tool-rounds+))
  "Create an agent, filling unspecified provider, conversation, registry, and worker roles."
  (unless (typep configuration 'configuration)
    (error 'configuration-error
           :message "AGENT-CREATE requires a CONFIGURATION instance."))
  (unless (typep maximum-tool-rounds '(integer 1))
    (error 'configuration-error
           :message "The maximum tool rounds must be a positive integer."))
  (make-instance 'agent
                 :configuration configuration
                 :provider (or provider (provider-create configuration))
                 :conversation (or conversation
                                   (conversation-create configuration))
                 :tool-registry (or tool-registry
                                    (make-default-tool-registry))
                 :worker (or worker (lisp-worker-create configuration))
                 :maximum-tool-rounds maximum-tool-rounds))

(-> agent-run-user-turn
    (agent string &key (:observer agent-observer))
    provider-result)
(defgeneric agent-run-user-turn (agent content &key observer)
  (:documentation
   "Persist user CONTENT, run all bounded model and tool rounds, and return the final provider result."))

(defmethod agent-run-user-turn
    ((agent agent) (content string) &key (observer (make-instance 'agent-observer)))
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
           (agent--run-provider-loop agent observer)
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

(-> agent--reject-tool-calls-at-limit
    (agent list agent-observer integer)
    null)
(defun agent--reject-tool-calls-at-limit (agent calls observer tool-round)
  "Append explicit failure outputs for CALLS that exceed AGENT's safe round limit."
  (let ((maximum (agent-maximum-tool-rounds agent)))
    (agent-observer-status
     observer
     :tool-round-limit-reached
     (list :tool-round tool-round :maximum maximum))
    (dolist (call calls)
      (let* ((call-id (json-get call "call_id"))
             (tool-name (function-call-canonical-name call))
             (output
               (format nil
                       "~A was not executed because this turn reached the ~:D-round tool limit."
                       tool-name
                       maximum)))
        (conversation-append-tool-result
         (agent-conversation agent)
         call-id
         tool-name
         output
         nil)
        (agent-observer-status
         observer
         :tool-call-completed
         (list :tool-round tool-round
               :call-id call-id
               :tool tool-name
               :success-p nil
               :output output))))
  nil))

(-> agent--run-provider-loop (agent agent-observer) provider-result)
(defun agent--run-provider-loop (agent observer)
  "Run repeated provider and tool rounds until AGENT receives a tool-free result."
  (let ((seen-call-identifiers (make-hash-table :test #'equal))
        (request-number 0)
        (tool-rounds 0))
    (loop
      (incf request-number)
      (agent-observer-status
       observer
       :provider-request-started
       (list :request-number request-number
             :tool-rounds tool-rounds))
      (let* ((conversation (agent-conversation agent))
             (result
               (provider-stream-turn
                (agent-provider agent)
                conversation
                (tool-registry-provider-schemas (agent-tool-registry agent))
                (agent--provider-event-callback observer)))
             (calls (provider-result-tool-calls result)))
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
               :tool-call-count (length calls)))
        (unless calls
          (agent-observer-status
           observer
           :turn-completed
           (list :provider-requests request-number
                 :tool-rounds tool-rounds
                 :response-id (provider-result-response-id result)))
          (return result))
        (agent--validate-tool-call-identifiers
         agent calls seen-call-identifiers request-number)
        (when (>= tool-rounds (agent-maximum-tool-rounds agent))
          (let ((rejected-round (1+ tool-rounds)))
            (agent--reject-tool-calls-at-limit
             agent calls observer rejected-round)
            (error 'agent-tool-round-limit-exceeded
                   :message (format nil
                                    "The model exceeded the ~:D-round tool limit."
                                    (agent-maximum-tool-rounds agent))
                   :conversation-id (conversation-identifier conversation)
                   :request-number request-number
                   :maximum (agent-maximum-tool-rounds agent)
                   :tool-round rejected-round)))
        (incf tool-rounds)
        (agent--execute-tool-calls agent calls observer tool-rounds)))))
