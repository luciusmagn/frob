(in-package #:autolith)

;;;; -- Agent Events --

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
    :documentation "The optional function called with a status keyword and portable details.")
   (steering-callback
    :initarg :steering-callback
    :initform nil
    :reader callback-agent-observer-steering-callback
    :type (option function)
    :documentation "The optional function that drains user messages waiting for a tool boundary.")
   (command-authorization-callback
    :initarg :command-authorization-callback
    :initform nil
    :reader callback-agent-observer-command-authorization-callback
    :type (option function)
    :documentation "The optional function authorizing one external command."))
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
   (turn-lock
    :initform (make-lock "Autolith agent turn")
    :reader agent-turn-lock
    :documentation "The lock preventing concurrent mutation of conversation turn state."))
  (:documentation "A model-driven conversation loop with namespaced Common Lisp tools."))


;;;; -- Agent Conditions --

(define-condition agent-loop-error (autolith-error)
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

(-> agent-observer-take-steering (agent-observer) list)
(defgeneric agent-observer-take-steering (observer)
  (:documentation
   "Return and consume user messages waiting at OBSERVER's next tool boundary."))

(-> agent-observer-authorize-command
    (agent-observer string pathname)
    keyword)
(defgeneric agent-observer-authorize-command (observer command directory)
  (:documentation "Return the execution permission for COMMAND in DIRECTORY."))

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

(defmethod agent-observer-take-steering ((observer agent-observer))
  "Return no steering messages for the default silent OBSERVER."
  (declare (ignore observer))
  nil)

(defmethod agent-observer-authorize-command
    ((observer agent-observer) (command string) (directory pathname))
  "Deny COMMAND when OBSERVER has no authorization interface."
  (declare (ignore observer command directory))
  ':deny)

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

(defmethod agent-observer-take-steering ((observer callback-agent-observer))
  "Drain steering messages through OBSERVER's configured callback."
  (let ((callback (callback-agent-observer-steering-callback observer)))
    (if callback
        (funcall callback)
        nil)))

(defmethod agent-observer-authorize-command
    ((observer callback-agent-observer) (command string) (directory pathname))
  "Authorize COMMAND through OBSERVER's callback, denying when absent."
  (let ((callback
          (callback-agent-observer-command-authorization-callback observer)))
    (if callback
        (funcall callback command directory)
        ':deny)))


;;;; -- Construction and Turn Entry --

(-> callback-agent-observer-create
    (&key
     (:text-callback (option function))
     (:reasoning-callback (option function))
     (:status-callback (option function))
     (:steering-callback (option function))
     (:command-authorization-callback (option function)))
    callback-agent-observer)
(defun callback-agent-observer-create
    (&key text-callback reasoning-callback status-callback steering-callback
      command-authorization-callback)
  "Create an observer backed by optional presentation callbacks."
  (make-instance 'callback-agent-observer
                 :text-callback text-callback
                 :reasoning-callback reasoning-callback
                 :status-callback status-callback
                 :steering-callback steering-callback
                 :command-authorization-callback
                 command-authorization-callback))

(-> agent-create
    (&key
     (:configuration configuration)
     (:provider (option model-provider))
     (:conversation (option conversation))
     (:tool-registry (option tool-registry))
     (:worker t))
    agent)
(defun agent-create
    (&key
       configuration
       provider
       conversation
       tool-registry
       worker)
  "Create an agent, filling unspecified provider, conversation, registry, and worker roles."
  (unless (typep configuration 'configuration)
    (error 'configuration-error
           :message "AGENT-CREATE requires a CONFIGURATION instance."))
  (make-instance 'agent
                 :configuration configuration
                 :provider (or provider (provider-create configuration))
                 :conversation (or conversation
                                   (conversation-create configuration))
                 :tool-registry (or tool-registry
                                    (make-default-tool-registry))
                 :worker (or worker (lisp-worker-pool-create configuration))))

(-> agent-run-user-turn
    (agent (or string user-message-input) &key (:observer agent-observer)
           (:goal-context (option string)))
    provider-result)
(defgeneric agent-run-user-turn (agent content &key observer goal-context)
  (:documentation
   "Persist user CONTENT, run model and tool rounds, and return the final provider result."))

(defmethod agent-run-user-turn
    ((agent agent) (content string)
     &key (observer (make-instance 'agent-observer)) goal-context)
  "Normalize a textual user turn before running it through AGENT."
  (agent-run-user-turn agent
                       (user-message-input-create :text content)
                       :observer observer
                       :goal-context goal-context))

(defmethod agent-run-user-turn
    ((agent agent) (content user-message-input)
     &key (observer (make-instance 'agent-observer)) goal-context)
  "Run one serialized user turn through AGENT while presenting events to OBSERVER."
  (unless (or (non-empty-string-p (user-message-input-text content))
              (user-message-input-image-pathnames content))
    (error 'agent-loop-error
           :message "A user turn requires text or an image."
           :conversation-id (conversation-identifier (agent-conversation agent))
           :request-number nil))
  (with-lock-held ((agent-turn-lock agent))
    (let ((conversation (agent-conversation agent)))
      ;; Compact before appending CONTENT so the fresh question survives
      ;; verbatim instead of being folded into the summary.
      (when (agent--should-compact-p agent)
        (agent-compact-conversation agent observer))
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
       (agent-observer-status observer :provider-progress nil)
       (agent-observer-text observer (assistant-delta-event-text event)))
      (reasoning-delta-event
       (agent-observer-status observer :provider-progress nil)
       (agent-observer-reasoning observer (reasoning-delta-event-text event)))
      (provider-event
       (agent-observer-status observer :provider-progress nil))
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
    (agent list
     &key (:seen-call-identifiers hash-table) (:request-number integer))
    null)
(defun agent--validate-tool-call-identifiers
    (agent calls &key seen-call-identifiers request-number)
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
    (agent list &key (:observer agent-observer) (:tool-round integer))
    null)
(defun agent--execute-tool-calls (agent calls &key observer tool-round)
  "Execute CALLS sequentially and append one correlated result for every call."
  (dolist (call calls)
    (let* ((call-id (json-get call "call_id"))
           (tool-name (function-call-canonical-name call))
           (context
             (make-instance 'tool-context
                            :configuration (agent-configuration agent)
                            :worker (agent-worker agent)
                            :conversation (agent-conversation agent)
                            :registry (agent-tool-registry agent)
                            :agent agent
                            :observer observer
                            :call-id call-id
                            :command-authorization-function
                            (lambda (command directory)
                              (agent-observer-authorize-command
                               observer command directory)))))
      (agent-observer-status
       observer
       :tool-call-started
       (list :tool-round tool-round
             :call-id call-id
             :tool tool-name))
      (let* ((real-start (get-internal-real-time))
             (cpu-start (get-internal-run-time))
             (result
               (tool-registry-execute-call
                (agent-tool-registry agent)
                call
                context))
             (cpu-microseconds
               (round (* (- (get-internal-run-time) cpu-start) 1000000)
                      internal-time-units-per-second))
             (real-microseconds
               (round (* (- (get-internal-real-time) real-start) 1000000)
                      internal-time-units-per-second)))
        (conversation-append-tool-result
         (agent-conversation agent)
         call-id
         :tool-name tool-name
         :output (tool-result-content result)
         :image-attachments (tool-result-image-attachments result)
         :success-p (tool-result-success-p result)
         :cpu-microseconds cpu-microseconds
         :real-microseconds real-microseconds)
        (agent-observer-status
         observer
         :tool-call-completed
         (list :tool-round tool-round
               :call-id call-id
               :tool tool-name
               :success-p (tool-result-success-p result)
               :cpu-microseconds cpu-microseconds
               :real-microseconds real-microseconds
               :output (tool-result-content result)
               :details (tool-result-details result))))))
  nil)

(-> agent--apply-steering-input (agent agent-observer integer) null)
(defun agent--apply-steering-input (agent observer request-number)
  "Persist user messages drained from OBSERVER after one completed tool round."
  (let ((messages (agent-observer-take-steering observer))
        (conversation (agent-conversation agent)))
    (unless (listp messages)
      (error 'agent-loop-error
             :message "The agent observer returned malformed steering input."
             :conversation-id (conversation-identifier conversation)
             :request-number request-number))
    (dolist (message messages)
      (unless (or (and (stringp message) (non-empty-string-p message))
                  (and (typep message 'user-message-input)
                       (or (non-empty-string-p
                            (user-message-input-text message))
                           (user-message-input-image-pathnames message))))
        (error 'agent-loop-error
               :message "The agent observer returned an empty steering message."
               :conversation-id (conversation-identifier conversation)
               :request-number request-number))
      (conversation-append-user-message conversation message))
    (when messages
      (agent-observer-status
       observer
       :steering-applied
       (list :message-count (length messages)))))
  nil)

(-> agent--should-compact-p (agent) boolean)
(defun agent--should-compact-p (agent)
  "Return true when the newest usage crossed AGENT's compaction limit."
  (>= (conversation-last-total-tokens (agent-conversation agent))
      (configuration-compaction-token-limit (agent-configuration agent))))

(-> agent-compact-conversation (agent agent-observer) null)
(defun agent-compact-conversation (agent observer)
  "Summarize AGENT's conversation and replace its projection with the summary.

The summarization request itself is a side channel: its output items are not
persisted as history, only the durable summary record is."
  (let ((conversation (agent-conversation agent)))
    (agent-observer-status
     observer
     :compaction-started
     (list :total-tokens (conversation-last-total-tokens conversation)))
    (let* ((result (provider-stream-turn
                    (agent-provider agent)
                    conversation
                    :tool-namespaces #()
                    :event-callback
                    (lambda (event)
                      (declare (ignore event))
                      (agent-observer-status observer :provider-progress nil))
                    :compaction-p t))
           (summary (provider-result-assistant-text result)))
      (unless (non-empty-string-p summary)
        (error 'agent-loop-error
               :message "Compaction produced no summary text."
               :conversation-id (conversation-identifier conversation)
               :request-number nil))
      (conversation-append-summary conversation summary)
      (agent-observer-status
       observer
       :compaction-completed
       (list :summary-characters (length summary)))))
  nil)

(-> agent--run-provider-loop
    (agent agent-observer (option string))
    provider-result)
(defun agent--run-provider-loop (agent observer goal-context)
  "Run provider and tool rounds until AGENT's turn completes."
  (let ((seen-call-identifiers (make-hash-table :test #'equal))
        (request-number 0)
        (tool-rounds 0)
        (tool-calls 0))
    (loop
      (when (agent--should-compact-p agent)
        (agent-compact-conversation agent observer))
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
                :tool-namespaces
                (tool-registry-provider-schemas
                 (agent-tool-registry agent))
                :event-callback (agent--provider-event-callback observer)
                :goal-context goal-context))
             (calls (provider-result-tool-calls result)))
        (agent--validate-tool-call-identifiers
         agent
         calls
         :seen-call-identifiers seen-call-identifiers
         :request-number request-number)
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
               :turn-completion (provider-result-turn-completion result)))
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
          (t
           (incf tool-rounds)
           (incf tool-calls (length calls))
           (agent--execute-tool-calls agent calls
                                      :observer observer
                                      :tool-round tool-rounds)
           (agent--apply-steering-input agent observer request-number)))))))
