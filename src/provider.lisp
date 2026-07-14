(in-package #:autolith)

;;;; -- Provider Events --

(defclass provider-event ()
  ()
  (:documentation "A semantic event emitted while consuming a provider stream."))

(defclass assistant-delta-event (provider-event)
  ((text
    :initarg :text
    :reader assistant-delta-event-text
    :type string
    :documentation "The newly received assistant text."))
  (:documentation "An incremental assistant text update."))

(defclass reasoning-delta-event (provider-event)
  ((text
    :initarg :text
    :reader reasoning-delta-event-text
    :type string
    :documentation "The newly received visible reasoning summary text."))
  (:documentation "An incremental visible reasoning summary update."))

(defclass provider-item-event (provider-event)
  ((item
    :initarg :item
    :reader provider-item-event-item
    :type json-object
    :documentation "The authoritative completed Responses item."))
  (:documentation "A completed provider output item ready for persistence."))

(defclass provider-completed-event (provider-event)
  ((response-id
    :initarg :response-id
    :reader provider-completed-event-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (usage
    :initarg :usage
    :reader provider-completed-event-usage
    :type t
    :documentation "Portable provider usage metadata, if supplied.")
   (turn-completion
    :initarg :turn-completion
    :initform :unspecified
    :reader provider-completed-event-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The successful terminal event for one provider request."))

(defclass provider-result ()
  ((response-id
    :initarg :response-id
    :reader provider-result-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (output-items
    :initarg :output-items
    :reader provider-result-output-items
    :type list
    :documentation "Authoritative completed response items in wire order.")
   (tool-calls
    :initarg :tool-calls
    :reader provider-result-tool-calls
    :type list
    :documentation "The function-call subset of OUTPUT-ITEMS.")
   (usage
    :initarg :usage
    :reader provider-result-usage
    :type t
    :documentation "Provider usage metadata, if supplied.")
   (turn-state
    :initarg :turn-state
    :reader provider-result-turn-state
    :type (option string)
    :documentation "The routing token to replay within the current user turn.")
   (turn-completion
    :initarg :turn-completion
    :initform :unspecified
    :reader provider-result-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The complete semantic result of one streamed provider request."))


;;;; -- Provider Protocol --

(defclass model-provider ()
  ()
  (:documentation "The abstract interface between an agent and a model service."))

(defclass codex-subscription-provider (model-provider)
  ((configuration
    :initarg :configuration
    :reader provider-configuration
    :type configuration
    :documentation "Immutable model and path configuration.")
   (credential-manager
    :initarg :credential-manager
    :reader provider-credential-manager
    :type credential-manager
    :documentation "Credential paths and refresh policy without retained tokens.")
   (session-id
    :initarg :session-id
    :reader provider-session-id
    :type non-empty-string
    :documentation "The stable provider session identifier.")
   (reasoning-summaries-p
    :initarg :reasoning-summaries-p
    :initform nil
    :accessor provider-reasoning-summaries-p
    :type boolean
    :documentation "Whether requests opt in to provider-visible reasoning summaries.")
   (rate-limits
    :initform nil
    :accessor provider-rate-limits
    :type list
    :documentation "The most recent portable rate limit snapshot from response headers."))
  (:documentation "A direct ChatGPT subscription client for the Codex Responses service."))

(defmethod provider-rate-limits ((provider model-provider))
  "Return no rate limit snapshot for providers that do not report one."
  (declare (ignore provider))
  nil)

(-> provider-create
    (configuration &key (:reasoning-summaries-p boolean))
    codex-subscription-provider)
(defun provider-create (configuration &key reasoning-summaries-p)
  "Create the default direct subscription provider for CONFIGURATION."
  (make-instance 'codex-subscription-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)
                 :reasoning-summaries-p reasoning-summaries-p))

(-> provider-with-configuration (model-provider configuration) model-provider)
(defgeneric provider-with-configuration (provider configuration)
  (:documentation
   "Return PROVIDER reconfigured for CONFIGURATION while preserving session state."))

(defmethod provider-with-configuration
    ((provider codex-subscription-provider) (configuration configuration))
  "Copy PROVIDER with CONFIGURATION, retaining credentials, session, and limits."
  (let ((copy
          (make-instance 'codex-subscription-provider
                         :configuration configuration
                         :credential-manager
                         (provider-credential-manager provider)
                         :session-id (provider-session-id provider)
                         :reasoning-summaries-p
                         (provider-reasoning-summaries-p provider))))
    (setf (provider-rate-limits copy) (copy-tree (provider-rate-limits provider)))
    copy))

(-> provider-set-reasoning-summaries (model-provider boolean) model-provider)
(defgeneric provider-set-reasoning-summaries (provider enabled-p)
  (:documentation
   "Set whether PROVIDER requests visible reasoning summaries when supported."))

(defmethod provider-set-reasoning-summaries
    ((provider model-provider) (enabled-p t))
  "Leave providers without reasoning-summary support unchanged."
  (declare (ignore enabled-p))
  provider)

(defmethod provider-set-reasoning-summaries
    ((provider codex-subscription-provider) (enabled-p t))
  "Set whether the Codex subscription provider requests reasoning summaries."
  (check-type enabled-p boolean)
  (setf (provider-reasoning-summaries-p provider) enabled-p)
  provider)

(-> provider-stream-turn
    (model-provider conversation vector function
     &key (:turn-budget-state turn-budget-state)
          (:goal-context (option string))
          (:compaction-p boolean))
    provider-result)
(defgeneric provider-stream-turn
    (provider conversation tool-namespaces event-callback
     &key turn-budget-state goal-context compaction-p)
  (:documentation
   "Stream one model response for CONVERSATION using TOOL-NAMESPACES and EVENT-CALLBACK."))

(-> provider-open-response-stream
    (model-provider json-object oauth-credentials conversation)
    (values stream integer t))
(defgeneric provider-open-response-stream (provider request credentials conversation)
  (:documentation "Open an authenticated provider stream and return body, status, and headers."))


;;;; -- Responses Lite Encoding --

(-> responses-lite-developer-message (string) json-object)
(defun responses-lite-developer-message (instructions)
  "Return the Responses Lite developer message containing INSTRUCTIONS."
  (json-object
   "type" "message"
   "role" "developer"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" instructions))))

(-> responses-lite-additional-tools (vector) json-object)
(defun responses-lite-additional-tools (tool-namespaces)
  "Return the Responses Lite developer item exposing TOOL-NAMESPACES."
  (json-object
   "type" "additional_tools"
   "role" "developer"
   "tools" tool-namespaces))

(define-constant +turn-budget-warning-instructions+
  "This turn is approaching its step budget. Finish the task efficiently. Avoid redundant inspection, combine independent work where practical, and preserve enough time to verify and summarize the result."
  :test #'string=
  :documentation "The model-visible reminder added late in a long agent turn.")

(define-constant +turn-budget-finalization-instructions+
  "This is the final step available for the current user turn. Tools are disabled. Respond with text only, stating what was completed, what remains, and the safest next action. Do not request or describe a tool call as completed."
  :test #'string=
  :documentation "The text-only instruction used for the final provider step.")

(-> responses-lite-budget-message (turn-budget-state) (option json-object))
(defun responses-lite-budget-message (state)
  "Return the transient developer message for turn-budget STATE, if any."
  (case state
    (:warning
     (responses-lite-developer-message +turn-budget-warning-instructions+))
    (:finalization
     (responses-lite-developer-message +turn-budget-finalization-instructions+))
    (t
     nil)))

;; Modeled on the Codex context checkpoint compaction instructions at
;; reference commit 5c19155c, restated for Autolith.
(define-constant +compaction-instructions+
  "You are performing a context checkpoint compaction. Write a handoff summary for another model that will resume this conversation. Include the current progress and key decisions, important context, constraints, and user preferences, what remains to be done as clear next steps, and any critical data or references needed to continue. Be concise, structured, and complete enough that no earlier context is required."
  :test #'string=
  :documentation "The developer instructions driving one compaction request.")

(-> response-item-assistant-text (json-object) (option string))
(defun response-item-assistant-text (item)
  "Return the joined visible text of assistant message ITEM, when applicable."
  (when (and (string= (or (json-get item "type") "") "message")
             (string= (or (json-get item "role") "") "assistant"))
    (let ((content (json-get item "content")))
      (when (vectorp content)
        (let ((parts
                (loop for part across content
                      when (and (json-object-p part)
                                (member (json-get part "type")
                                        '("output_text" "text")
                                        :test #'string=)
                                (stringp (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~%~}" parts)))))))

(-> provider-result-assistant-text (provider-result) (option string))
(defun provider-result-assistant-text (result)
  "Return the joined assistant text across RESULT's output items."
  (let ((parts (loop for item in (provider-result-output-items result)
                     for text = (and (json-object-p item)
                                     (response-item-assistant-text item))
                     when text
                       collect text)))
    (when parts
      (format nil "~{~A~^~%~}" parts))))

(-> response-item-reasoning-summary (json-object) (option string))
(defun response-item-reasoning-summary (item)
  "Return ITEM's provider-visible reasoning summary, never raw reasoning text."
  (when (string= (or (json-get item "type") "") "reasoning")
    (let ((summary (json-get item "summary")))
      (when (vectorp summary)
        (let ((parts
                (loop for part across summary
                      when (and (json-object-p part)
                                (string= (or (json-get part "type") "")
                                         "summary_text")
                                (non-empty-string-p (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~2%~}" parts)))))))

(-> provider-web-search-tool (configuration) (option json-object))
(defun provider-web-search-tool (configuration)
  "Return the hosted web search tool for CONFIGURATION, or NIL when disabled.

Cached mode keeps external_web_access false so searches use the provider's
indexed corpus, while live mode permits direct fetches. The tool rides in the
additional_tools developer item exactly as Codex Responses Lite requests do
at reference commit 5c19155c."
  (let ((mode (configuration-web-search-mode configuration)))
    (cond
      ((string= mode "disabled")
       nil)
      ((string= mode "live")
       (json-object "type" "web_search"
                    "external_web_access" t))
      (t
       (json-object "type" "web_search"
                    "external_web_access" false)))))

;; No service_tier field is ever sent. Omitting it selects the provider's
;; standard processing path; sending "priority" selects the fast path that
;; drains subscription rate limits much faster (Codex reference commit
;; 5c19155c filters its explicit "default" sentinel out of requests too).
(-> provider-request-object
    (codex-subscription-provider conversation vector
     &key (:turn-budget-state turn-budget-state)
          (:goal-context (option string))
          (:compaction-p boolean))
    json-object)
(defun provider-request-object
    (provider conversation tool-namespaces
     &key (turn-budget-state :normal) goal-context compaction-p)
  "Build the complete stateless Sol Responses Lite request for CONVERSATION.

The request never carries a service_tier, keeping Autolith on the standard path.
GOAL-CONTEXT rides as one transient developer message that is never persisted
in the durable conversation, mirroring the budget reminders. COMPACTION-P
builds a tool-free summarization request whose trailing developer message
asks for a context checkpoint handoff."
  (let* ((configuration (provider-configuration provider))
         (finalization-p (eq turn-budget-state :finalization))
         (web-search-tool (and (not finalization-p)
                               (not compaction-p)
                               (provider-web-search-tool configuration)))
         (effective-tools
           (cond
             ((or finalization-p compaction-p)
              #())
             (web-search-tool
              (concatenate 'vector
                           tool-namespaces
                           (vector web-search-tool)))
             (t
              tool-namespaces)))
         (budget-message (and (not compaction-p)
                              (responses-lite-budget-message turn-budget-state)))
         (reasoning
           (json-object
            "effort" (configuration-wire-effort configuration)
            "context" "all_turns"))
         (prefix (append
                  (list (responses-lite-additional-tools effective-tools)
                        (responses-lite-developer-message
                         (system-prompt configuration)))
                  (when (and goal-context
                             (not finalization-p)
                             (not compaction-p))
                    (list (responses-lite-developer-message goal-context)))
                  (when budget-message
                    (list budget-message))))
         (input (coerce (append prefix
                                (conversation-input-items conversation)
                                (when compaction-p
                                  (list (responses-lite-developer-message
                                        +compaction-instructions+))))
                        'vector)))
    (when (and (provider-reasoning-summaries-p provider)
               (not compaction-p))
      (setf (gethash "summary" reasoning) "auto"))
    (json-object
     "model" (configuration-model configuration)
     "input" input
     "tool_choice" "auto"
     "parallel_tool_calls" false
     "reasoning" reasoning
     "store" false
     "stream" t
     "include" (json-array "reasoning.encrypted_content")
     "prompt_cache_key" (conversation-identifier conversation)
     "text" (json-object "verbosity" "low"))))

(-> provider-user-agent () string)
(defun provider-user-agent ()
  "Return an honest, stable user agent for direct Autolith provider requests."
  (format nil "autolith/~A (~A ~A; ~A)"
          +autolith-version+
          (software-type)
          (software-version)
          (machine-type)))

(defmethod provider-open-response-stream
    ((provider codex-subscription-provider)
     (request hash-table)
     (credentials oauth-credentials)
     (conversation conversation))
  "Open a direct authenticated SSE request to the ChatGPT Codex endpoint."
  (let* ((configuration (provider-configuration provider))
         (thread-id (conversation-identifier conversation))
         (request-id (make-identifier))
         (headers
           (append
            (list
             (cons "Authorization"
                   (format nil "Bearer ~A"
                           (oauth-credentials-access-token credentials)))
             (cons "ChatGPT-Account-ID"
                   (oauth-credentials-account-id credentials))
             (cons "Content-Type" "application/json")
             (cons "Accept" "text/event-stream")
             (cons "x-openai-internal-codex-responses-lite" "true")
             (cons "originator" "autolith")
             (cons "User-Agent" (provider-user-agent))
             (cons "session-id" (provider-session-id provider))
             (cons "thread-id" thread-id)
             (cons "x-client-request-id" request-id))
            (when (conversation-turn-state conversation)
              (list (cons "x-codex-turn-state"
                          (conversation-turn-state conversation)))))))
    (dexador:post
     (configuration-provider-endpoint configuration)
     :headers headers
     :content (json-encode request)
     :want-stream t
     :force-string t
     :keep-alive nil
     :connect-timeout 30
     :read-timeout 300)))


;;;; -- SSE Decoding --

(defparameter +sse-end-of-stream+ (gensym "SSE-END-")
  "A private marker returned after a clean SSE end of stream.")

(-> sse-data-line (string) (option string))
(defun sse-data-line (line)
  "Return the payload of an SSE data LINE, or NIL for another field."
  (when (and (>= (length line) 5)
             (string= line "data:" :end1 5 :end2 5))
    (let ((start (if (and (> (length line) 5)
                          (char= (char line 5) #\Space))
                     6
                     5)))
      (subseq line start))))

(-> sse-read-line (stream) t)
(defun sse-read-line (stream)
  "Read a line from STREAM using only the portable character-stream protocol."
  (let ((characters nil))
    (loop for character = (read-char stream nil +sse-end-of-stream+)
          do (cond
               ((eq character +sse-end-of-stream+)
                (return (if characters
                            (coerce (nreverse characters) 'string)
                            +sse-end-of-stream+)))
               ((char= character #\Newline)
                (return (coerce (nreverse characters) 'string)))
               (t
                (push character characters))))))

(-> read-sse-data (stream) t)
(defun read-sse-data (stream)
  "Read one SSE event's joined data field from STREAM."
  (let ((data-lines nil))
    (loop
      (let ((raw-line (sse-read-line stream)))
        (when (eq raw-line +sse-end-of-stream+)
          (return (if data-lines
                      (format nil "~{~A~^~%~}" (nreverse data-lines))
                      +sse-end-of-stream+)))
        (let ((line (string-right-trim '(#\Return) raw-line)))
          (when (zerop (length line))
            (when data-lines
              (return (format nil "~{~A~^~%~}" (nreverse data-lines)))))
          (let ((data (sse-data-line line)))
            (when data
              (push data data-lines))))))))

(-> response-header (t string) (option string))
(defun response-header (headers name)
  "Return case-insensitive header NAME from Dexador HEADERS."
  (labels ((matching-name-p (candidate)
             (string-equal (string candidate) name)))
    (cond
      ((hash-table-p headers)
       (loop for key being the hash-keys of headers
               using (hash-value value)
             when (matching-name-p key)
               return value))
      ((listp headers)
       (let ((pair (find name headers :key #'first :test #'string-equal)))
         (when pair
           (if (consp (rest pair))
               (second pair)
               (rest pair)))))
      (t
       nil))))

;;;; -- Rate Limit Snapshots --

(define-constant +unix-epoch-universal-time+ 2208988800
  :documentation "The Common Lisp universal time of the POSIX epoch.")

(-> provider--parse-decimal (string) (option real))
(defun provider--parse-decimal (text)
  "Parse non-negative decimal TEXT such as 28 or 28.5 without the Lisp reader."
  (handler-case
      (let* ((trimmed (string-trim " " text))
             (dot (position #\. trimmed)))
        (if dot
            (let ((whole (parse-integer trimmed :end dot))
                  (fraction (subseq trimmed (1+ dot))))
              (if (zerop (length fraction))
                  whole
                  (float (+ whole
                            (/ (parse-integer fraction)
                               (expt 10 (length fraction)))))))
            (parse-integer trimmed)))
    (error ()
      nil)))

(-> provider--rate-limit-window (t string) (option list))
(defun provider--rate-limit-window (headers prefix)
  "Return one portable rate limit window parsed from HEADERS under PREFIX."
  (let ((used (response-header headers
                               (format nil "~A-used-percent" prefix))))
    (when (non-empty-string-p used)
      (let ((used-percent (provider--parse-decimal used))
            (minutes (response-header headers
                                      (format nil "~A-window-minutes" prefix)))
            (resets (response-header headers
                                     (format nil "~A-reset-at" prefix))))
        (when used-percent
          (list :used-percent used-percent
                :window-minutes (and (non-empty-string-p minutes)
                                     (parse-integer minutes :junk-allowed t))
                :resets-at (let ((seconds
                                   (and (non-empty-string-p resets)
                                        (parse-integer resets
                                                       :junk-allowed t))))
                             (and seconds
                                  (+ seconds
                                     +unix-epoch-universal-time+)))))))))

(-> provider-rate-limit-snapshot (t) (option list))
(defun provider-rate-limit-snapshot (headers)
  "Return the portable subscription rate limit snapshot carried by HEADERS."
  (let ((primary (provider--rate-limit-window headers "x-codex-primary"))
        (secondary (provider--rate-limit-window headers "x-codex-secondary")))
    (when (or primary secondary)
      (list :captured-at (get-universal-time)
            :primary primary
            :secondary secondary))))

(-> provider-record-rate-limits (codex-subscription-provider t) (option list))
(defun provider-record-rate-limits (provider headers)
  "Record and return rate limit data from HEADERS when the provider sent it."
  (let ((snapshot (provider-rate-limit-snapshot headers)))
    (when snapshot
      (setf (provider-rate-limits provider) snapshot))
    snapshot))

(-> provider--drain-error-body (stream) (option string))
(defun provider--drain-error-body (stream)
  "Read and return a bounded error body from STREAM, closing it afterwards."
  (unwind-protect
       (handler-case
           (let ((buffer (make-string 4000)))
             (let ((end (read-sequence buffer stream)))
               (and (plusp end) (subseq buffer 0 end))))
         (error ()
           nil))
    (when (open-stream-p stream)
      (close stream))))

(-> provider--error-body-detail ((option string)) (option string))
(defun provider--error-body-detail (body)
  "Return the human-readable explanation carried by an error BODY, if any."
  (when (non-empty-string-p body)
    (let ((message
            (handler-case
                (let ((decoded (json-decode body)))
                  (when (json-object-p decoded)
                    (let ((error-object (json-get decoded "error")))
                      (or (and (json-object-p error-object)
                               (let ((text (json-get error-object "message")))
                                 (and (non-empty-string-p text) text)))
                          (let ((detail (json-get decoded "detail")))
                            (and (non-empty-string-p detail) detail))))))
              (error ()
                nil))))
      (bounded-string (or message body) :limit 400))))

(-> provider--http-error-message (integer (option string)) string)
(defun provider--http-error-message (status body)
  "Return a display message for HTTP STATUS including BODY's explanation."
  (let ((detail (provider--error-body-detail body))
        (hint (case status
                (404 "The requested resource or model is not being served.")
                (429 "The subscription rate limit was reached; see /status.")
                ((500 502 503) "The provider service is having trouble.")
                (t nil))))
    (format nil "The provider returned HTTP ~D.~@[ ~A~]~@[~%~A~]"
            status
            hint
            detail)))

(-> provider-signal-http-failure
    (codex-subscription-provider http-request-failed)
    null)
(defun provider-signal-http-failure (provider condition)
  "Record CONDITION headers and signal a typed provider or authentication error."
  (let ((status (response-status condition))
        (headers (response-headers condition))
        (body (handler-case
                  (let ((content (response-body condition)))
                    (and (stringp content) content))
                (error ()
                  nil))))
    (provider-record-rate-limits provider headers)
    (if (= status 401)
        (error 'provider-unauthorized
               :message "The provider rejected the current ChatGPT credentials."
               :status status
               :request-id (response-header headers "x-request-id")
               :response nil)
        (error 'provider-error
               :message (provider--http-error-message status body)
               :status status
               :request-id (response-header headers "x-request-id")
               :response (and body (bounded-string body :limit 2000))))))


(-> normalize-response-item (json-object) json-object)
(defun normalize-response-item (item)
  "Remove transient server item identifiers from replayable provider ITEM."
  (remhash "id" item)
  item)

(-> function-call-item-p (json-object) boolean)
(defun function-call-item-p (item)
  "Return true when ITEM is a Responses function call."
  (string= (or (json-get item "type") "") "function_call"))

(-> provider--reasoning-summary-key (json-object) (option list))
(defun provider--reasoning-summary-key (event)
  "Return EVENT's stable reasoning summary part identity, when available."
  (let ((item-id (json-get event "item_id"))
        (output-index (json-get event "output_index"))
        (summary-index (json-get event "summary_index")))
    (cond
      ((integerp output-index)
       (list :output output-index :summary summary-index))
      ((non-empty-string-p item-id)
       (list :item item-id :summary summary-index))
      (t
       nil))))

(-> provider--consume-stream (stream t function) provider-result)
(defun provider--consume-stream (stream headers event-callback)
  "Consume STREAM into a provider result while invoking EVENT-CALLBACK."
  (let ((output-items nil)
        (response-id nil)
        (usage nil)
        (turn-completion :unspecified)
        (reasoning-summary-key nil)
        (completed-p nil))
    (loop until completed-p
          for data = (read-sse-data stream)
          do (when (eq data +sse-end-of-stream+)
               (error 'response-stream-error
                      :message "The provider stream closed before a terminal event."
                      :status nil
                      :request-id nil
                      :response nil))
             (unless (string= data "[DONE]")
               (let* ((event (json-decode data))
                      (type (and (json-object-p event)
                                 (json-get event "type"))))
                 (cond
                   ((string= (or type "") "response.created")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (json-get response "id")))))
                   ((string= (or type "") "response.output_text.delta")
                    (funcall event-callback
                             (make-instance 'assistant-delta-event
                                            :text (or (json-get event "delta") ""))))
                   ((string= (or type "")
                             "response.reasoning_summary_text.delta")
                    (let* ((delta (or (json-get event "delta") ""))
                           (next-key
                             (and (plusp (length delta))
                                  (provider--reasoning-summary-key event)))
                           (new-part-p
                             (and next-key
                                  reasoning-summary-key
                                  (not (equal next-key reasoning-summary-key)))))
                      (when next-key
                        (setf reasoning-summary-key next-key))
                      (funcall event-callback
                               (make-instance
                                'reasoning-delta-event
                                :text (if new-part-p
                                          (format nil "~2%~A" delta)
                                          delta)))))
                   ((string= (or type "") "response.output_item.done")
                    (let ((item (json-get event "item")))
                      (when (json-object-p item)
                        (normalize-response-item item)
                        (push item output-items)
                        (funcall event-callback
                                 (make-instance 'provider-item-event :item item)))))
                   ((string= (or type "") "response.completed")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (or (json-get response "id") response-id)
                              usage (json-get response "usage"))
                        (multiple-value-bind (end-turn present-p)
                            (gethash "end_turn" response)
                          (when present-p
                            (setf turn-completion
                                  (if end-turn :end :continue)))))
                      (setf completed-p t)
                      (funcall event-callback
                               (make-instance 'provider-completed-event
                                              :response-id response-id
                                              :usage usage
                                              :turn-completion turn-completion))))
                   ((and type
                         (member type
                                 '("response.failed" "response.incomplete" "error")
                                 :test #'string=))
                    (error 'provider-error
                           :message (format nil "The provider ended with ~A." type)
                           :status nil
                           :request-id response-id
                           :response (bounded-string data :limit 2000)))))))
    (let* ((ordered-items (nreverse output-items))
           (tool-calls (remove-if-not #'function-call-item-p ordered-items)))
      (make-instance 'provider-result
                     :response-id response-id
                     :output-items ordered-items
                     :tool-calls tool-calls
                     :usage usage
                     :turn-state (response-header headers "x-codex-turn-state")
                     :turn-completion turn-completion))))

(-> provider-attempt-turn
    (model-provider conversation vector function
     &key (:force-refresh boolean)
          (:turn-budget-state turn-budget-state)
          (:goal-context (option string))
          (:compaction-p boolean))
    provider-result)
(defgeneric provider-attempt-turn
    (provider conversation tool-namespaces event-callback
     &key force-refresh turn-budget-state goal-context compaction-p)
  (:documentation
   "Perform one normalized provider attempt, optionally forcing credential refresh."))

(defmethod provider-attempt-turn
    ((provider codex-subscription-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function)
     &key
       force-refresh
       (turn-budget-state :normal)
       goal-context
       compaction-p)
  "Perform one direct request and normalize every HTTP boundary condition."
  (declare (type boolean force-refresh))
  (handler-case
      (with-credentials (credentials (provider-credential-manager provider)
                                     :force-refresh force-refresh)
        (multiple-value-bind (stream status headers)
            (provider-open-response-stream
             provider
             (provider-request-object
              provider
              conversation
              tool-namespaces
              :turn-budget-state turn-budget-state
              :goal-context goal-context
              :compaction-p compaction-p)
             credentials
             conversation)
          (provider-record-rate-limits provider headers)
          (unless (= status 200)
            (let ((body (provider--drain-error-body stream)))
              (if (= status 401)
                  (error 'provider-unauthorized
                         :message "The provider rejected the current ChatGPT credentials."
                         :status status
                         :request-id (response-header headers "x-request-id")
                         :response nil)
                  (error 'provider-error
                         :message (provider--http-error-message status body)
                         :status status
                         :request-id (response-header headers "x-request-id")
                         :response (and body
                                        (bounded-string body :limit 2000))))))
          (unwind-protect
               (provider--consume-stream stream headers event-callback)
            (when (open-stream-p stream)
              (close stream)))))
    (dexador.error:http-request-unauthorized (condition)
      (provider-signal-http-failure provider condition))
    (http-request-failed (condition)
      (provider-signal-http-failure provider condition))))

(defmethod provider-stream-turn
    ((provider codex-subscription-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function)
     &key
       (turn-budget-state :normal)
       goal-context
       compaction-p)
  "Stream one Sol turn with one credential reload and one bounded refresh attempt."
  (loop for attempt-number from 1 to 3
        for force-refresh = (= attempt-number 3)
        do (handler-case
               (return-from provider-stream-turn
                 (provider-attempt-turn provider
                                        conversation
                                        tool-namespaces
                                        event-callback
                                        :force-refresh force-refresh
                                        :turn-budget-state turn-budget-state
                                        :goal-context goal-context
                                        :compaction-p compaction-p))
             (provider-unauthorized ()
               (when (= attempt-number 3)
                 (error 'authentication-error
                        :message
                        "ChatGPT rejected Autolith's credentials after a bounded refresh.")))))
  (error 'authentication-error
         :message "ChatGPT authentication retry ended unexpectedly."))
