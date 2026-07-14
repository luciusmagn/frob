(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-provider-rate-limits () null)
(defun test-provider-rate-limits ()
  "Test rate limit header parsing into portable snapshots."
  (let ((snapshot (provider-rate-limit-snapshot
                   '(("x-codex-primary-used-percent" . "28.5")
                     ("x-codex-primary-window-minutes" . "300")
                     ("x-codex-primary-reset-at" . "1783000000")
                     ("x-codex-secondary-used-percent" . "45")
                     ("x-codex-secondary-window-minutes" . "10080")))))
    (test-assert (= (getf (getf snapshot :primary) :window-minutes) 300)
                 "primary rate limit windows parse their minutes")
    (test-assert (= (round (* 10 (getf (getf snapshot :primary)
                                       :used-percent)))
                    285)
                 "decimal used percents parse without the Lisp reader")
    (test-assert (= (getf (getf snapshot :primary) :resets-at)
                    (+ 1783000000 +unix-epoch-universal-time+))
                 "reset times convert from the POSIX epoch")
    (test-assert (= (getf (getf snapshot :secondary) :used-percent) 45)
                 "secondary rate limit windows parse")
    (test-assert (null (getf (getf snapshot :secondary) :resets-at))
                 "missing reset headers stay absent"))
  (test-assert (null (provider-rate-limit-snapshot
                      '(("content-type" . "text/event-stream"))))
               "absent rate limit headers produce no snapshot")
  (test-assert
   (search "model_not_found means this model is unavailable"
           (provider--http-error-message
            404
            "{\"error\":{\"message\":\"model_not_found means this model is unavailable\"}}"))
   "HTTP errors surface the provider's own explanation")
  (test-assert
   (search "not being served"
           (provider--http-error-message 404 nil))
   "HTTP 404 carries a human hint even without a body")
  (test-assert
   (search "rate limit"
           (provider--http-error-message 429 "plain text overload"))
   "HTTP 429 explains itself and keeps the raw body")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration))
         (condition
           (make-condition
            'http-request-failed
            :body "rate limited"
            :status 429
            :headers '(("x-request-id" . "request-429")
                       ("x-codex-primary-used-percent" . "100")
                       ("x-codex-primary-window-minutes" . "300"))
            :uri nil
            :method :post)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-error (error)
                (and (= (provider-error-status error) 429)
                     (string= (provider-error-request-id error)
                              "request-429"))))
            "HTTP 429 remains a typed provider failure")
           (test-assert
            (= (getf (getf (provider-rate-limits provider) :primary)
                       :used-percent)
               100)
            "HTTP error headers refresh the visible rate limit snapshot"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-provider-request () null)
(defun test-provider-request ()
  "Test the Sol Responses Lite request shape without network access."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "request-shape"))
                (provider (provider-create configuration))
                (schemas (json-array
                          (json-object
                           "type" "namespace"
                           "name" "test"
                           "description" "Test tools."
                           "tools" (json-array))))
                (request nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert (null (json-get request "service_tier"))
                        "requests never select a provider service tier")
           (let ((input (json-get request "input")))
             (test-assert (= (length input) 3)
                          "the provider request prefixes two developer items")
             (test-assert
              (string= (json-get (aref input 0) "type") "additional_tools")
              "additional tools are the first input item")
             (let* ((tools (coerce (json-get (aref input 0) "tools") 'list))
                    (web-search
                      (find "web_search" tools
                            :key (lambda (tool)
                                   (and (json-object-p tool)
                                        (json-get tool "type")))
                            :test #'equal)))
               (test-assert web-search
                            "cached web search rides in additional tools")
               (test-assert (eq (json-get web-search "external_web_access")
                                false)
                            "cached web search forbids live web access"))
             (test-assert
              (string= (json-get (aref input 1) "role") "developer")
              "the Autolith system prompt is the second input item")
             (test-assert (string= (json-get (aref input 2) "role") "user")
                          "conversation history follows the developer prefix"))
           (let* ((warning-request
                    (provider-request-object
                     provider conversation schemas
                     :turn-budget-state :warning))
                  (warning-input (json-get warning-request "input"))
                  (warning-message (aref warning-input 2)))
             (test-assert (= (length warning-input) 4)
                          "a late turn adds one transient budget reminder")
             (test-assert
              (search "approaching its step budget"
                      (json-get (aref (json-get warning-message "content") 0)
                                "text"))
              "the warning tells the model to finish efficiently"))
           (let* ((final-request
                    (provider-request-object
                     provider conversation schemas
                     :turn-budget-state :finalization))
                  (final-input (json-get final-request "input"))
                  (additional-tools (aref final-input 0))
                  (final-message (aref final-input 2)))
             (test-assert (zerop (length (json-get additional-tools "tools")))
                          "the final provider step disables all local and hosted tools")
             (test-assert
              (search "Tools are disabled"
                      (json-get (aref (json-get final-message "content") 0)
                                "text"))
              "the final provider step requests a text-only summary")
             (test-assert (string= (json-get (aref final-input 3) "role")
                                   "user")
                          "finalization retains the original conversation input"))
           (let* ((goal-request
                    (provider-request-object
                     provider conversation schemas
                     :goal-context "<goal_context>persist</goal_context>"))
                  (goal-input (json-get goal-request "input"))
                  (goal-message (aref goal-input 2)))
             (test-assert (= (length goal-input) 4)
                          "an active goal adds one transient developer message")
             (test-assert (string= (json-get goal-message "role") "developer")
                          "the goal context is a developer message")
             (test-assert
              (search "<goal_context>"
                      (json-get (aref (json-get goal-message "content") 0)
                                "text"))
              "the goal context rides as developer text"))
           (let ((final-goal-input
                   (json-get (provider-request-object
                              provider conversation schemas
                              :turn-budget-state :finalization
                              :goal-context "<goal_context>x</goal_context>")
                             "input")))
             (test-assert (= (length final-goal-input) 4)
                          "finalization drops the transient goal context"))
           (test-assert
            (null (provider-web-search-tool
                   (make-instance 'configuration
                                  :web-search-mode "disabled")))
            "disabled web search adds no hosted tool")
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "max")
            "the provider request maps Ultra reasoning to Max")
           (multiple-value-bind (value present-p)
               (gethash "summary" (json-get request "reasoning"))
             (declare (ignore value))
             (test-assert (not present-p)
                          "hidden traces do not request reasoning summaries"))
           (let* ((trace-provider
                    (provider-create configuration :reasoning-summaries-p t))
                  (trace-request
                    (provider-request-object
                     trace-provider conversation schemas))
                  (trace-reasoning (json-get trace-request "reasoning")))
             (test-assert
              (string= (json-get trace-reasoning "summary") "auto")
              "visible traces request the best supported reasoning summary")
             (let ((compaction-reasoning
                     (json-get
                      (provider-request-object
                       trace-provider conversation schemas :compaction-p t)
                      "reasoning")))
               (multiple-value-bind (value present-p)
                   (gethash "summary" compaction-reasoning)
                 (declare (ignore value))
                 (test-assert
                  (not present-p)
                  "side-channel compaction does not request unused summaries")))
             (let ((reconfigured
                     (provider-with-configuration trace-provider configuration)))
               (test-assert
                (provider-reasoning-summaries-p reconfigured)
                "provider reconfiguration preserves the trace preference")))
           (test-assert
            (string= (json-get (json-get request "reasoning") "context")
                     "all_turns")
            "the provider request retains reasoning across the current context")
           (test-assert (string= (json-get request "tool_choice") "auto")
                        "the provider request permits automatic tool selection")
           (test-assert (eq (json-get request "parallel_tool_calls") false)
                        "the provider request disables parallel tool calls")
           (test-assert (eq (json-get request "store") false)
                        "the provider request disables server-side storage")
           (test-assert (eq (json-get request "stream") t)
                        "the provider request enables event streaming")
           (test-assert
            (equalp (json-get request "include")
                    (json-array "reasoning.encrypted_content"))
            "the provider request retains encrypted reasoning for replay")
           (test-assert
            (string= (json-get request "prompt_cache_key") "request-shape")
            "the conversation identifier is the stable prompt cache key")
           (test-assert
            (string= (json-get (json-get request "text") "verbosity") "low")
            "the provider request asks for restrained text verbosity")
           (multiple-value-bind (value present-p)
               (gethash "instructions" request)
             (declare (ignore value))
             (test-assert (not present-p)
                          "Responses Lite omits top-level instructions"))
           (multiple-value-bind (value present-p)
               (gethash "tools" request)
             (declare (ignore value))
             (test-assert (not present-p)
                          "Responses Lite omits top-level tools")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-sse-event-string (json-object) string)
(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

(defclass test-character-input-stream
    (sb-gray:fundamental-character-input-stream)
  ((source
    :initarg :source
    :reader test-character-input-stream-source
    :type string
    :documentation "The deterministic character source.")
   (position
    :initform 0
    :accessor test-character-input-stream-position
    :type integer
    :documentation "The next source character offset."))
  (:documentation "A test stream implementing character reads but not line reads."))

(defmethod sb-gray:stream-read-char ((stream test-character-input-stream))
  "Read one character from STREAM, returning the Gray-stream EOF marker at its end."
  (let ((position (test-character-input-stream-position stream))
        (source (test-character-input-stream-source stream)))
    (if (< position (length source))
        (prog1 (char source position)
          (incf (test-character-input-stream-position stream)))
        :eof)))

(-> test-provider-stream-decoding () null)
(defun test-provider-stream-decoding ()
  "Test semantic stream decoding from a deterministic SSE fixture."
  (let* ((message-item
           (json-object
            "id" "ephemeral-item-id"
            "type" "message"
            "role" "assistant"
            "content" (json-array
                       (json-object "type" "output_text" "text" "hello"))))
         (reasoning-item
           (json-object
            "id" "ephemeral-reasoning-id"
            "type" "reasoning"
            "summary" (json-array
                       (json-object "type" "summary_text"
                                    "text" "I inspected the request.")
                       (json-object "type" "summary_text"
                                    "text" "I chose a safe response."))
            "content" (json-array
                       (json-object "type" "reasoning_text"
                                    "text" "raw private reasoning"))
            "encrypted_content" "opaque-test-ciphertext"))
         (source
           (concatenate
            'string
            (test-sse-event-string
             (json-object
              "type" "response.created"
              "response" (json-object "id" "response-1")))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 0
              "delta" "I inspected "))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 0
              "delta" "the request."))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_summary_text.delta"
              "item_id" "ephemeral-reasoning-id"
              "output_index" 0
              "summary_index" 1
              "delta" "I chose a safe response."))
            (test-sse-event-string
             (json-object
              "type" "response.reasoning_text.delta"
              "delta" "raw private reasoning"))
            (test-sse-event-string
             (json-object "type" "response.output_text.delta" "delta" "hello"))
            (test-sse-event-string
             (json-object
              "type" "response.output_item.done"
              "item" message-item))
            (test-sse-event-string
             (json-object
              "type" "response.output_item.done"
              "item" reasoning-item))
            (test-sse-event-string
             (json-object
              "type" "response.completed"
              "response" (json-object
                           "id" "response-1"
                           "end_turn" false
                           "usage" (json-object "input_tokens" 5))))))
         (events nil)
         (result
           (provider--consume-stream
            (make-instance 'test-character-input-stream :source source)
            '(("x-codex-turn-state" . "turn-state-1"))
            (lambda (event)
              (push event events)))))
    (test-assert (= (length (provider-result-output-items result)) 2)
                 "the stream retains authoritative completed items in wire order")
    (test-assert (string= (provider-result-response-id result) "response-1")
                 "the stream retains its response identifier")
    (test-assert (string= (provider-result-turn-state result) "turn-state-1")
                 "the stream retains request-local turn state")
    (test-assert (eq (provider-result-turn-completion result) :continue)
                 "the stream retains an explicit provider continuation")
    (test-assert (not (gethash "id"
                               (first (provider-result-output-items result))))
                 "completed response items discard transient server identifiers")
    (test-assert
     (string= (json-get (second (provider-result-output-items result))
                        "encrypted_content")
              "opaque-test-ciphertext")
     "completed encrypted reasoning remains available for replay")
    (let* ((reasoning-output (second (provider-result-output-items result)))
           (summary (response-item-reasoning-summary reasoning-output)))
      (test-assert
       (string= summary
                (format nil "I inspected the request.~2%I chose a safe response."))
       "completed reasoning exposes only its dedicated visible summary")
      (test-assert (not (search "raw private reasoning" summary))
                   "raw reasoning content is never folded into the summary"))
    (let* ((reasoning-events
             (reverse
              (remove-if-not (lambda (event)
                               (typep event 'reasoning-delta-event))
                             events)))
           (streamed-summary
             (format nil
                     "~{~A~}"
                     (mapcar #'reasoning-delta-event-text reasoning-events))))
      (test-assert (= (length reasoning-events) 3)
                   "only summary deltas become visible reasoning events")
      (test-assert
       (string= streamed-summary
                (format nil
                        "I inspected the request.~2%I chose a safe response."))
       "summary part boundaries match the authoritative completed text"))
    (test-assert (= (length events) 7)
                 "the stream emits safe deltas, items, and completion events"))
  nil)

(-> test-provider-stream-failures () null)
(defun test-provider-stream-failures ()
  "Test failed and truncated streams become typed provider conditions."
  (dolist (source
           (list
            (test-sse-event-string
             (json-object "type" "response.failed"
                          "response" (json-object "id" "failed-response")))
            (test-sse-event-string
             (json-object "type" "response.output_text.delta" "delta" "partial"))))
    (test-assert
     (handler-case
         (progn
           (provider--consume-stream
            (make-instance 'test-character-input-stream :source source)
            nil
            (lambda (event)
              (declare (ignore event))))
           nil)
       (provider-error ()
         t))
     "failed and unterminated SSE streams signal typed provider errors"))
  nil)

(defclass test-codex-provider (codex-subscription-provider)
  ((outcomes
    :initarg :outcomes
    :accessor test-codex-provider-outcomes
    :type list
    :documentation "The attempt outcomes returned in order.")
   (refresh-flags
    :initform nil
    :accessor test-codex-provider-refresh-flags
    :type list
    :documentation "The force-refresh values observed by attempts.")
   (turn-budget-states
    :initform nil
    :accessor test-codex-provider-turn-budget-states
    :type list
    :documentation "The turn-budget states observed by attempts."))
  (:documentation "A direct-provider test double for bounded authentication retries."))

(defmethod provider-attempt-turn
    ((provider test-codex-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function)
     &key
       force-refresh
       (turn-budget-state :normal)
       goal-context
       compaction-p)
  "Return the next scripted PROVIDER outcome and record FORCE-REFRESH."
  (declare (ignore conversation tool-namespaces event-callback goal-context
                   compaction-p))
  (push force-refresh (test-codex-provider-refresh-flags provider))
  (push turn-budget-state (test-codex-provider-turn-budget-states provider))
  (let ((outcome (pop (test-codex-provider-outcomes provider))))
    (cond
      ((typep outcome 'provider-result)
       outcome)
      ((eq outcome :unauthorized)
       (error 'provider-unauthorized
              :message "Injected unauthorized response."
              :status 401
              :request-id nil
              :response nil))
      (t
       (error "Invalid scripted provider outcome ~S." outcome)))))

(-> test-codex-provider-create (configuration list) test-codex-provider)
(defun test-codex-provider-create (configuration outcomes)
  "Return a test direct provider yielding OUTCOMES."
  (make-instance 'test-codex-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)
                 :outcomes outcomes))

(-> test-provider-authentication-retries () null)
(defun test-provider-authentication-retries ()
  "Test bounded credential reload, refresh, and final unauthorized normalization."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "provider-retry"))
         (result
           (make-instance 'provider-result
                          :response-id "retry-success"
                          :output-items nil
                          :tool-calls nil
                          :usage nil
                          :turn-state nil)))
    (unwind-protect
         (progn
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized result))))
             (test-assert
              (eq (provider-stream-turn provider conversation #() #'identity)
                  result)
              "a credential reload may satisfy the first unauthorized response")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil nil))
              "the reload retry does not rotate credentials"))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized :unauthorized result))))
             (test-assert
              (eq (provider-stream-turn provider conversation #() #'identity)
                  result)
              "one forced refresh may satisfy two unauthorized responses")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil nil t))
              "the third and final attempt forces credential refresh"))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    '(:unauthorized :unauthorized :unauthorized))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation #() #'identity)
                    nil)
                (authentication-error ()
                  t))
              "a third unauthorized response becomes a typed authentication failure")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
