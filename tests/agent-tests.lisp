(in-package #:frob)

;;;; -- Scripted Agent Boundary --

(defclass scripted-provider (model-provider)
  ((results
    :initarg :results
    :accessor scripted-provider-results
    :type list
    :documentation "The provider results returned in request order.")
   (input-counts
    :initform nil
    :accessor scripted-provider-input-counts
    :type list
    :documentation "Conversation input lengths observed before each request.")
   (turn-states
    :initform nil
    :accessor scripted-provider-turn-states
    :type list
    :documentation "Request-local turn states observed before each request."))
  (:documentation "A deterministic provider for exercising repeated agent rounds."))

(defmethod provider-stream-turn
    ((provider scripted-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function))
  "Return PROVIDER's next scripted result after recording request state."
  (declare (ignore tool-namespaces))
  (push (length (conversation-input-items conversation))
        (scripted-provider-input-counts provider))
  (push (conversation-turn-state conversation)
        (scripted-provider-turn-states provider))
  (let ((result (pop (scripted-provider-results provider))))
    (unless result
      (error "The scripted provider has no remaining result."))
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "delta"))
    result))

(defclass agent-test-echo-tool (tool)
  ()
  (:documentation "Return one required string to the scripted agent provider."))

(defmethod tool-execute ((tool agent-test-echo-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the required test value without external effects."
  (declare (ignore tool context))
  (tool-success
   (format nil "echo: ~A"
           (tool-argument arguments "value" :required t))))

(-> agent-test-result
    (string list &key (:turn-state (option string)))
    provider-result)
(defun agent-test-result (response-id output-items &key turn-state)
  "Return a scripted provider result containing OUTPUT-ITEMS."
  (make-instance 'provider-result
                 :response-id response-id
                 :output-items output-items
                 :tool-calls (remove-if-not #'function-call-item-p output-items)
                 :usage (json-object "input_tokens" 1 "output_tokens" 1)
                 :turn-state turn-state))

(-> test-agent-tool-loop () null)
(defun test-agent-tool-loop ()
  "Test authoritative replay, correlated tool output, callbacks, and turn-state scope."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "agent-loop"))
         (call (json-object
                "type" "function_call"
                "call_id" "call-1"
                "namespace" "test"
                "name" "echo"
                "arguments" "{\"value\":\"hello\"}"))
         (message (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object
                               "type" "output_text"
                               "text" "complete"))))
         (provider
           (make-instance
            'scripted-provider
            :results (list (agent-test-result "response-1"
                                              (list call)
                                              :turn-state "turn-state-1")
                           (agent-test-result "response-2" (list message)))))
         (registry (make-instance 'tool-registry))
         (deltas nil)
         (statuses nil))
    (unwind-protect
         (progn
           (tool-registry-register
            registry
            (make-instance
             'agent-test-echo-tool
             :namespace "test"
             :name "echo"
             :description "Echo a test string."
             :parameters
             (tool-object-schema
              (json-object
               "value" (tool-string-property "The value to echo."))
              '("value"))))
           (let* ((agent (agent-create
                          :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry registry
                          :worker ':unused
                          :maximum-tool-rounds 2))
                  (observer
                    (callback-agent-observer-create
                     :text-callback (lambda (text)
                                      (push text deltas))
                     :status-callback (lambda (status details)
                                        (declare (ignore details))
                                        (push status statuses))))
                  (result (agent-run-user-turn agent "run the echo" :observer observer)))
             (test-assert (string= (provider-result-response-id result) "response-2")
                          "the agent returns the final tool-free provider result")
             (test-assert (equal (nreverse (scripted-provider-input-counts provider))
                                 '(1 3))
                          "the second request replays the call and its correlated output")
             (test-assert (equal (nreverse (scripted-provider-turn-states provider))
                                 '(nil "turn-state-1"))
                          "provider turn state is replayed only inside the active turn")
             (test-assert (null (conversation-turn-state conversation))
                          "the agent clears request-local turn state after completion")
             (test-assert (= (length (conversation-input-items conversation)) 4)
                          "conversation history contains user, call, output, and answer")
             (test-assert (equal (nreverse deltas) '("delta" "delta"))
                          "the observer receives deltas from every provider request")
             (test-assert (member :tool-call-completed statuses)
                          "the observer receives correlated tool lifecycle status")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-agent-tests () boolean)
(defun run-agent-tests ()
  "Run focused agent-loop tests and return true on success."
  (test-agent-tool-loop)
  t)
