(in-package #:frob)

;;;; -- Minimal Test Harness --

(defvar *test-count* 0
  "The number of assertions attempted by the current test run.")

(-> test-assert (t string) null)
(defun test-assert (value description)
  "Record one assertion and signal an error when VALUE is false."
  (incf *test-count*)
  (unless value
    (error "Test failed: ~A" description))
  nil)

(-> test-configuration () configuration)
(defun test-configuration ()
  "Return an isolated configuration rooted in a fresh temporary directory."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "frob-tests-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (source-root (asdf:system-source-directory :frob)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :data-root (merge-pathnames "data/" root)
                   :state-root (merge-pathnames "state/" root)
                   :cache-root (merge-pathnames "cache/" root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" root)
                   :model +default-model+
                   :reasoning-effort +default-reasoning-effort+
                   :provider-endpoint +codex-responses-endpoint+)))

(-> test-configuration-root (configuration) pathname)
(defun test-configuration-root (configuration)
  "Return the common temporary root containing CONFIGURATION's data directory."
  (uiop:pathname-parent-directory-pathname
   (configuration-data-root configuration)))

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration :identifier "test-turn"))
                (assistant-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text" "text" "hello")))))
           (conversation-append-user-message conversation "hi")
           (conversation-append-provider-item conversation assistant-item)
           (conversation-append-tool-result
            conversation "call-1" "lisp.eval" "42" t)
           (with-open-file (stream (conversation-pathname conversation)
                                   :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:incomplete" stream))
           (let ((loaded (conversation-load-by-id configuration "test-turn")))
             (test-assert (= (length (conversation-input-items loaded)) 3)
                          "conversation reload projects complete wire items")
             (test-assert (= (conversation-next-sequence loaded) 4)
                          "conversation reload restores its next sequence")
             (test-assert
              (string= (json-get (first (conversation-input-items loaded)) "role")
                       "user")
              "conversation reload preserves the first user message")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-authentication-store () null)
(defun test-authentication-store ()
  "Test private credential storage without exposing real authentication data."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (make-instance 'frob-credential-source
                                :pathname (configuration-auth-path configuration)))
         (credentials (make-instance 'oauth-credentials
                                     :access-token "test-access-token"
                                     :refresh-token "test-refresh-token"
                                     :id-token nil
                                     :account-id "test-account"
                                     :expires-at nil
                                     :source-path (configuration-auth-path configuration))))
    (unwind-protect
         (progn
           (credential-source-save source credentials)
           (let* ((loaded (credential-source-load source))
                  (mode (sb-posix:stat-mode
                         (sb-posix:stat
                          (namestring (configuration-auth-path configuration))))))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "test-account")
              "the private credential store round-trips its account")
             (test-assert (= (logand mode #o777) #o600)
                          "the private credential store has mode 0600")))
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
           (let ((input (json-get request "input")))
             (test-assert (= (length input) 3)
                          "the provider request prefixes two developer items")
             (test-assert
              (string= (json-get (aref input 0) "type") "additional_tools")
              "additional tools are the first input item")
             (test-assert
              (string= (json-get (aref input 1) "role") "developer")
              "the Frob system prompt is the second input item")
             (test-assert (string= (json-get (aref input 2) "role") "user")
                          "conversation history follows the developer prefix"))
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "max")
            "the provider request maps Ultra reasoning to Max")
           (multiple-value-bind (value present-p)
               (gethash "instructions" request)
             (declare (ignore value))
             (test-assert (not present-p)
                          "Responses Lite omits top-level instructions")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-sse-event-string (json-object) string)
(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

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
         (source
           (concatenate
            'string
            (test-sse-event-string
             (json-object
              "type" "response.created"
              "response" (json-object "id" "response-1")))
            (test-sse-event-string
             (json-object "type" "response.output_text.delta" "delta" "hello"))
            (test-sse-event-string
             (json-object
              "type" "response.output_item.done"
              "item" message-item))
            (test-sse-event-string
             (json-object
              "type" "response.completed"
              "response" (json-object
                           "id" "response-1"
                           "usage" (json-object "input_tokens" 5))))))
         (events nil)
         (result
           (provider--consume-stream
            (make-string-input-stream source)
            '(("x-codex-turn-state" . "turn-state-1"))
            (lambda (event)
              (push event events)))))
    (test-assert (= (length (provider-result-output-items result)) 1)
                 "the stream retains one authoritative completed item")
    (test-assert (string= (provider-result-response-id result) "response-1")
                 "the stream retains its response identifier")
    (test-assert (string= (provider-result-turn-state result) "turn-state-1")
                 "the stream retains request-local turn state")
    (test-assert (not (gethash "id"
                               (first (provider-result-output-items result))))
                 "completed response items discard transient server identifiers")
    (test-assert (= (length events) 3)
                 "the stream emits delta, item, and completion events"))
  nil)

(-> test-tool-registry () null)
(defun test-tool-registry ()
  "Test namespaced tool schema construction and total dispatch failure handling."
  (let* ((registry (make-default-tool-registry))
         (schemas (tool-registry-provider-schemas registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "tool-registry"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (unknown-call (json-object
                               "namespace" "missing"
                               "name" "operation"
                               "arguments" "{}"))
                (result (tool-registry-execute-call
                         registry unknown-call context)))
           (test-assert (= (length (tool-registry-tools registry)) 13)
                        "the default registry exposes the complete initial tool set")
           (test-assert (= (length schemas) 2)
                        "the provider schemas contain two namespaces")
           (test-assert (string= (json-get (aref schemas 0) "name") "lisp")
                        "the disposable Lisp namespace is first")
           (test-assert (= (length (json-get (aref schemas 0) "tools")) 6)
                        "the Lisp namespace exposes six worker operations")
           (test-assert (string= (json-get (aref schemas 1) "name") "self")
                        "the active-image namespace is second")
           (test-assert (= (length (json-get (aref schemas 1) "tools")) 7)
                        "the self namespace exposes seven active-image operations")
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure"))
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
  nil)

(-> run-tests () boolean)
(defun run-tests ()
  "Run Frob's dependency-free unit tests and return true on success."
  (setf *test-count* 0)
  (let ((configuration (configuration-create
                        :source-root (asdf:system-source-directory :frob)
                        :working-directory (asdf:system-source-directory :frob))))
    (test-assert (string= (configuration-model configuration) "gpt-5.6-sol")
                 "the default model is gpt-5.6-sol")
    (test-assert (string= (configuration-reasoning-effort configuration) "ultra")
                 "the default reasoning effort is ultra")
    (test-assert (string= (configuration-wire-effort configuration) "max")
                 "ultra maps to the provider max effort")
    (test-assert (= (json-get (json-object "answer" 42) "answer") 42)
                 "JSON object access preserves values")
    (test-conversation-persistence)
    (test-authentication-store)
    (test-provider-request)
    (test-provider-stream-decoding)
    (test-tool-registry)
    (test-lisp-worker-protocol))
  (format t "~&~:D Frob tests passed.~%" *test-count*)
  t)
