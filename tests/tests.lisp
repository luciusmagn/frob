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

(-> test-configuration-for-source-root (pathname) configuration)
(defun test-configuration-for-source-root (source-root)
  "Return an isolated configuration whose tracked source is SOURCE-ROOT."
  (let ((state-root (merge-pathnames ".frob-test-state/" source-root)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :data-root (merge-pathnames "data/" state-root)
                   :state-root (merge-pathnames "state/" state-root)
                   :cache-root (merge-pathnames "cache/" state-root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" state-root)
                   :model +default-model+
                   :reasoning-effort +default-reasoning-effort+
                   :provider-endpoint +codex-responses-endpoint+)))

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

(-> test-write-codex-auth
    (pathname &key (:auth-mode string) (:account-id string) (:access-token string))
    null)
(defun test-write-codex-auth (pathname &key auth-mode account-id access-token)
  "Write a synthetic Codex credential document to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string
     (json-encode
      (json-object
       "auth_mode" auth-mode
       "tokens" (json-object
                  "access_token" access-token
                  "refresh_token" "must-not-be-imported"
                  "account_id" account-id)))
     stream))
  nil)

(-> test-account-jwt (string) string)
(defun test-account-jwt (account-id)
  "Return a synthetic unsigned JWT carrying ACCOUNT-ID."
  (format nil
          "e30.~A.signature"
          (cl-base64:string-to-base64-string
           (json-encode (json-object "chatgpt_account_id" account-id))
           :uri t)))

(-> test-authentication-bootstrap-and-refresh () null)
(defun test-authentication-bootstrap-and-refresh ()
  "Test one-way Codex bootstrap import, account continuity, and refresh parsing."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (bootstrap-pathname (configuration-codex-auth-path configuration))
         (manager (credential-manager-create configuration)))
    (unwind-protect
         (progn
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "apikey"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (test-assert
            (null (credential-source-load
                   (credential-manager-bootstrap-source manager)))
            "Codex bootstrap rejects non-ChatGPT authentication modes")
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (let ((imported (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id imported) "account-a")
              "the initial ChatGPT bootstrap account is imported")
             (test-assert (null (oauth-credentials-refresh-token imported))
                          "the Codex refresh token is never imported")
             (test-assert
              (equal (oauth-credentials-source-path imported)
                     (configuration-auth-path configuration))
              "bootstrap access is copied into Frob's private store"))
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-b"
                                  :access-token "bootstrap-b")
           (let ((loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "account-a")
              "subsequent loads ignore changes to the Codex bootstrap store")
             (test-assert
              (string= (oauth-credentials-access-token loaded) "bootstrap-a")
              "Frob requests depend only on the imported private credential"))
           (test-assert
            (handler-case
                (progn
                  (credential-manager-refresh manager
                                              (credential-manager-load manager))
                  nil)
              (token-refresh-failed ()
                t))
            "non-renewable bootstrap credentials require Frob's device flow")
           (let* ((primary-source (credential-manager-primary-source manager))
                  (renewable
                    (make-instance 'oauth-credentials
                                   :access-token "old-access"
                                   :refresh-token "old-refresh"
                                   :id-token nil
                                   :account-id "account-a"
                                   :expires-at nil
                                   :source-path
                                   (credential-source-pathname primary-source)))
                  (valid
                    (oauth-refresh-response-credentials
                     manager
                     renewable
                     (json-encode
                      (json-object "access_token" "new-access"
                                   "refresh_token" "new-refresh")))))
             (test-assert
              (string= (oauth-credentials-access-token valid) "new-access")
              "a validated refresh response yields new access credentials")
             (test-assert
              (string= (oauth-credentials-account-id valid) "account-a")
              "refresh without an account claim preserves the pinned account")
             (dolist (body '("not-json" "{}"))
               (test-assert
                (handler-case
                    (progn
                      (oauth-refresh-response-credentials manager renewable body)
                      nil)
                  (token-refresh-failed ()
                    t))
                "malformed refresh success bodies become typed failures"))
             (test-assert
              (handler-case
                  (progn
                    (oauth-refresh-response-credentials
                     manager
                     renewable
                     (json-encode
                      (json-object
                       "access_token" (test-account-jwt "account-b")
                       "refresh_token" "new-refresh")))
                    nil)
                (token-refresh-failed ()
                  t))
              "refresh rejects a token that switches ChatGPT accounts")))
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
            (make-instance 'test-character-input-stream :source source)
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
           (test-assert (= (length (tool-registry-tools registry)) 16)
                        "the default registry exposes the complete initial tool set")
           (test-assert (= (length schemas) 2)
                        "the provider schemas contain two namespaces")
           (test-assert (string= (json-get (aref schemas 0) "name") "lisp")
                        "the disposable Lisp namespace is first")
           (test-assert (= (length (json-get (aref schemas 0) "tools")) 6)
                        "the Lisp namespace exposes six worker operations")
           (test-assert (string= (json-get (aref schemas 1) "name") "self")
                        "the active-image namespace is second")
           (test-assert (= (length (json-get (aref schemas 1) "tools")) 10)
                        "the self namespace exposes ten active-image operations")
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-generation-manifest () null)
(defun test-generation-manifest ()
  "Test generation publication, loading, selection, and compatibility checks."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (directory (merge-pathnames "generation-under-test/"
                                     (generation-root configuration)))
         (generation
           (make-instance 'generation
                          :identifier "generation-under-test"
                          :directory directory
                          :core-pathname (merge-pathnames "frob.core" directory)
                          :temporary-core-pathname
                          (merge-pathnames ".frob.core.tmp" directory)
                          :manifest-pathname
                          (merge-pathnames "manifest.sexp" directory)
                          :git-commit "0123456789abcdef"
                          :journal-position 27
                          :created-at 4000000000
                          :status ':pending)))
    (unwind-protect
         (progn
           (ensure-directories-exist
            (generation-temporary-core-pathname generation))
           (with-open-file (stream (generation-temporary-core-pathname generation)
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
             (write-byte 42 stream))
           (generation-publish configuration generation)
           (let ((loaded (generation-find configuration
                                          "generation-under-test")))
             (test-assert loaded
                          "a published generation appears in retained listings")
             (test-assert (generation-compatible-p loaded)
                          "a manifest from this runtime is compatible")
             (test-assert (= (generation-journal-position loaded) 27)
                          "generation manifests preserve mutation journal position")
             (test-assert
              (string= (generation-identifier
                        (generation-selected configuration))
                       "generation-under-test")
              "publication atomically selects the ready generation")))
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
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (worker (lisp-worker-create configuration)))
    (unwind-protect
         (let ((response (lisp-worker-request worker :eval '(:form "(+ 40 2)"))))
           (test-assert (eq (getf (rest response) :status) :ok)
                        "the disposable worker starts through its direct active loader")
           (test-assert (equal (getf (rest response) :values) '("42"))
                        "the launched worker completes its isolated protocol request"))
      (lisp-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

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

(-> test-durable-self-mutation () null)
(defun test-durable-self-mutation ()
  "Test checked live installation, source persistence, commit, and durable journaling."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "frob-durable-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration (test-configuration-for-source-root source-root))
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
              (test-assert
               (search "Return the durable baseline."
                       (uiop:read-file-string source-pathname))
               "active checks run before durable source replacement")
              "active checks passed")
            :source-callback
            (lambda (checked-configuration paths)
              (declare (ignore checked-configuration))
              (incf source-check-count)
              (test-assert (equal paths '("src/definitions.lisp"))
                           "source checks receive normalized explicit paths")
              (test-assert
               (search "Return the durable value."
                       (uiop:read-file-string source-pathname))
               "source checks run after durable source replacement")
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
                     "(in-package #:frob)~%~%(defun test-self-target () \"Return the durable baseline.\" 0)~%"))
           (self-git-command configuration '("init" "--quiet"))
           (self-git-command configuration '("config" "user.name" "Frob Test"))
           (self-git-command configuration
                             '("config" "user.email" "frob-test@example.invalid"))
           (self-git-command configuration '("add" "src/definitions.lisp"))
           (self-git-command configuration
                             '("commit" "--quiet" "-m" "Create baseline"))
           (let* ((conversation
                    (conversation-create configuration :identifier "durable-mutation"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
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
                        (declare (ignore checked-configuration definition-source))
                        (error "Injected active check failure."))
                      :source-callback
                      (lambda (checked-configuration paths)
                        (declare (ignore checked-configuration paths))
                        "unused"))))
                  (registry (make-default-tool-registry))
                  (persist-tool (tool-registry-find registry "self" "persist-definition"))
                  (commit-tool (tool-registry-find registry "self" "commit"))
                  (failed-persist-p
                    (handler-case
                        (progn
                          (tool-execute
                           persist-tool
                           failing-active-context
                           (json-object
                            "definition"
                            "(defun test-self-target () \"Return a rejected value.\" 13)"
                            "pathname" "src/definitions.lisp"))
                          nil)
                      (error ()
                        t)))
                  (failed-persist-restored-p (= (test-self-target) 0))
                  (failed-persist-source-unchanged-p
                    (search "Return the durable baseline."
                            (uiop:read-file-string source-pathname)))
                  (persist-result
                    (tool-execute
                     persist-tool
                     context
                     (json-object
                      "definition"
                      "(defun test-self-target () \"Return the durable value.\" 84)"
                      "pathname" "src/definitions.lisp")))
                  (mutation
                    (loop for value being the hash-values of *durable-mutations*
                          when (and
                                (string= (durable-mutation-target value)
                                         (definition-key '(defun test-self-target () 84)))
                                (eq (durable-mutation-phase value) :source-written))
                            return value)))
             (test-assert failed-persist-p
                          "a failing active check rejects durable persistence")
             (test-assert failed-persist-restored-p
                          "a rejected durable definition restores the active definition")
             (test-assert failed-persist-source-unchanged-p
              "a rejected durable definition leaves source unchanged")
             (test-assert (tool-result-success-p persist-result)
                          "durable persistence succeeds after active checks")
             (test-assert (= (test-self-target) 84)
                          "durable persistence installs the live definition")
             (test-assert (= active-check-count 1)
                          "durable persistence runs active checks exactly once")
             (test-assert mutation
                          "durable persistence retains an explicit source-written transaction")
             (let ((identifier (durable-mutation-identifier mutation)))
               (clrhash *durable-mutations*)
               (durable-mutations-load configuration)
               (setf mutation (gethash identifier *durable-mutations*))
               (test-assert
                (and mutation
                     (eq (durable-mutation-phase mutation) :source-written))
                "pending durable state reconstructs from the append-only journal"))
             (let* ((failing-source-context
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
                          (declare (ignore checked-configuration definition-source))
                          "unused")
                        :source-callback
                        (lambda (checked-configuration paths)
                          (declare (ignore checked-configuration paths))
                          (error "Injected source check failure.")))))
                    (baseline-commit
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD"))))
                    (failed-commit-p
                      (handler-case
                          (progn
                            (tool-execute
                             commit-tool
                             failing-source-context
                             (json-object
                              "title" "Reject durable test definition"
                              "paths" (json-array "src/definitions.lisp")))
                            nil)
                        (error ()
                          t)))
                    (failed-commit-left-git-p
                      (string= baseline-commit
                               (string-trim
                                '(#\Space #\Tab #\Newline #\Return)
                                (self-git-command configuration
                                                  '("rev-parse" "HEAD")))))
                    (failed-commit-left-pending-p
                      (eq (durable-mutation-phase mutation) :source-written))
                    (commit-result
                     (tool-execute
                      commit-tool
                      context
                      (json-object
                       "title" "Persist durable test definition"
                       "paths" (json-array "src/definitions.lisp")))))
               (test-assert failed-commit-p
                            "a failing clean-source check rejects self.commit")
               (test-assert failed-commit-left-git-p
                "a rejected self.commit leaves Git unchanged")
               (test-assert failed-commit-left-pending-p
                            "a rejected self.commit leaves its transaction pending")
               (test-assert (tool-result-success-p commit-result)
                            "self.commit creates the explicit checked commit")
               (test-assert (= source-check-count 1)
                            "self.commit runs clean-source checks exactly once")
               (test-assert (eq (durable-mutation-phase mutation) :durable)
                            "self.commit marks the matching transaction durable")
               (test-assert
                (string= (durable-mutation-git-commit mutation)
                         (string-trim
                          '(#\Space #\Tab #\Newline #\Return)
                          (self-git-command configuration '("rev-parse" "HEAD"))))
                "the durable journal records the exact Git commit"))))
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
    (test-assert (vectorp (json-decode "[1,2,3]"))
                 "JSON arrays have one consistent vector representation")
    (test-conversation-persistence)
    (test-authentication-store)
    (test-authentication-bootstrap-and-refresh)
    (test-provider-request)
    (test-provider-stream-decoding)
    (test-tool-registry)
    (test-lisp-worker-protocol)
    (test-self-tools)
    (test-durable-self-mutation)
    (test-generation-manifest)
    (run-device-authentication-tests)
    (run-agent-tests)
    (run-terminal-tests))
  (format t "~&~:D Frob tests passed.~%" *test-count*)
  t)
