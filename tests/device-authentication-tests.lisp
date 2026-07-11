(in-package #:frob)

;;;; -- Device Authentication Test Support --

(defvar *device-authentication-test-saved-credentials* nil
  "The credentials observed by the recording test store.")

(defclass recording-frob-credential-source (frob-credential-source)
  ()
  (:documentation "A Frob credential source that records rather than writes test data."))

(defmethod credential-source-save
    ((source recording-frob-credential-source)
     (credentials oauth-credentials))
  "Record CREDENTIALS without touching SOURCE's pathname."
  (declare (ignore source))
  (setf *device-authentication-test-saved-credentials* credentials)
  credentials)

(-> device-authentication-test--manager () credential-manager)
(defun device-authentication-test--manager ()
  "Return a credential manager whose writable source records test credentials."
  (make-instance
   'credential-manager
   :primary-source
   (make-instance 'recording-frob-credential-source
                  :pathname #P"/tmp/frob-device-authentication/auth.sexp")
   :bootstrap-source
   (make-instance 'codex-bootstrap-credential-source
                  :pathname #P"/tmp/frob-device-authentication/codex-auth.json")))

(-> device-authentication-test--base64url (string) string)
(defun device-authentication-test--base64url (source)
  "Return SOURCE encoded as unpadded RFC 4648 Base64url text."
  (string-right-trim
   '(#\=)
   (substitute #\_
               #\/
               (substitute #\-
                           #\+
                           (cl-base64:string-to-base64-string source)))))

(-> device-authentication-test--jwt (json-object) string)
(defun device-authentication-test--jwt (payload)
  "Return an unsigned test JWT containing PAYLOAD."
  (format nil "~A.~A.signature"
          (device-authentication-test--base64url "{\"alg\":\"none\"}")
          (device-authentication-test--base64url (json-encode payload))))

(-> device-authentication-test--request
    (list string)
    list)
(defun device-authentication-test--request (requests suffix)
  "Return the first recorded request whose URL ends in SUFFIX."
  (find-if (lambda (request)
             (let ((url (getf request :url)))
               (and (>= (length url) (length suffix))
                    (string= url
                             suffix
                             :start1 (- (length url) (length suffix))))))
           requests))

(-> device-authentication-test--signals
    (function keyword &key (:status (option integer)) (:code (option string)))
    null)
(defun device-authentication-test--signals
    (function stage &key status code)
  "Assert that FUNCTION signals a safe device error for STAGE."
  (let ((signaled-p nil))
    (handler-case
        (funcall function)
      (device-authentication-error (condition)
        (setf signaled-p t)
        (test-assert (eq (device-authentication-error-stage condition) stage)
                     "the device error reports the failed stage")
        (test-assert (eql (device-authentication-error-status condition) status)
                     "the device error reports only the expected status")
        (test-assert (equal (device-authentication-error-code condition) code)
                     "the device error reports only the expected OAuth code")))
    (test-assert signaled-p "the device operation signals its expected condition")
    nil))


;;;; -- Device Authentication Tests --

(-> device-authentication-test--complete-flow () null)
(defun device-authentication-test--complete-flow ()
  "Exercise request, pending poll, exchange, display, and secure publication."
  (let* ((account-id "account-test-123")
         (id-token
           (device-authentication-test--jwt
            (json-object
             "https://api.openai.com/auth"
             (json-object "chatgpt_account_id" account-id))))
         (requests nil)
         (poll-count 0)
         (clock 0)
         (sleeps nil)
         (opened-url nil)
         (*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (push (list :method method
                         :url url
                         :headers headers
                         :content content)
                   requests)
             (cond
               ((device-authentication-test--url-suffix-p
                 url
                 "/api/accounts/deviceauth/usercode")
                (values
                 (json-encode
                  (json-object
                   "device_auth_id" "device-test-123"
                   "user_code" "TEST-CODE"
                   "interval" "2"))
                 200
                 nil))
               ((device-authentication-test--url-suffix-p
                 url
                 "/api/accounts/deviceauth/token")
                (incf poll-count)
                (if (= poll-count 1)
                    (values "{}" 404 nil)
                    (values
                     (json-encode
                      (json-object
                       "authorization_code" "authorization-test-123"
                       "code_challenge" "challenge-test-123"
                       "code_verifier" "verifier-test-123"))
                     200
                     nil)))
               ((device-authentication-test--url-suffix-p url "/oauth/token")
                (values
                 (json-encode
                  (json-object
                   "id_token" id-token
                   "access_token" "access-test-123"
                   "refresh_token" "refresh-test-123"))
                 200
                 nil))
               (t
                (error "Unexpected test URL."))))

           (pause (seconds)
             (push seconds sleeps)
             (incf clock seconds))

           (now ()
             clock)

           (open-browser (url)
             (setf opened-url url)
             t))
      (let* ((client
               (device-authentication-client-create
                :issuer "https://issuer.test/"
                :request-function #'request
                :sleep-function #'pause
                :clock-function #'now
                :browser-function #'open-browser))
             (manager (device-authentication-test--manager))
             (output
               (with-output-to-string (stream)
                 (test-assert
                  (device-authentication-login client manager :stream stream)
                  "the complete device flow succeeds")))
             (ordered-requests (nreverse requests))
             (code-request
               (device-authentication-test--request
                ordered-requests
                "/api/accounts/deviceauth/usercode"))
             (exchange-request
               (device-authentication-test--request
                ordered-requests
                "/oauth/token"))
             (saved *device-authentication-test-saved-credentials*))
        (test-assert (= poll-count 2)
                     "pending authorization is polled until approved")
        (test-assert (equal sleeps '(2))
                     "the server polling interval is honored")
        (test-assert
         (string= opened-url "https://issuer.test/codex/device")
         "the configured browser receives the verification URL")
        (test-assert (search "https://issuer.test/codex/device" output)
                     "the verification URL is always displayed")
        (test-assert (search "TEST-CODE" output)
                     "the one-time user code is always displayed")
        (dolist (secret (list id-token
                              "access-test-123"
                              "refresh-test-123"
                              "authorization-test-123"
                              "verifier-test-123"))
          (test-assert (null (search secret output))
                       "credential material is never displayed"))
        (test-assert
         (string=
          (json-get (json-decode (getf code-request :content)) "client_id")
          +openai-oauth-client-id+)
         "the current public OAuth client identifier is sent")
        (test-assert
         (string-equal
          (rest (assoc "Content-Type"
                       (getf exchange-request :headers)
                       :test #'string-equal))
          "application/x-www-form-urlencoded")
         "the code exchange uses form encoding")
        (test-assert
         (and (search "grant_type=authorization_code"
                      (getf exchange-request :content))
              (search "code=authorization-test-123"
                      (getf exchange-request :content))
              (search "code_verifier=verifier-test-123"
                      (getf exchange-request :content))
              (search "redirect_uri=https%3A%2F%2Fissuer.test%2Fdeviceauth%2Fcallback"
                      (getf exchange-request :content)))
         "the code exchange contains the exact device grant fields")
        (test-assert (typep saved 'oauth-credentials)
                     "credentials are published through Frob's store protocol")
        (test-assert
         (string= (oauth-credentials-account-id saved) account-id)
         "the nested ChatGPT account identifier is extracted")
        (test-assert
         (string= (oauth-credentials-access-token saved) "access-test-123")
         "the exchanged access token reaches only the credential store")
        (test-assert
         (equal (oauth-credentials-source-path saved)
                #P"/tmp/frob-device-authentication/auth.sexp")
         "saved credentials are attributed to Frob's private store")))
    nil))

(-> device-authentication-test--injected-poll () null)
(defun device-authentication-test--injected-poll ()
  "Verify high-level authentication accepts an injected polling effect."
  (let* ((account-id "account-from-access")
         (access-token
           (device-authentication-test--jwt
            (json-object "chatgpt_account_id" account-id)))
         (poll-calls 0)
         (*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (declare (ignore method headers content))
             (cond
               ((device-authentication-test--url-suffix-p
                 url
                 "/api/accounts/deviceauth/usercode")
                (values
                 (json-encode
                  (json-object
                   "device_auth_id" "device-injected"
                   "user_code" "INJECTED-CODE"
                   "interval" "7"))
                 200
                 nil))
               ((device-authentication-test--url-suffix-p url "/oauth/token")
                (values
                 (json-encode
                  (json-object
                   "id_token"
                   (device-authentication-test--jwt (json-object))
                   "access_token" access-token
                   "refresh_token" "refresh-injected"))
                 200
                 nil))
               (t
                (error "The injected poll should avoid the poll endpoint."))))

           (poll (client authorization)
             (declare (ignore client))
             (incf poll-calls)
             (test-assert
              (string= (device-authorization-user-code authorization)
                       "INJECTED-CODE")
              "the injected poll receives the requested authorization")
             (make-instance 'device-authorization-code
                            :authorization-code "authorization-injected"
                            :code-verifier "verifier-injected"))

           (unexpected-sleep (seconds)
             (declare (ignore seconds))
             (error "The injected poll must not sleep.")))
      (let ((client
              (device-authentication-client-create
               :issuer "https://issuer.test"
               :request-function #'request
               :poll-function #'poll
               :sleep-function #'unexpected-sleep)))
        (with-output-to-string (stream)
          (device-authentication-login
           client
           (device-authentication-test--manager)
           :stream stream
           :open-browser-p nil))
        (test-assert (= poll-calls 1)
                     "the injected polling function is called exactly once")
        (test-assert
         (string= (oauth-credentials-account-id
                   *device-authentication-test-saved-credentials*)
                  account-id)
         "account extraction falls back from the ID token to the access token")))
    nil))

(-> device-authentication-test--timeout () null)
(defun device-authentication-test--timeout ()
  "Verify pending responses stop at the configured polling deadline."
  (let ((clock 0)
        (poll-count 0)
        (*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (declare (ignore method headers content))
             (if (device-authentication-test--url-suffix-p
                  url
                  "/api/accounts/deviceauth/usercode")
                 (values
                  (json-encode
                   (json-object
                    "device_auth_id" "device-timeout"
                    "user_code" "TIMEOUT-CODE"
                    "interval" "5"))
                  200
                  nil)
                 (progn
                   (incf poll-count)
                   (values "{}" 403 nil))))

           (pause (seconds)
             (incf clock seconds))

           (now ()
             clock))
      (let* ((client
               (device-authentication-client-create
                :issuer "https://issuer.test"
                :request-function #'request
                :sleep-function #'pause
                :clock-function #'now
                :poll-timeout 10))
             (authorization (device-authentication-request-code client)))
        (device-authentication-test--signals
         (lambda ()
           (device-authentication-complete
            client
            authorization
            (device-authentication-test--manager)))
         ':poll)
        (test-assert (= poll-count 3)
                     "pending authorization stops at its deadline")
        (test-assert (null *device-authentication-test-saved-credentials*)
                     "timed-out authentication publishes no credentials")))
    nil))

(-> device-authentication-test--declined () null)
(defun device-authentication-test--declined ()
  "Verify a declined authorization exposes only its safe OAuth code."
  (let ((*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (declare (ignore method headers content))
             (if (device-authentication-test--url-suffix-p
                  url
                  "/api/accounts/deviceauth/usercode")
                 (values
                  (json-encode
                   (json-object
                    "device_auth_id" "device-declined"
                    "user_code" "DECLINED-CODE"
                    "interval" "1"))
                  200
                  nil)
                 (values
                  (json-encode
                   (json-object
                    "error" "authorization_declined"
                    "error_description" "A sensitive provider explanation"))
                  401
                  nil))))
      (let* ((client
               (device-authentication-client-create
                :issuer "https://issuer.test"
                :request-function #'request))
             (authorization (device-authentication-request-code client)))
        (device-authentication-test--signals
         (lambda ()
           (device-authentication-complete
            client
            authorization
            (device-authentication-test--manager)))
         ':poll
         :status 401
         :code "authorization_declined")
        (test-assert (null *device-authentication-test-saved-credentials*)
                     "declined authentication publishes no credentials")))
    nil))

(-> device-authentication-test--missing-account () null)
(defun device-authentication-test--missing-account ()
  "Verify token exchange without an account identifier is never published."
  (let ((*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (declare (ignore method headers content))
             (cond
               ((device-authentication-test--url-suffix-p
                 url
                 "/api/accounts/deviceauth/usercode")
                (values
                 (json-encode
                  (json-object
                   "device_auth_id" "device-no-account"
                   "user_code" "NO-ACCOUNT-CODE"
                   "interval" "1"))
                 200
                 nil))
               ((device-authentication-test--url-suffix-p
                 url
                 "/api/accounts/deviceauth/token")
                (values
                 (json-encode
                  (json-object
                   "authorization_code" "authorization-no-account"
                   "code_verifier" "verifier-no-account"))
                 200
                 nil))
               (t
                (values
                 (json-encode
                  (json-object
                   "id_token"
                   (device-authentication-test--jwt (json-object))
                   "access_token"
                   (device-authentication-test--jwt (json-object))
                   "refresh_token" "refresh-no-account"))
                 200
                 nil)))))
      (let* ((client
               (device-authentication-client-create
                :issuer "https://issuer.test"
                :request-function #'request))
             (authorization (device-authentication-request-code client)))
        (device-authentication-test--signals
         (lambda ()
           (device-authentication-complete
            client
            authorization
            (device-authentication-test--manager)))
         ':credentials)
        (test-assert (null *device-authentication-test-saved-credentials*)
                     "an incomplete token exchange is never published")))
    nil))

(-> device-authentication-test--url-suffix-p (string string) boolean)
(defun device-authentication-test--url-suffix-p (url suffix)
  "Return true when URL ends with SUFFIX."
  (and (>= (length url) (length suffix))
       (if (string= url suffix :start1 (- (length url) (length suffix)))
           t
           nil)))

(-> run-device-authentication-tests () boolean)
(defun run-device-authentication-tests ()
  "Run the offline ChatGPT device authentication tests."
  (device-authentication-test--complete-flow)
  (device-authentication-test--injected-poll)
  (device-authentication-test--timeout)
  (device-authentication-test--declined)
  (device-authentication-test--missing-account)
  t)
