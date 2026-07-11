(in-package #:frob)

;;;; -- Device Authentication Constants --

(define-constant +openai-oauth-issuer+ "https://auth.openai.com"
  :test #'string=
  :documentation "The issuer for Frob-owned ChatGPT device authentication.")

(defconstant +device-authentication-timeout+ 900
  "The maximum number of seconds allowed for device authorization.")


;;;; -- Device Authentication Conditions --

(define-condition device-authentication-error (authentication-error)
  ((stage
    :initarg :stage
    :reader device-authentication-error-stage
    :type keyword
    :documentation "The device flow stage that failed.")
   (status
    :initarg :status
    :reader device-authentication-error-status
    :type (option integer)
    :documentation "The HTTP status associated with the failure, if known.")
   (code
    :initarg :code
    :reader device-authentication-error-code
    :type (option string)
    :documentation "A bounded non-secret OAuth error code, if supplied."))
  (:documentation "A safe, structured failure in ChatGPT device authentication."))


;;;; -- Device Authentication State --

(defclass device-authorization ()
  ((verification-url
    :initarg :verification-url
    :reader device-authorization-verification-url
    :type non-empty-string
    :documentation "The URL at which the user approves this authorization.")
   (user-code
    :initarg :user-code
    :reader device-authorization-user-code
    :type non-empty-string
    :documentation "The one-time code displayed to the user.")
   (device-authorization-id
    :initarg :device-authorization-id
    :reader device-authorization-id
    :type non-empty-string
    :documentation "The opaque server identifier used only while polling.")
   (poll-interval
   :initarg :poll-interval
    :reader device-authorization-poll-interval
    :type (integer 1)
    :documentation "The server-requested number of seconds between polls."))
  (:documentation "The non-credential state of one pending device authorization."))

(defclass device-authorization-code ()
  ((authorization-code
    :initarg :authorization-code
    :reader device-authorization-code-value
    :type non-empty-string
    :documentation "The short-lived OAuth authorization code.")
   (code-verifier
    :initarg :code-verifier
    :reader device-authorization-code-verifier
    :type non-empty-string
    :documentation "The PKCE verifier returned by the device service."))
  (:documentation "The short-lived result of an approved device authorization."))

(defclass device-authentication-client ()
  ((issuer
    :initarg :issuer
    :reader device-authentication-client-issuer
    :type non-empty-string
    :documentation "The OAuth issuer, without a trailing slash.")
   (client-id
    :initarg :client-id
    :reader device-authentication-client-id
    :type non-empty-string
    :documentation "The public OAuth client identifier.")
   (request-function
    :initarg :request-function
    :reader device-authentication-client-request-function
    :type function
    :documentation "The injected HTTP request function.")
   (poll-function
    :initarg :poll-function
    :reader device-authentication-client-poll-function
    :type function
    :documentation "The injected authorization polling function.")
   (sleep-function
    :initarg :sleep-function
    :reader device-authentication-client-sleep-function
    :type function
    :documentation "The injected interruptible sleep function.")
   (clock-function
    :initarg :clock-function
    :reader device-authentication-client-clock-function
    :type function
    :documentation "The injected monotonic clock function returning seconds.")
   (browser-function
    :initarg :browser-function
    :reader device-authentication-client-browser-function
    :type function
    :documentation "The injected best-effort browser opening function.")
   (poll-timeout
    :initarg :poll-timeout
    :reader device-authentication-client-poll-timeout
    :type (integer 1)
    :documentation "The maximum number of seconds spent polling."))
  (:documentation "Replaceable effects and endpoints for ChatGPT device authentication."))


;;;; -- Device Authentication Protocol --

(-> device-authentication-request-code
    (device-authentication-client)
    device-authorization)
(defgeneric device-authentication-request-code (client)
  (:documentation "Start device authentication through CLIENT and return its public code."))

(-> device-authentication-complete
    (device-authentication-client device-authorization credential-manager)
    boolean)
(defgeneric device-authentication-complete (client authorization manager)
  (:documentation
   "Complete AUTHORIZATION and publish credentials through MANAGER's private store."))

(-> device-authentication-login
    (device-authentication-client credential-manager
     &key (:stream stream) (:open-browser-p boolean))
    boolean)
(defgeneric device-authentication-login
    (client manager &key stream open-browser-p)
  (:documentation
   "Run the complete device flow, always displaying the URL and code on STREAM."))


;;;; -- Device Authentication Methods --

(defmethod device-authentication-request-code
    ((client device-authentication-client))
  "Request a fresh user code from CLIENT's configured OpenAI issuer."
  (let* ((document
           (device-authentication--json-request
            :client client
            :url (device-authentication--issuer-url
                  client
                  "/api/accounts/deviceauth/usercode")
            :content-type "application/json"
            :content (json-encode
                      (json-object
                       "client_id"
                       (device-authentication-client-id client)))
            :stage ':request-code))
         (device-authorization-id
           (json-get document "device_auth_id"))
         (user-code
           (or (json-get document "user_code")
               (json-get document "usercode")))
         (poll-interval
           (device-authentication--poll-interval
            (json-get document "interval"))))
    (unless (and (non-empty-string-p device-authorization-id)
                 (non-empty-string-p user-code))
      (device-authentication--fail
       :stage ':request-code
       :message "The device authorization response omitted required fields."))
    (make-instance 'device-authorization
                   :verification-url
                   (device-authentication--issuer-url client "/codex/device")
                   :user-code user-code
                   :device-authorization-id device-authorization-id
                   :poll-interval poll-interval)))

(defmethod device-authentication-complete
    ((client device-authentication-client)
     (authorization device-authorization)
     (manager credential-manager))
  "Poll AUTHORIZATION, exchange its code, and securely publish the result."
  (let* ((authorization-code
           (funcall (device-authentication-client-poll-function client)
                    client
                    authorization))
         (primary-source (credential-manager-primary-source manager)))
    (unless (typep authorization-code 'device-authorization-code)
      (device-authentication--fail
       :stage ':poll
       :message "The device authorization poll returned an invalid result."))
    (credential-source-save
     primary-source
     (device-authentication--exchange-code
      :client client
      :authorization-code authorization-code
      :source-path (credential-source-pathname primary-source)))
    t))

(defmethod device-authentication-login
    ((client device-authentication-client)
     (manager credential-manager)
     &key
       (stream *standard-output*)
       (open-browser-p t))
  "Run device authentication while keeping every credential off STREAM."
  (let ((authorization (device-authentication-request-code client)))
    (device-authentication-display-code authorization stream)
    (when open-browser-p
      (handler-case
          (funcall (device-authentication-client-browser-function client)
                   (device-authorization-verification-url authorization))
        (error ()
          nil)))
    (device-authentication-complete client authorization manager)))


;;;; -- Public Construction and Presentation --

(-> device-authentication-client-create
    (&key
     (:issuer string)
     (:client-id string)
     (:request-function (option function))
     (:poll-function (option function))
     (:sleep-function function)
     (:clock-function function)
     (:browser-function function)
     (:poll-timeout integer))
    device-authentication-client)
(defun device-authentication-client-create
    (&key
       (issuer +openai-oauth-issuer+)
       (client-id +openai-oauth-client-id+)
       request-function
       poll-function
       (sleep-function #'sleep)
       (clock-function #'device-authentication--monotonic-seconds)
       (browser-function #'device-authentication-open-browser)
       (poll-timeout +device-authentication-timeout+))
  "Create a device client, optionally replacing every external effect."
  (unless (and (non-empty-string-p issuer)
               (non-empty-string-p client-id)
               (plusp poll-timeout))
    (device-authentication--fail
     :stage ':configuration
     :message "Device authentication configuration is invalid."))
  (make-instance 'device-authentication-client
                 :issuer (string-right-trim '(#\/) issuer)
                 :client-id client-id
                 :request-function
                 (or request-function #'device-authentication--request)
                 :poll-function
                 (or poll-function #'device-authentication--poll-for-code)
                 :sleep-function sleep-function
                 :clock-function clock-function
                 :browser-function browser-function
                 :poll-timeout poll-timeout))

(-> device-authentication-display-code (device-authorization stream) null)
(defun device-authentication-display-code (authorization stream)
  "Display AUTHORIZATION's public URL and code on STREAM, then flush it."
  (format stream
          "~&Sign in with ChatGPT:~%  Open: ~A~%  Code: ~A~%~%The code expires in 15 minutes. Continue only if you started this login in Frob.~%"
          (device-authorization-verification-url authorization)
          (device-authorization-user-code authorization))
  (finish-output stream)
  nil)

(-> device-authentication-open-browser (string) boolean)
(defun device-authentication-open-browser (url)
  "Try to open URL with the platform browser and return whether launch succeeded."
  (handler-case
      (let ((command
              (cond
                ((uiop:os-windows-p)
                 (list "rundll32" "url.dll,FileProtocolHandler" url))
                ((uiop:os-macosx-p)
                 (list "open" url))
                (t
                 (list "xdg-open" url)))))
        (uiop:launch-program command
                             :input nil
                             :output nil
                             :error-output nil
                             :ignore-error-status t)
        t)
    (error ()
      nil)))


;;;; -- Private Device Flow --

(-> device-authentication--fail
    (&key
     (:stage keyword)
     (:message string)
     (:status (option integer))
     (:code (option string)))
    nil)
(defun device-authentication--fail
    (&key stage message status code)
  "Signal a structured device authentication failure without secret material."
  (error 'device-authentication-error
         :message message
         :stage stage
         :status status
         :code code))

(-> device-authentication--issuer-url
    (device-authentication-client string)
    string)
(defun device-authentication--issuer-url (client path)
  "Return CLIENT's issuer joined to absolute PATH."
  (concatenate 'string
               (device-authentication-client-issuer client)
               path))

(-> device-authentication--user-agent () string)
(defun device-authentication--user-agent ()
  "Return the honest Frob user agent sent to device endpoints."
  (format nil "frob/~A (~A ~A; ~A)"
          +frob-version+
          (software-type)
          (software-version)
          (machine-type)))

(-> device-authentication--request
    (&key
     (:method keyword)
     (:url string)
     (:headers list)
     (:content string))
    (values string integer t))
(defun device-authentication--request (&key method url headers content)
  "Perform one device-flow HTTP request and return body, status, and headers."
  (unless (eq method :post)
    (device-authentication--fail
     :stage ':transport
     :message "Device authentication supports only HTTP POST requests."))
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dexador:post url
                        :headers headers
                        :content content
                        :force-string t
                        :keep-alive nil
                        :connect-timeout 30
                        :read-timeout 60)
        (values body status response-headers))
    (http-request-failed (condition)
      (values (or (response-body condition) "")
              (response-status condition)
              (response-headers condition)))))

(-> device-authentication--invoke-request
    (&key
     (:client device-authentication-client)
     (:url string)
     (:headers list)
     (:content string)
     (:stage keyword))
    (values string integer t))
(defun device-authentication--invoke-request
    (&key client url headers content stage)
  "Invoke CLIENT's request effect and normalize transport failures for STAGE."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (funcall (device-authentication-client-request-function client)
                   :method ':post
                   :url url
                   :headers headers
                   :content content)
        (unless (and (stringp body) (integerp status))
          (device-authentication--fail
           :stage stage
           :message "The device authentication transport returned an invalid response."))
        (values body status response-headers))
    (device-authentication-error (condition)
      (error condition))
    (error ()
      (device-authentication--fail
       :stage stage
       :message "The device authentication transport failed."))))

(-> device-authentication--success-status-p (integer) boolean)
(defun device-authentication--success-status-p (status)
  "Return true when STATUS is an HTTP success status."
  (if (<= 200 status 299) t nil))

(-> device-authentication--json-request
    (&key
     (:client device-authentication-client)
     (:url string)
     (:content-type string)
     (:content string)
     (:stage keyword))
    json-object)
(defun device-authentication--json-request
    (&key client url content-type content stage)
  "POST CONTENT to URL and return its validated JSON object for STAGE."
  (multiple-value-bind (body status response-headers)
      (device-authentication--invoke-request
       :client client
       :url url
       :headers (list (cons "Content-Type" content-type)
                      (cons "Accept" "application/json")
                      (cons "User-Agent"
                            (device-authentication--user-agent)))
       :content content
       :stage stage)
    (declare (ignore response-headers))
    (unless (device-authentication--success-status-p status)
      (let ((code (oauth-error-code body)))
        (device-authentication--fail
         :stage stage
         :message (if (and (eq stage :request-code) (= status 404))
                      "Device authentication is unavailable for this issuer."
                      (format nil "Device authentication failed during ~A~@[ (~A)~]."
                              stage
                              code))
         :status status
         :code code)))
    (handler-case
        (let ((document (json-decode body)))
          (if (json-object-p document)
              document
              (device-authentication--fail
               :stage stage
               :message "The device authentication response was not a JSON object.")))
      (device-authentication-error (condition)
        (error condition))
      (error ()
        (device-authentication--fail
         :stage stage
         :message "The device authentication response contained invalid JSON.")))))

(-> device-authentication--poll-interval (t) integer)
(defun device-authentication--poll-interval (value)
  "Return VALUE as a positive whole-second polling interval."
  (let ((interval
          (cond
            ((integerp value)
             value)
            ((stringp value)
             (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         value)))
               (multiple-value-bind (parsed end)
                   (parse-integer trimmed :junk-allowed t)
                 (and parsed (= end (length trimmed)) parsed))))
            (t
             nil))))
    (unless (and interval (plusp interval))
      (device-authentication--fail
       :stage ':request-code
       :message "The device authorization response contained an invalid polling interval."))
    interval))

(-> device-authentication--monotonic-seconds () real)
(defun device-authentication--monotonic-seconds ()
  "Return monotonically increasing process time in seconds."
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(-> device-authentication--poll-for-code
    (device-authentication-client device-authorization)
    device-authorization-code)
(defun device-authentication--poll-for-code (client authorization)
  "Poll CLIENT until AUTHORIZATION succeeds, fails, or reaches its deadline."
  (let* ((clock (device-authentication-client-clock-function client))
         (started-at (funcall clock))
         (deadline (+ started-at
                      (device-authentication-client-poll-timeout client)))
         (url (device-authentication--issuer-url
               client
               "/api/accounts/deviceauth/token"))
         (content
           (json-encode
            (json-object
             "device_auth_id" (device-authorization-id authorization)
             "user_code" (device-authorization-user-code authorization)))))
    (loop
      (multiple-value-bind (body status response-headers)
          (device-authentication--invoke-request
           :client client
           :url url
           :headers (list (cons "Content-Type" "application/json")
                          (cons "Accept" "application/json")
                          (cons "User-Agent"
                                (device-authentication--user-agent)))
           :content content
           :stage ':poll)
        (declare (ignore response-headers))
        (cond
          ((device-authentication--success-status-p status)
           (let* ((document
                    (handler-case
                        (json-decode body)
                      (error ()
                        (device-authentication--fail
                         :stage ':poll
                         :message "The approved device response contained invalid JSON."))))
                  (authorization-code
                    (and (json-object-p document)
                         (json-get document "authorization_code")))
                  (code-verifier
                    (and (json-object-p document)
                         (json-get document "code_verifier"))))
             (unless (and (non-empty-string-p authorization-code)
                          (non-empty-string-p code-verifier))
               (device-authentication--fail
                :stage ':poll
                :message "The approved device response omitted required fields."))
             (return
               (make-instance 'device-authorization-code
                              :authorization-code authorization-code
                              :code-verifier code-verifier))))
          ((member status '(403 404))
           (let ((now (funcall clock)))
             (when (>= now deadline)
               (device-authentication--fail
                :stage ':poll
                :message "Device authentication timed out after 15 minutes."))
             (funcall (device-authentication-client-sleep-function client)
                      (min (device-authorization-poll-interval authorization)
                           (max 0 (- deadline now))))))
          (t
           (let ((code (oauth-error-code body)))
             (device-authentication--fail
              :stage ':poll
              :message (format nil "Device authorization was not completed~@[ (~A)~]."
                               code)
              :status status
              :code code))))))))

(-> device-authentication--exchange-code
    (&key
     (:client device-authentication-client)
     (:authorization-code device-authorization-code)
     (:source-path pathname))
    oauth-credentials)
(defun device-authentication--exchange-code
    (&key client authorization-code source-path)
  "Exchange AUTHORIZATION-CODE and return credentials attributed to SOURCE-PATH."
  (let* ((redirect-url
           (device-authentication--issuer-url client "/deviceauth/callback"))
         (content
           (url-encode-params
            (list
             (cons "grant_type" "authorization_code")
             (cons "code"
                   (device-authorization-code-value authorization-code))
             (cons "redirect_uri" redirect-url)
             (cons "client_id" (device-authentication-client-id client))
             (cons "code_verifier"
                   (device-authorization-code-verifier authorization-code)))))
         (document
           (device-authentication--json-request
            :client client
            :url (device-authentication--issuer-url client "/oauth/token")
            :content-type "application/x-www-form-urlencoded"
            :content content
            :stage ':exchange))
         (id-token (json-get document "id_token"))
         (access-token (json-get document "access_token"))
         (refresh-token (json-get document "refresh_token"))
         (account-id
           (or (and (stringp id-token)
                    (device-authentication--jwt-account-id id-token))
               (and (stringp access-token)
                    (device-authentication--jwt-account-id access-token)))))
    (unless (and (non-empty-string-p id-token)
                 (non-empty-string-p access-token)
                 (non-empty-string-p refresh-token)
                 (non-empty-string-p account-id))
      (device-authentication--fail
       :stage ':credentials
       :message "The OAuth exchange omitted required credential fields."))
    (make-instance 'oauth-credentials
                   :access-token access-token
                   :refresh-token refresh-token
                   :id-token id-token
                   :account-id account-id
                   :expires-at (or (jwt-expiration access-token)
                                   (jwt-expiration id-token))
                   :source-path source-path)))

(-> device-authentication--jwt-account-id (string) (option string))
(defun device-authentication--jwt-account-id (token)
  "Return the account identifier carried by TOKEN's unverified JWT payload."
  (jwt-account-id token))
