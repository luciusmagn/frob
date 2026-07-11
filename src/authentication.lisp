(in-package #:frob)

;;;; -- OAuth Credentials --

(defconstant +unix-to-universal-time+ 2208988800
  "The number of seconds between the Unix and Common Lisp epochs.")

(defvar *credentials-in-request-scope* nil
  "True only while provider credentials are dynamically available to a request.")

(defclass oauth-credentials ()
  ((access-token
    :initarg :access-token
    :reader oauth-credentials-access-token
    :type non-empty-string
    :documentation "The bearer token used for one provider request scope.")
   (refresh-token
    :initarg :refresh-token
    :reader oauth-credentials-refresh-token
    :type (option string)
    :documentation "The rotating OAuth refresh token, if available.")
   (id-token
    :initarg :id-token
    :reader oauth-credentials-id-token
    :type (option string)
    :documentation "The OpenID token, if supplied by the OAuth server.")
   (account-id
    :initarg :account-id
    :reader oauth-credentials-account-id
    :type non-empty-string
    :documentation "The ChatGPT account routed by the provider.")
   (expires-at
    :initarg :expires-at
    :reader oauth-credentials-expires-at
    :type (option timestamp)
    :documentation "The access-token expiration in universal time, if known.")
   (source-path
    :initarg :source-path
    :reader oauth-credentials-source-path
    :type pathname
    :documentation "The file from which these request-scoped credentials came."))
  (:documentation "ChatGPT OAuth material held only inside request scope."))

(-> padded-base64url (string) string)
(defun padded-base64url (source)
  "Return SOURCE padded to a complete Base64 quartet."
  (let ((missing (mod (- 4 (mod (length source) 4)) 4)))
    (concatenate 'string source (make-string missing :initial-element #\.))))

(-> jwt-payload (string) (option json-object))
(defun jwt-payload (token)
  "Decode TOKEN's unverified JWT payload, returning NIL for malformed input."
  (handler-case
      (let* ((first-dot (position #\. token))
             (second-dot (and first-dot (position #\. token :start (1+ first-dot)))))
        (when second-dot
          (let* ((encoded (subseq token (1+ first-dot) second-dot))
                 (decoded (base64-string-to-string
                           (padded-base64url encoded)
                           :uri t))
                 (payload (json-decode decoded)))
            (and (json-object-p payload) payload))))
    (error ()
      nil)))

(-> jwt-expiration (string) (option timestamp))
(defun jwt-expiration (token)
  "Return TOKEN's unverified JWT expiration as universal time."
  (let* ((payload (jwt-payload token))
         (unix-expiration (and payload (json-get payload "exp"))))
    (when (integerp unix-expiration)
      (+ unix-expiration +unix-to-universal-time+))))

(-> jwt-account-id (string) (option string))
(defun jwt-account-id (token)
  "Return the ChatGPT account identifier carried by TOKEN, if any."
  (let ((payload (jwt-payload token)))
    (when payload
      (or (json-get payload "chatgpt_account_id")
          (let ((auth (json-get payload "https://api.openai.com/auth")))
            (and (json-object-p auth)
                 (json-get auth "chatgpt_account_id")))
          (let ((organizations (json-get payload "organizations")))
            (when (and (vectorp organizations)
                       (plusp (length organizations))
                       (json-object-p (aref organizations 0)))
              (json-get (aref organizations 0) "id")))))))

(-> credentials-needs-refresh-p (oauth-credentials &key (:window integer)) boolean)
(defun credentials-needs-refresh-p (credentials &key (window 300))
  "Return true when CREDENTIALS expire within WINDOW seconds."
  (let ((expiration (oauth-credentials-expires-at credentials)))
    (and expiration
         (<= expiration (+ (get-universal-time) window)))))


;;;; -- Credential Sources --

(defclass credential-source ()
  ((pathname
    :initarg :pathname
    :reader credential-source-pathname
    :type pathname
    :documentation "The credential file read by this source."))
  (:documentation "A replaceable source of OAuth credentials."))

(defclass frob-credential-source (credential-source)
  ()
  (:documentation "Frob's private, writable S-expression credential store."))

(defclass codex-bootstrap-credential-source (credential-source)
  ()
  (:documentation "A read-only adapter for an existing Codex auth.json file."))

(-> credential-source-load (credential-source) (option oauth-credentials))
(defgeneric credential-source-load (source)
  (:documentation "Load request-scoped credentials from SOURCE, or return NIL."))

(-> credential-source-save (credential-source oauth-credentials) oauth-credentials)
(defgeneric credential-source-save (source credentials)
  (:documentation "Atomically save CREDENTIALS to writable SOURCE."))

(-> read-portable-form (pathname) t)
(defun read-portable-form (pathname)
  "Read one portable form from PATHNAME with reader evaluation disabled."
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((*read-eval* nil))
      (read stream t nil))))

(-> read-json-file-with-retry (pathname &key (:attempts integer)) json-object)
(defun read-json-file-with-retry (pathname &key (attempts 3))
  "Read PATHNAME as JSON, retrying transient partial rewrites up to ATTEMPTS."
  (loop for attempt from 1 to attempts
        do (handler-case
               (with-open-file (stream pathname
                                       :direction :input
                                       :external-format :utf-8)
                 (let ((value (yason:parse stream)))
                   (unless (json-object-p value)
                     (error "Credential root is not a JSON object."))
                   (return value)))
             (error (condition)
               (when (= attempt attempts)
                 (error condition))
               (sleep 0.02)))))

(defmethod credential-source-load ((source frob-credential-source))
  "Load Frob's private OAuth record from SOURCE."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (let ((record (read-portable-form pathname)))
        (unless (and (listp record) (eq (first record) :oauth))
          (error 'authentication-error
                 :message (format nil "Invalid Frob credential record at ~A." pathname)))
        (let ((access-token (getf (rest record) :access-token))
              (account-id (getf (rest record) :account-id)))
          (when (and (non-empty-string-p access-token)
                     (non-empty-string-p account-id))
            (make-instance 'oauth-credentials
                           :access-token access-token
                           :refresh-token (getf (rest record) :refresh-token)
                           :id-token (getf (rest record) :id-token)
                           :account-id account-id
                           :expires-at (or (getf (rest record) :expires-at)
                                           (jwt-expiration access-token))
                           :source-path pathname)))))))

(defmethod credential-source-load ((source codex-bootstrap-credential-source))
  "Load one non-renewable ChatGPT bootstrap credential without modifying Codex."
  (let ((pathname (credential-source-pathname source)))
    (when (probe-file pathname)
      (handler-case
          (let* ((document (read-json-file-with-retry pathname))
                 (auth-mode (json-get document "auth_mode"))
                 (tokens (json-get document "tokens"))
                 (access-token (and (json-object-p tokens)
                                    (json-get tokens "access_token")))
                 (id-token (and (json-object-p tokens)
                                (json-get tokens "id_token")))
                 (account-id (and (json-object-p tokens)
                                  (or (json-get tokens "account_id")
                                      (and id-token (jwt-account-id id-token))
                                      (and access-token (jwt-account-id access-token))))))
            (when (and (stringp auth-mode)
                       (string-equal auth-mode "chatgpt")
                       (non-empty-string-p access-token)
                       (non-empty-string-p account-id))
              (make-instance 'oauth-credentials
                             :access-token access-token
                             :refresh-token nil
                             :id-token nil
                             :account-id account-id
                             :expires-at (jwt-expiration access-token)
                             :source-path pathname)))
        (error ()
          nil)))))

(defmethod credential-source-save ((source frob-credential-source)
                                   (credentials oauth-credentials))
  "Atomically save CREDENTIALS to Frob's private store with mode 0600."
  (let* ((pathname (credential-source-pathname source))
         (directory (uiop:pathname-directory-pathname pathname))
         (temporary (merge-pathnames
                     (format nil ".auth.~D.tmp" (sb-posix:getpid))
                     directory))
         (record (list :oauth
                       :version 1
                       :access-token (oauth-credentials-access-token credentials)
                       :refresh-token (oauth-credentials-refresh-token credentials)
                       :id-token (oauth-credentials-id-token credentials)
                       :account-id (oauth-credentials-account-id credentials)
                       :expires-at (oauth-credentials-expires-at credentials))))
    (ensure-directories-exist pathname)
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-circle* t)
            (*print-readably* t))
        (prin1 record stream)
        (terpri stream)
        (finish-output stream)))
    (sb-posix:chmod (namestring temporary) #o600)
    (uiop:rename-file-overwriting-target temporary pathname)
    credentials))

(defmethod credential-source-save ((source codex-bootstrap-credential-source)
                                   (credentials oauth-credentials))
  "Reject writes to the Codex bootstrap source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message (format nil "The Codex bootstrap store ~A is read-only."
                          (credential-source-pathname source))))


;;;; -- Credential Manager --

(defclass credential-manager ()
  ((primary-source
    :initarg :primary-source
    :reader credential-manager-primary-source
    :type frob-credential-source
    :documentation "Frob's writable credential source.")
   (bootstrap-source
    :initarg :bootstrap-source
    :reader credential-manager-bootstrap-source
    :type codex-bootstrap-credential-source
    :documentation "The optional read-only Codex bootstrap source.")
   (refresh-lock
    :initform (make-lock "Frob OAuth refresh")
    :reader credential-manager-refresh-lock
    :documentation "The in-process serialization lock for token rotation.")
   (account-id
    :initform nil
    :accessor credential-manager-account-id
    :type (option string)
    :documentation "The account identity pinned for this manager's lifetime."))
  (:documentation "Credential paths and refresh policy without retained tokens."))

(-> credential-manager-create (configuration) credential-manager)
(defun credential-manager-create (configuration)
  "Create a credential manager using CONFIGURATION's private and bootstrap paths."
  (make-instance 'credential-manager
                 :primary-source (make-instance
                                  'frob-credential-source
                                  :pathname (configuration-auth-path configuration))
                 :bootstrap-source (make-instance
                                    'codex-bootstrap-credential-source
                                    :pathname (configuration-codex-auth-path configuration))))

(-> credential-manager-accept-account
    (credential-manager oauth-credentials &key (:allow-change boolean))
    oauth-credentials)
(defun credential-manager-accept-account (manager credentials &key allow-change)
  "Pin CREDENTIALS' account in MANAGER, rejecting an unexplained account change."
  (let ((expected (credential-manager-account-id manager))
        (actual (oauth-credentials-account-id credentials)))
    (when (and expected
               (not allow-change)
               (not (string= expected actual)))
      (error 'authentication-error
             :message "The ChatGPT credential account changed during this Frob session."))
    (setf (credential-manager-account-id manager) actual)
    credentials))

(-> credential-manager-import-bootstrap
    (credential-manager oauth-credentials)
    oauth-credentials)
(defun credential-manager-import-bootstrap (manager bootstrap)
  "Copy BOOTSTRAP's bounded access token once into Frob's private store."
  (when (credentials-needs-refresh-p bootstrap :window 0)
    (error 'credentials-unavailable
           :message "The Codex bootstrap access token expired; run frob --auth."
           :searched-paths
           (list (credential-source-pathname
                  (credential-manager-bootstrap-source manager)))))
  (let* ((primary-source (credential-manager-primary-source manager))
         (imported
           (make-instance 'oauth-credentials
                          :access-token
                          (oauth-credentials-access-token bootstrap)
                          :refresh-token nil
                          :id-token nil
                          :account-id
                          (oauth-credentials-account-id bootstrap)
                          :expires-at
                          (oauth-credentials-expires-at bootstrap)
                          :source-path
                          (credential-source-pathname primary-source))))
    (credential-manager-accept-account manager imported)
    (credential-source-save primary-source imported)))

(-> credential-manager-load (credential-manager) oauth-credentials)
(defun credential-manager-load (manager)
  "Load only Frob-owned credentials, importing Codex once when no store exists."
  (let ((primary (credential-source-load
                  (credential-manager-primary-source manager))))
    (cond
      (primary
       (credential-manager-accept-account manager primary))
      (t
       (let ((bootstrap
               (credential-source-load
                (credential-manager-bootstrap-source manager))))
         (if bootstrap
             (credential-manager-import-bootstrap manager bootstrap)
             (error 'credentials-unavailable
                    :message
                    "No ChatGPT OAuth credentials are available; run frob --auth."
                    :searched-paths
                    (list (credential-source-pathname
                           (credential-manager-primary-source manager))
                          (credential-source-pathname
                           (credential-manager-bootstrap-source manager))))))))))

(-> oauth-error-code (t) (option string))
(defun oauth-error-code (body)
  "Extract a non-secret OAuth error code from BODY, if possible."
  (handler-case
      (let ((document (and (stringp body) (json-decode body))))
        (when (json-object-p document)
          (let ((error-value (json-get document "error")))
            (cond
              ((stringp error-value)
               error-value)
              ((json-object-p error-value)
               (or (json-get error-value "code")
                   (json-get error-value "type")))))))
    (error ()
      nil)))

(-> oauth-refresh-response-credentials
    (credential-manager oauth-credentials string)
    oauth-credentials)
(defun oauth-refresh-response-credentials (manager credentials body)
  "Validate refresh BODY and return account-continuous Frob credentials."
  (handler-case
      (let ((response (json-decode body)))
        (unless (json-object-p response)
          (error "The OAuth refresh root is not an object."))
        (let* ((access-token (json-get response "access_token"))
               (response-id-token (json-get response "id_token"))
               (id-token (or response-id-token
                             (oauth-credentials-id-token credentials)))
               (rotated-refresh-token
                 (or (json-get response "refresh_token")
                     (oauth-credentials-refresh-token credentials))))
          (unless (and (non-empty-string-p access-token)
                       (or (null response-id-token)
                           (non-empty-string-p response-id-token))
                       (non-empty-string-p rotated-refresh-token))
            (error "The OAuth refresh response omitted required fields."))
          (let* ((previous-account
                   (oauth-credentials-account-id credentials))
                 (token-account
                   (or (and id-token (jwt-account-id id-token))
                       (jwt-account-id access-token)))
                 (account-id (or token-account previous-account)))
            (when (and token-account
                       (not (string= token-account previous-account)))
              (error 'token-refresh-failed
                     :message "The OAuth refresh response changed ChatGPT accounts."
                     :status nil
                     :response nil))
            (make-instance
             'oauth-credentials
             :access-token access-token
             :refresh-token rotated-refresh-token
             :id-token id-token
             :account-id account-id
             :expires-at (jwt-expiration access-token)
             :source-path
             (credential-source-pathname
              (credential-manager-primary-source manager))))))
    (token-refresh-failed (condition)
      (error condition))
    (error ()
      (error 'token-refresh-failed
             :message "The OAuth refresh response was malformed."
             :status nil
             :response nil))))

(-> credential-manager-refresh (credential-manager oauth-credentials) oauth-credentials)
(defun credential-manager-refresh (manager stale-credentials)
  "Refresh STALE-CREDENTIALS, publishing rotated tokens only to Frob's store."
  (with-lock-held ((credential-manager-refresh-lock manager))
    (let* ((primary-source (credential-manager-primary-source manager))
           (bootstrap-pathname
             (credential-source-pathname
              (credential-manager-bootstrap-source manager)))
           (latest (credential-source-load primary-source))
           (credentials
             (if (and latest
                      (not (string= (oauth-credentials-access-token latest)
                                    (oauth-credentials-access-token stale-credentials))))
                 (credential-manager-accept-account manager latest)
                 stale-credentials))
           (refresh-token (oauth-credentials-refresh-token credentials)))
      (when (equal (oauth-credentials-source-path credentials)
                   bootstrap-pathname)
        (error 'token-refresh-failed
               :message "Codex bootstrap credentials are never refreshed by Frob."
               :status nil
               :response nil))
      (when (and latest
                 (not (string= (oauth-credentials-access-token latest)
                               (oauth-credentials-access-token stale-credentials)))
                 (not (credentials-needs-refresh-p latest)))
        (return-from credential-manager-refresh credentials))
      (unless (non-empty-string-p refresh-token)
        (error 'token-refresh-failed
               :message "These credentials cannot refresh; run frob --auth."
               :status nil
               :response nil))
      (handler-case
          (let* ((request (json-object
                           "client_id" +openai-oauth-client-id+
                           "grant_type" "refresh_token"
                           "refresh_token" refresh-token))
                 (body (dexador:post
                        +openai-oauth-token-endpoint+
                        :headers '(("Content-Type" . "application/json")
                                   ("Accept" . "application/json"))
                        :content (json-encode request)
                        :force-string t
                        :connect-timeout 30
                        :read-timeout 60))
                 (refreshed
                   (oauth-refresh-response-credentials
                    manager credentials body)))
            (credential-manager-accept-account manager refreshed)
            (credential-source-save primary-source refreshed))
        (http-request-failed (condition)
          (let* ((body (response-body condition))
                 (code (oauth-error-code body))
                 (newer-primary
                   (and (string= (or code "") "refresh_token_reused")
                        (credential-source-load primary-source))))
            (if (and newer-primary
                     (not (string= (oauth-credentials-access-token newer-primary)
                                   (oauth-credentials-access-token credentials))))
                (credential-manager-accept-account manager newer-primary)
                (error 'token-refresh-failed
                       :message (format nil "OAuth token refresh failed~@[ (~A)~]." code)
                       :status (response-status condition)
                       :response code))))
        (authentication-error (condition)
          (error condition))
        (error ()
          (error 'token-refresh-failed
                 :message "OAuth token refresh could not be completed."
                 :status nil
                 :response nil))))))

(-> credential-manager-credentials
    (credential-manager &key (:force-refresh boolean))
    oauth-credentials)
(defun credential-manager-credentials (manager &key force-refresh)
  "Load credentials and refresh them when expired or FORCE-REFRESH is true."
  (let ((credentials (credential-manager-load manager)))
    (if (or force-refresh (credentials-needs-refresh-p credentials))
        (credential-manager-refresh manager credentials)
        credentials)))

(-> call-with-credentials
    (credential-manager function &key (:force-refresh boolean))
    t)
(defun call-with-credentials (manager function &key force-refresh)
  "Call FUNCTION with request-scoped credentials managed by MANAGER."
  (let ((*credentials-in-request-scope* t))
    (funcall function
             (credential-manager-credentials
              manager
              :force-refresh force-refresh))))

(defmacro with-credentials ((variable manager &key force-refresh) &body body)
  "Bind VARIABLE to request-scoped credentials from MANAGER while evaluating BODY."
  `(call-with-credentials ,manager
                          (lambda (,variable)
                            ,@body)
                          :force-refresh ,force-refresh))
