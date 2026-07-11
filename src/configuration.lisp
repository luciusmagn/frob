(in-package #:frob)

;;;; -- Defaults --

(define-constant +frob-version+ "0.1.0"
  :test #'string=
  :documentation "The user-visible Frob version.")

(define-constant +default-model+ "gpt-5.6-sol"
  :test #'string=
  :documentation "The default model requested from the subscription provider.")

(define-constant +default-reasoning-effort+ "ultra"
  :test #'string=
  :documentation "The user-visible default reasoning effort.")

(define-constant +codex-responses-endpoint+
  "https://chatgpt.com/backend-api/codex/responses"
  :test #'string=
  :documentation "The current ChatGPT Codex Responses endpoint.")

(define-constant +openai-oauth-token-endpoint+
  "https://auth.openai.com/oauth/token"
  :test #'string=
  :documentation "The OpenAI OAuth token endpoint.")

(define-constant +openai-oauth-client-id+ "app_EMoamEEZ73f0CkXaXp7hrann"
  :test #'string=
  :documentation "The public OAuth client identifier used by Codex-compatible clients.")

(defparameter +supported-reasoning-efforts+
  '("low" "medium" "high" "xhigh" "max" "ultra")
  "Reasoning effort names accepted by Frob configuration.")


;;;; -- Configuration Object --

(defclass configuration ()
  ((source-root
    :initarg :source-root
    :reader configuration-source-root
    :type pathname
    :documentation "The tracked Frob source root.")
   (working-directory
    :initarg :working-directory
    :reader configuration-working-directory
    :type pathname
    :documentation "The workspace visible to the agent and Lisp worker.")
   (data-root
    :initarg :data-root
    :reader configuration-data-root
    :type pathname
    :documentation "The root for durable user data such as conversations.")
   (state-root
    :initarg :state-root
    :reader configuration-state-root
    :type pathname
    :documentation "The root for mutable state such as credentials and journals.")
   (cache-root
    :initarg :cache-root
    :reader configuration-cache-root
    :type pathname
    :documentation "The root for replaceable caches and temporary artifacts.")
   (codex-auth-path
    :initarg :codex-auth-path
    :reader configuration-codex-auth-path
    :type pathname
    :documentation "The optional Codex OAuth bootstrap file.")
   (model
    :initarg :model
    :reader configuration-model
    :type non-empty-string
    :documentation "The provider model identifier.")
   (reasoning-effort
    :initarg :reasoning-effort
    :reader configuration-reasoning-effort
    :type non-empty-string
    :documentation "The user-visible reasoning effort.")
   (provider-endpoint
    :initarg :provider-endpoint
    :reader configuration-provider-endpoint
    :type non-empty-string
    :documentation "The streaming Responses endpoint."))
  (:documentation "Immutable paths and model choices for one Frob process."))

(-> environment-directory (string pathname) pathname)
(defun environment-directory (variable fallback)
  "Return directory VARIABLE as a pathname, or FALLBACK when it is unset."
  (let ((value (uiop:getenv variable)))
    (uiop:ensure-directory-pathname
     (if (non-empty-string-p value)
         (pathname value)
         fallback))))

(-> configuration-create (&key
                           (:source-root (option pathname))
                           (:working-directory (option pathname))
                           (:model (option string))
                           (:reasoning-effort (option string)))
    configuration)
(defun configuration-create (&key source-root working-directory model reasoning-effort)
  "Create validated runtime configuration from explicit values and the environment."
  (let* ((home (user-homedir-pathname))
         (data-home (environment-directory
                     "XDG_DATA_HOME"
                     (merge-pathnames ".local/share/" home)))
         (state-home (environment-directory
                      "XDG_STATE_HOME"
                      (merge-pathnames ".local/state/" home)))
         (cache-home (environment-directory
                      "XDG_CACHE_HOME"
                      (merge-pathnames ".cache/" home)))
         (codex-home (environment-directory
                      "CODEX_HOME"
                      (merge-pathnames ".codex/" home)))
         (environment-source-root (uiop:getenv "FROB_SOURCE_ROOT"))
         (selected-model (or model (uiop:getenv "FROB_MODEL") +default-model+))
         (selected-effort (or reasoning-effort
                              (uiop:getenv "FROB_REASONING_EFFORT")
                              +default-reasoning-effort+)))
    (unless (member selected-effort +supported-reasoning-efforts+ :test #'string=)
      (error 'configuration-error
             :message (format nil "Unsupported reasoning effort ~S." selected-effort)))
    (make-instance 'configuration
                   :source-root (uiop:ensure-directory-pathname
                                 (or source-root
                                     (and (non-empty-string-p environment-source-root)
                                          (pathname environment-source-root))
                                     (asdf:system-source-directory :frob)))
                   :working-directory (uiop:ensure-directory-pathname
                                       (or working-directory (uiop:getcwd)))
                   :data-root (merge-pathnames "frob/" data-home)
                   :state-root (merge-pathnames "frob/" state-home)
                   :cache-root (merge-pathnames "frob/" cache-home)
                   :codex-auth-path (merge-pathnames "auth.json" codex-home)
                   :model selected-model
                   :reasoning-effort selected-effort
                   :provider-endpoint (or (uiop:getenv "FROB_PROVIDER_ENDPOINT")
                                          +codex-responses-endpoint+))))

(-> configuration-ensure-directories (configuration) configuration)
(defun configuration-ensure-directories (configuration)
  "Create CONFIGURATION's private data, state, and cache directories."
  (dolist (directory (list (configuration-data-root configuration)
                           (configuration-state-root configuration)
                           (configuration-cache-root configuration)))
    (ensure-directories-exist directory))
  configuration)

(-> configuration-conversation-root (configuration) pathname)
(defun configuration-conversation-root (configuration)
  "Return the directory containing append-only conversation files."
  (merge-pathnames "conversations/" (configuration-data-root configuration)))

(-> configuration-auth-path (configuration) pathname)
(defun configuration-auth-path (configuration)
  "Return Frob's private OAuth credential pathname."
  (merge-pathnames "auth.sexp" (configuration-state-root configuration)))

(-> configuration-journal-path (configuration) pathname)
(defun configuration-journal-path (configuration)
  "Return the append-only live-mutation journal pathname."
  (merge-pathnames "mutations.sexp" (configuration-state-root configuration)))

(-> configuration-wire-effort (configuration) string)
(defun configuration-wire-effort (configuration)
  "Return the provider effort, mapping user-visible Ultra to wire-level Max."
  (if (string= (configuration-reasoning-effort configuration) "ultra")
      "max"
      (configuration-reasoning-effort configuration)))

(-> make-identifier () string)
(defun make-identifier ()
  "Return a process-independent identifier suitable for conversations and requests."
  (handler-case
      (with-open-file (stream #P"/proc/sys/kernel/random/uuid"
                              :direction :input
                              :external-format :utf-8)
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (read-line stream)))
    (error ()
      (format nil "~36R-~16,'0X"
              (get-universal-time)
              (random (ash 1 64))))))
