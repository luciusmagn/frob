(in-package #:autolith)

;;;; -- Active Application --

(defclass application ()
  ((configuration
    :initarg :configuration
    :accessor application-configuration
    :type configuration
    :documentation "The current paths, model, and provider choices.")
   (conversation
    :initarg :conversation
    :accessor application-conversation
    :type conversation
    :documentation "The durable conversation currently shown to the user.")
   (provider
    :initarg :provider
    :accessor application-provider
    :type (option model-provider)
    :documentation "The reconnectable model provider.")
   (tool-registry
    :initarg :tool-registry
    :accessor application-tool-registry
    :type tool-registry
    :documentation "The live, checkpointed tool registry.")
   (worker
    :initarg :worker
    :accessor application-worker
    :type t
    :documentation "The reconnectable pool of named Lisp REPL workers.")
   (agent
    :initarg :agent
    :accessor application-agent
    :type (option agent)
    :documentation "The reconnectable provider and tool coordinator.")
   (ui
    :initarg :ui
    :accessor application-ui
    :type (option terminal-ui)
    :documentation "The reconnectable primary-screen terminal UI.")
   (goal
    :initform nil
    :accessor application-goal
    :type list
    :documentation "The session goal plist holding objective, status, and continuations.")
   (reasoning-traces-p
    :initarg :reasoning-traces-p
    :initform nil
    :accessor application-reasoning-traces-p
    :type boolean
    :documentation "Whether provider-visible reasoning summaries appear in the transcript.")
   (compact-view-p
    :initarg :compact-view-p
    :initform t
    :accessor application-compact-view-p
    :type boolean
    :documentation "Whether successful routine tool results are hidden.")
   (installation-provenance
    :initarg :installation-provenance
    :initform nil
    :accessor application-installation-provenance
    :type (option installation-provenance)
    :documentation "The structurally validated installation method for this run.")
   (update-availability
    :initarg :update-availability
    :initform nil
    :accessor application-update-availability
    :type (option update-availability)
    :documentation "The newer nondismissed release cached before startup.")
   (update-check-thread
    :initform nil
    :accessor application-update-check-thread
    :type t
    :documentation "The bounded availability thread awaiting checkpoint quiescence.")
   (permission-state
    :initarg :permission-state
    :initform (make-instance 'permission-state)
    :accessor application-permission-state
    :type permission-state
    :documentation "The exact command approvals loaded from private state.")
   (permission-mode
    :initarg :permission-mode
    :initform :ask
    :accessor application-permission-mode
    :type (member :ask :sandboxed :full-access)
    :documentation "The command approval behavior for this process session.")
   (command-authorization-lock
    :initform (make-lock "Autolith command authorization")
    :reader application-command-authorization-lock
    :documentation "The lock serializing command prompts from concurrent agents.")
   (input-controller
    :initform nil
    :accessor application-input-controller
    :type t
    :documentation "The active responsive input controller, when one is running.")
   (overlay-failures
    :initform nil
    :accessor application-overlay-failures
    :type list
    :documentation "Overlay files that failed to load at startup, with reasons.")
   (rendered-sequence
    :initform 0
    :accessor application-rendered-sequence
    :type integer
    :documentation "The last durable conversation sequence printed to scrollback.")
   (presentation-counter
    :initform 0
    :accessor application-presentation-counter
    :type integer
    :documentation "The identifier source for non-conversation terminal notices.")
   (project-adaptation-offer-p
    :initform nil
    :accessor application-project-adaptation-offer-p
    :type boolean
    :documentation
    "Whether command-line resume should offer project adaptation notes."))
  (:documentation "The globally rooted logical state and reconnectable resources of Autolith."))

(defvar *active-application* nil
  "The live application root retained in saved generations.")

(defvar *terminal-resize-pending-p* nil
  "True after SIGWINCH until the active UI recomputes its dimensions.")


;;;; -- Interactive Command Table --

(defmacro define-application-commands (&body commands)
  "Define literal COMMANDS after validating their user-facing metadata.

Each command requires a non-empty :TIP at macro expansion time. Optional
:ALIASES share the canonical command's behavior and tip without entering
completion or help output."
  (labels ((non-empty-literal-string-p (value)
             "Return true when VALUE is a non-blank literal string."
             (and (stringp value)
                  (plusp
                   (length
                    (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 value)))))

           (command-identifiers (command)
             "Return COMMAND's canonical name followed by its aliases."
             (cons (getf command :name) (getf command :aliases)))

           (command-identifier-p (value)
             "Return true when VALUE is a normalized slash command."
             (and (non-empty-literal-string-p value)
                  (char= (char value 0) #\/)
                  (string= value (string-downcase value)))))
    (unless commands
      (error "At least one application command must be defined."))
    (let ((seen-identifiers nil))
      (dolist (command commands)
        (unless (and (listp command) (evenp (length command)))
          (error "Application command ~S is not a literal property list."
                 command))
        (let ((name (getf command :name))
              (aliases (getf command :aliases))
              (tip (getf command :tip)))
          (unless (command-identifier-p name)
            (error "Application command ~S needs a lowercase slash-prefixed literal name."
                   command))
          (unless (and (listp aliases)
                       (every #'command-identifier-p aliases))
            (error "Application command ~A has invalid literal aliases." name))
          (unless (non-empty-literal-string-p tip)
            (error "Application command ~A requires a non-empty literal :TIP."
                   name))
          (dolist (identifier (command-identifiers command))
            (when (member identifier seen-identifiers :test #'string=)
              (error "Application command identifier ~A is defined twice."
                     identifier))
            (push identifier seen-identifiers)))))
    `(define-constant +application-commands+
       ',commands
       :test #'equal
       :documentation "Canonical interactive commands, aliases, help, and tips.")))

(define-application-commands
  (:name "/help" :argument nil
   :description "show this reference"
   :tip "shows every interactive command.")
  (:name "/new" :argument nil
   :description "start a new conversation"
   :tip "starts fresh without deleting the current conversation.")
  (:name "/resume" :argument nil
   :description "pick a saved conversation to resume"
   :tip "returns to a saved conversation from this workspace or another one.")
  (:name "/conversations" :argument nil
   :description "list saved conversations"
   :tip "lists saved conversations from newest to oldest.")
  (:name "/cwd" :argument "PATH"
   :description "change the active workspace"
   :tip "moves the active workspace without restarting Autolith.")
  (:name "/auth" :argument nil
   :description "authenticate Autolith with ChatGPT"
   :tip "starts direct ChatGPT authentication when credentials need attention.")
  (:name "/model" :argument nil
   :description "pick the 5.6 model and reasoning effort"
   :tip "changes both the model and its reasoning effort.")
  (:name "/effort" :argument nil
   :description "pick the reasoning effort"
   :tip "changes reasoning effort without switching models.")
  (:name "/trace" :argument "on|off"
   :description "show visible reasoning summaries"
   :tip "toggles visible reasoning summaries with on or off.")
  (:name "/permissions" :argument nil
   :description "choose command access for this session"
   :tip "chooses how shell commands are authorized for this session.")
  (:name "/later" :argument "INPUT"
   :description "run input after rate limits reset"
   :tip "queues a prompt for the next known rate-limit reset.")
  (:name "/goal" :argument "OBJECTIVE"
   :description "set or view the session goal"
   :tip "sets the objective Autolith should pursue across continuations.")
  (:name "/agenda" :argument nil
   :description "show workspace agenda entries"
   :tip "shows durable commitments and notes for the current workspace.")
  (:name "/checkpoint" :argument nil
   :description "save a retained live generation"
   :tip "saves the current live state as a retained generation.")
  (:name "/generations" :argument nil
   :description "list retained generations"
   :tip "shows live generations available for recovery.")
  (:name "/rollback" :argument nil
   :description "pick a generation for recovery"
   :tip "selects a retained generation for the next recovery start.")
  (:name "/status" :argument nil :aliases ("/usage")
   :description "show usage and rate limits"
   :tip "shows the model, context usage, and subscription rate limits.")
  (:name "/context" :argument nil
   :description "inspect request-local context"
   :tip "reveals the ephemeral context prepared for provider requests.")
  (:name "/compact" :argument "on|off"
   :description "hide routine results, or summarize with no argument"
   :tip "toggles routine result visibility; with no argument it compacts context.")
  (:name "/quit" :argument nil :aliases ("/exit")
   :description "leave Autolith"
   :tip "exits cleanly; Ctrl-C also prints the exact resume command."))

(-> application-command-entry (string) (option list))
(defun application-command-entry (name)
  "Return the canonical command entry matching NAME or one of its aliases."
  (let ((normalized-name (string-downcase name)))
    (find-if
     (lambda (entry)
       (or (string= normalized-name (getf entry :name))
           (member normalized-name (getf entry :aliases) :test #'string=)))
     +application-commands+)))

(-> application-command-canonical-name (string) string)
(defun application-command-canonical-name (name)
  "Return NAME normalized through the canonical command and alias table."
  (let* ((normalized-name (string-downcase name))
         (entry (application-command-entry normalized-name)))
    (if entry
        (getf entry :name)
        normalized-name)))


;;;; -- Construction and Reconnection --

(-> terminal--positive-integer-or-nil ((option string)) (option integer))
(defun terminal--positive-integer-or-nil (value)
  "Parse VALUE as a positive integer, returning NIL on failure."
  (handler-case
      (let ((parsed (and (non-empty-string-p value)
                         (parse-integer value :junk-allowed t))))
        (and parsed (plusp parsed) parsed))
    (error ()
      nil)))

(-> terminal--query-dimension (string) (option integer))
(defun terminal--query-dimension (capability)
  "Return positive TPUT CAPABILITY output when a terminal is attached."
  (when (interactive-stream-p *terminal-io*)
    (handler-case
        (terminal--positive-integer-or-nil
         (uiop:run-program (list "tput" capability)
                           :output :string
                           :error-output :output))
      (error ()
        nil))))

(-> terminal-current-size () (values integer integer))
(defun terminal-current-size ()
  "Return current terminal rows and columns, preferring its kernel dimensions."
  (multiple-value-bind (terminal-rows terminal-columns)
      (terminal-file-descriptor-size 0)
    (values
     (or terminal-rows
         (terminal--query-dimension "lines")
         (terminal--positive-integer-or-nil (uiop:getenv "LINES"))
         +terminal-default-rows+)
     (or terminal-columns
         (terminal--query-dimension "cols")
         (terminal--positive-integer-or-nil (uiop:getenv "COLUMNS"))
         +terminal-default-columns+))))

(-> application-pending-terminal-size
    ()
    (option (cons (integer 1) (integer 1))))
(defun application-pending-terminal-size ()
  "Consume a pending SIGWINCH and return refreshed rows and columns."
  (when *terminal-resize-pending-p*
    (setf *terminal-resize-pending-p* nil)
    (multiple-value-bind (rows columns)
        (terminal-current-size)
      (cons rows columns))))

(define-constant +application-prompt+ "❯ "
  :test #'string=
  :documentation "The styled input prompt shown on the live editor row.")

(define-constant +application-placeholder+
  "Ask Autolith anything. Type /help for commands."
  :test #'string=
  :documentation "The dim hint shown on the prompt row while input is empty.")

(-> application-terminal-ui-create () terminal-ui)
(defun application-terminal-ui-create ()
  "Create the standard interactive terminal UI at the current terminal size."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (terminal-ui-create
     :terminal (stream-terminal-create :rows rows :columns columns)
     :prompt +application-prompt+
     :placeholder +application-placeholder+
     :completions +application-commands+)))

(-> application--configuration-for-conversation
    (configuration conversation)
    configuration)
(defun application--configuration-for-conversation (configuration conversation)
  "Restore CONVERSATION's model selection over CONFIGURATION when present."
  (configuration--clone
   configuration
   :model (conversation-model conversation)
   :reasoning-effort (conversation-reasoning-effort conversation)))

(-> application-create
    (configuration &key (:conversation-id (option string)))
    application)
(defun application-create (configuration &key conversation-id)
  "Create a connected application, loading CONVERSATION-ID when supplied."
  (let ((preferred-configuration
          (preferences-apply-model-selection configuration)))
    (context-runtime-reset)
    (configuration-ensure-directories preferred-configuration)
    (conversation-identifier-migrate preferred-configuration)
    (durable-mutations-load preferred-configuration)
    (let* ((overlay-failures (image-state-load preferred-configuration))
           (user-init-pathname (user-init-load preferred-configuration))
           (reasoning-traces-p
             (preferences-reasoning-traces-p preferred-configuration))
           (compact-view-p
             (preferences-compact-view-p preferred-configuration))
           (permission-state (permissions-load preferred-configuration))
           (conversation (if conversation-id
                             (conversation-load-by-id preferred-configuration
                                                      conversation-id)
                             (conversation-create preferred-configuration)))
           (configuration
             (application--configuration-for-conversation
              preferred-configuration
              conversation))
           (installation-provenance
             (installation-provenance-detect configuration))
           (update-availability
             (update-availability-current configuration installation-provenance))
           (provider (provider-create
                      configuration
                      :reasoning-summaries-p reasoning-traces-p))
           (registry
             (task-augment-tool-registry
              (make-default-tool-registry
               :immutable-p (configuration-immutable-p configuration))))
           (worker (lisp-worker-pool-create configuration))
           (agent (agent-create :configuration configuration
                                :provider provider
                                :conversation conversation
                                :tool-registry registry
                                :worker worker))
           (ui (application-terminal-ui-create))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider provider
                                       :tool-registry registry
                                       :worker worker
                                       :agent agent
                                       :ui ui
                                       :permission-state permission-state
                                       :reasoning-traces-p reasoning-traces-p
                                       :compact-view-p compact-view-p
                                       :installation-provenance
                                       installation-provenance
                                       :update-availability
                                       update-availability)))
      (declare (ignore user-init-pathname))
      (setf (application-overlay-failures application) overlay-failures)
      (application--load-goal application)
      application)))

(-> application-reconnect
    (application &key (:conversation-id (option string))
                      (:immutable-p boolean))
    application)
(defun application-reconnect
    (application &key conversation-id
                   (immutable-p nil immutable-p-supplied-p))
  "Reconnect retained APPLICATION resources, optionally selecting CONVERSATION-ID."
  (tool-registry-close-runtime-state
   (application-tool-registry application))
  (image-state-reconnect)
  (context-runtime-reset)
  (let* ((previous (application-configuration application))
         (effective-immutable-p
           (if immutable-p-supplied-p
               immutable-p
               (configuration-immutable-p previous)))
         (retained-conversation (application-conversation application))
         (retained-configuration
           (configuration-create
            :working-directory (uiop:getcwd)
            :model (configuration-model previous)
            :reasoning-effort (configuration-reasoning-effort previous)
            :immutable-p effective-immutable-p))
         (prepared-configuration
           (progn
             (configuration-ensure-directories retained-configuration)
             (conversation-identifier-migrate retained-configuration)
             (user-init-load retained-configuration)
             retained-configuration))
         (reasoning-traces-p
           (preferences-reasoning-traces-p prepared-configuration))
         (compact-view-p
           (preferences-compact-view-p prepared-configuration))
         (permission-state (permissions-load prepared-configuration))
         (recovery-conversation-id
           (let ((value (uiop:getenv "AUTOLITH_RECOVERY_CONVERSATION_ID")))
             (and (non-empty-string-p value) value)))
         (overlay-failures nil)
         (conversation
           (cond
             (conversation-id
              (conversation-load-by-id prepared-configuration conversation-id))
             (recovery-conversation-id
              (conversation-load-by-id prepared-configuration
                                       recovery-conversation-id))
             ((conversation-persisted-p retained-conversation)
              (conversation-load-by-id
               prepared-configuration
               (conversation-identifier retained-conversation)))
             (t
              (conversation-create prepared-configuration))))
         (configuration
           (application--configuration-for-conversation
            prepared-configuration
            conversation))
         (installation-provenance
           (installation-provenance-detect configuration))
         (update-availability
           (update-availability-current configuration installation-provenance))
         (provider (provider-create
                    configuration
                    :reasoning-summaries-p reasoning-traces-p))
         (worker (lisp-worker-pool-create configuration))
         (registry (task-augment-tool-registry
                    (make-default-tool-registry
                     :immutable-p effective-immutable-p)))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (application-terminal-ui-create))
         (recovery-rendered-sequence
           (handler-case
               (let ((value (uiop:getenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")))
                 (and (non-empty-string-p value)
                      (parse-integer value :junk-allowed nil)))
             (error ()
               nil))))
    (setf (application-configuration application) configuration
          (application-conversation application) conversation
          (application-provider application) provider
          (application-tool-registry application) registry
          (application-worker application) worker
          (application-agent application) agent
          (application-ui application) ui
          (application-permission-state application) permission-state
          (application-permission-mode application) :ask
          (application-input-controller application) nil
          (application-reasoning-traces-p application) reasoning-traces-p
          (application-compact-view-p application) compact-view-p
          (application-installation-provenance application)
          installation-provenance
          (application-update-availability application) update-availability
          (application-update-check-thread application) nil
          (application-rendered-sequence application)
          (if (and recovery-rendered-sequence
                   (string= (conversation-identifier conversation)
                            (if recovery-conversation-id
                                (conversation-identifier-migration-resolve
                                 prepared-configuration
                                 recovery-conversation-id)
                                "")))
              recovery-rendered-sequence
              0)
          (application-overlay-failures application) overlay-failures)
    (application--load-goal application)
    application))

(-> application--quiesce-update-check (application) null)
(defun application--quiesce-update-check (application)
  "Join APPLICATION's bounded availability thread before a checkpoint fork."
  (let ((thread (application-update-check-thread application)))
    (when thread
      (when (eq thread (current-thread))
        (error 'checkpoint-error
               :message "An update check cannot quiesce its own thread."
               :stage ':fork
               :pathname nil))
      (join-thread thread)
      (when (eq thread (application-update-check-thread application))
        (setf (application-update-check-thread application) nil))))
  nil)

(defmethod checkpoint-detach-state ((application application))
  "Detach APPLICATION's ephemeral object graph in a checkpoint saver child."
  (context-runtime-reset)
  (tool-registry-detach-runtime-state
   (application-tool-registry application))
  (setf (application-provider application) nil
        (application-worker application) nil
        (application-agent application) nil
        (application-ui application) nil
        (application-input-controller application) nil
        (application-update-check-thread application) nil)
  application)

(-> application--install-configuration
    (application configuration &key (:conversation (option conversation)))
    null)
(defun application--install-configuration (application configuration
                                            &key conversation)
  "Switch APPLICATION to CONFIGURATION and optionally CONVERSATION."
  (let* ((active-conversation (or conversation
                                  (application-conversation application)))
         (previous-provider (application-provider application))
         (provider
           (if previous-provider
               (provider-with-configuration previous-provider configuration)
               (provider-create configuration)))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation active-conversation
                              :tool-registry (application-tool-registry
                                              application)
                              :worker (application-worker application))))
    (setf (application-configuration application) configuration
          (application-conversation application) active-conversation
          (application-provider application) provider
          (application-agent application) agent))
  nil)

(-> application--working-directory-failure
    (&key (:requested-path t)
          (:previous-directory pathname)
          (:stage keyword)
          (:cause t)
          (:rollback-cause t))
    null)
(defun application--working-directory-failure
    (&key requested-path previous-directory stage cause rollback-cause)
  "Signal a structured workspace change failure and include any rollback failure."
  (error 'working-directory-error
         :message
         (format nil
                 "Could not change working directory from ~A to ~S during ~(~A~): ~A~@[ Rollback also failed: ~A~]"
                 previous-directory
                 requested-path
                 stage
                 cause
                 rollback-cause)
         :requested-path requested-path
         :previous-directory previous-directory
         :stage stage
         :cause cause
         :rollback-cause rollback-cause))

(-> application-set-working-directory
    (application (or pathname string))
    pathname)
(defun application-set-working-directory (application location)
  "Move APPLICATION and its Lisp workers to existing directory LOCATION."
  (let* ((previous-configuration (application-configuration application))
         (configuration
           (configuration-with-working-directory previous-configuration location))
         (previous-directory
           (configuration-working-directory previous-configuration))
         (directory (configuration-working-directory configuration))
         (manager (application-worker application))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (workers-moved-p nil)
         (process-moved-p nil))
    (labels ((restore-previous-workspace ()
               "Restore changed process and worker state, returning rollback failures."
               (let ((failures nil))
                 (when process-moved-p
                   (handler-case
                       (progn
                         (uiop:chdir previous-process-directory)
                         (setf *default-pathname-defaults* previous-defaults))
                     (error (condition)
                       (push condition failures))))
                 (when workers-moved-p
                   (handler-case
                       (lisp-worker-manager-change-working-directory
                        manager previous-configuration)
                     (error (condition)
                       (push condition failures))))
                 (nreverse failures)))

             (fail (stage cause)
               "Restore prior state and signal the failure at STAGE caused by CAUSE."
               (application--working-directory-failure
                :requested-path location
                :previous-directory previous-directory
                :stage stage
                :cause cause
                :rollback-cause (restore-previous-workspace))))
      (handler-case
          (tool-registry-close-runtime-state
           (application-tool-registry application))
        (error (condition)
          (fail ':tools condition)))
      (handler-case
          (lisp-worker-manager-change-working-directory manager configuration)
        (error (condition)
          (fail ':workers condition)))
      (setf workers-moved-p t)
      (handler-case
          (progn
            (uiop:chdir directory)
            (setf process-moved-p t
                  *default-pathname-defaults* directory))
        (error (condition)
          (fail ':process condition)))
      (handler-case
          (application--install-configuration application configuration)
        (error (condition)
          (fail ':application condition)))
      directory)))

(-> application-install-conversation (application conversation) application)
(defun application-install-conversation (application conversation)
  "Make CONVERSATION active and restore its model selection."
  (tool-registry-close-runtime-state
   (application-tool-registry application))
  (application--install-configuration
   application
   (application--configuration-for-conversation
    (application-configuration application)
    conversation)
   :conversation conversation)
  (setf (application-rendered-sequence application) 0)
  (application--load-goal application)
  application)


;;;; -- Transcript Projection --

(-> application--transcript-width (application) integer)
(defun application--transcript-width (application)
  "Return APPLICATION's terminal width available to wrapped transcript bodies."
  (max 20
       (- (terminal-columns (terminal-ui-terminal (application-ui application)))
          3)))

(-> application--indented-body (application string) string)
(defun application--indented-body (application text)
  "Return sanitized TEXT wrapped and indented under its transcript header."
  (format nil "~{  ~A~^~%~}"
          (wrap-text (string-right-trim
                      '(#\Space #\Tab #\Newline #\Return)
                      (sanitize-text text))
                     (application--transcript-width application))))

(-> application--markdown-renderer (application) markdown-renderer)
(defun application--markdown-renderer (application)
  "Return a markdown renderer sized to APPLICATION's current terminal width."
  (markdown-renderer-create
   :width (max 24
               (1- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))))))

(-> application--markdown-body (application string) list)
(defun application--markdown-body (application text)
  "Return sanitized TEXT rendered as markdown transcript spans."
  (let ((renderer (application--markdown-renderer application))
        (trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                    (sanitize-text text))))
    (loop for line in (or (uiop:split-string trimmed :separator '(#\Newline))
                          (list ""))
          append (loop for row in (markdown-render-line renderer line)
                       append (append row
                                      (list (terminal-span
                                             ':plain
                                             (string #\Newline))))))))

(-> application--transcript-entry
    (application &key (:style terminal-style) (:header string)
                 (:detail (option string)) (:body (option string))
                 (:body-style terminal-style))
    list)
(defun application--transcript-entry
    (application &key (style ':plain) (header "") detail body
                 (body-style ':plain))
  "Return one styled transcript entry with HEADER, optional DETAIL, and BODY rows."
  (let ((spans (list (terminal-span style header))))
    (when detail
      (let ((available (- (terminal-columns
                           (terminal-ui-terminal (application-ui application)))
                          3
                          (text-cell-width header)))
            (safe-detail (sanitize-text detail :single-line-p t)))
        (when (> available 1)
          (let ((visible (text-cell-prefix safe-detail (1- available))))
            (setf spans
                  (append spans
                          (list (terminal-span
                                 :dim
                                 (format nil "  ~A~:[~;…~]"
                                         visible
                                         (< (length visible)
                                            (length safe-detail)))))))))))
    (when body
      (setf spans
            (append spans
                    (list (terminal-span ':plain (string #\Newline))
                          (terminal-span body-style
                                         (application--indented-body
                                          application
                                          body))))))
    spans))

;;;; -- Tool Transcript Protocol --

(-> application-tool-call-entry ((option tool) application json-object) list)
(defgeneric application-tool-call-entry (tool application call)
  (:documentation "Return the styled transcript entry for one function CALL."))

(-> application-tool-result-entry ((option tool) application list) list)
(defgeneric application-tool-result-entry (tool application record)
  (:documentation "Return the styled transcript entry for one tool result RECORD."))

(-> application--find-tool (application string) (option tool))
(defun application--find-tool (application canonical-name)
  "Return APPLICATION's registered tool named CANONICAL-NAME, when available."
  (let ((separator (position #\. canonical-name))
        (registry (and (slot-boundp application 'tool-registry)
                       (application-tool-registry application))))
    (when (and separator (typep registry 'tool-registry))
      (tool-registry-find registry
                          (subseq canonical-name 0 separator)
                          (subseq canonical-name (1+ separator))))))


(-> application--field-spans (string string) list)
(defun application--field-spans (label value)
  "Return one aligned dim LABEL and plain VALUE line as transcript spans."
  (list (terminal-span :dim (format nil "  ~13A " label))
        (terminal-span :plain (format nil "~A~%" value))))

(-> web-search-call-detail (json-object) string)
(defun web-search-call-detail (item)
  "Return a short human-readable description of a web search call ITEM."
  (let* ((action (json-get item "action"))
         (action-type (if (json-object-p action)
                          (or (json-get action "type") "")
                          "")))
    (cond
      ((string= action-type "search")
       (let ((query (json-get action "query"))
             (queries (json-get action "queries")))
         (cond
           ((non-empty-string-p query)
            query)
           ((and (vectorp queries)
                 (plusp (length queries))
                 (stringp (aref queries 0)))
            (format nil "~A~:[~;, more~]"
                    (aref queries 0)
                    (> (length queries) 1)))
           (t
            ""))))
      ((string= action-type "open_page")
       (or (json-get action "url") ""))
      ((string= action-type "find_in_page")
       (format nil "~@[~A in ~]~A"
               (json-get action "pattern")
               (or (json-get action "url") "")))
      (t
       ""))))

(define-constant +application-reasoning-preview-row-limit+ 5
  :documentation "The maximum rows shown for one unfinished reasoning summary.")

(-> application--reasoning-summary-rows (application string) list)
(defun application--reasoning-summary-rows (application summary)
  "Return styled rows for one provider-visible reasoning SUMMARY."
  (let* ((safe-summary
           (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                              (sanitize-text summary)))
         (body-width
           (max 1
                (- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))
                   5))))
    (cons
     (list (terminal-span ':hint "◇ reasoning summary"))
     (loop for row in (markdown-render-inline safe-summary body-width)
           collect (append (list (terminal-span ':dim "  │ ")) row)))))

(-> application--reasoning-preview-rows (application string) list)
(defun application--reasoning-preview-rows (application summary)
  "Return a tail-biased bounded live preview of reasoning SUMMARY."
  (let ((rows (application--reasoning-summary-rows application summary)))
    (if (<= (length rows) +application-reasoning-preview-row-limit+)
        rows
        (append (list (first rows)
                      (list (terminal-span ':dim "  │ …")))
                (last rows (- +application-reasoning-preview-row-limit+ 2))))))

(-> application--reasoning-summary-entry (application string) list)
(defun application--reasoning-summary-entry (application summary)
  "Return one railed transcript entry for provider-visible reasoning SUMMARY."
  (loop for row in (application--reasoning-summary-rows application summary)
        for first-row-p = t then nil
        append (if first-row-p
                   row
                   (cons (terminal-span ':plain (string #\Newline)) row))))

(-> response-item-entry (application json-object) (option list))
(defun response-item-entry (application item)
  "Return a styled transcript entry for completed provider ITEM."
  (let ((type (json-get item "type")))
    (cond
      ((and (string= (or type "") "message")
            (string= (or (json-get item "role") "") "assistant"))
       (let ((text (response-item-assistant-text item)))
         (when text
           (append (list (terminal-span ':brand "● autolith")
                         (terminal-span ':plain (string #\Newline)))
                   (application--markdown-body application text)))))
      ((string= (or type "") "reasoning")
       (when (application-reasoning-traces-p application)
         (let ((summary (response-item-reasoning-summary item)))
           (when summary
             (application--reasoning-summary-entry application summary)))))
      ((string= (or type "") "function_call")
       (let* ((canonical-name (function-call-canonical-name item))
              (tool (application--find-tool application canonical-name)))
         (application-tool-call-entry tool application item)))
      ((string= (or type "") "web_search_call")
       (application--transcript-entry
        application
        :style ':tool
        :header "▸ web search"
        :detail (let ((detail (web-search-call-detail item)))
                  (and (non-empty-string-p detail)
                       detail))))
      (t
       nil))))

(-> conversation-record-entry (application list) (option list))
(defun conversation-record-entry (application record)
  "Return the styled transcript entry represented by durable RECORD."
  (case (first record)
    (:message
     (when (eq (getf (rest record) :role) :user)
       (let ((content (getf (rest record) :content)))
         (if (application--goal-continuation-message-p content)
             (list (terminal-span ':hint "∙ goal continues"))
             (application--transcript-entry application
                                            :style ':user
                                            :header "❯ you"
                                            :body content)))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (response-item-entry application (json-decode wire-json)))))
    (:tool-result
     (let* ((canonical-name (getf (rest record) :tool))
            (tool (application--find-tool application canonical-name)))
       (when (or (not (application-compact-view-p application))
                 (not (eq (getf (rest record) :status) ':ok))
                 (member canonical-name
                         '("fs.write" "fs.edit" "shell.run"
                           "lisp.eval" "self.eval")
                         :test #'string=))
         (application-tool-result-entry tool application record))))
    (:summary
     (list (terminal-span
            ':hint
            (format nil "∙ context compacted through sequence ~A"
                    (getf (rest record) :through-seq)))))
    (otherwise
     nil)))

(-> application-present (application (or string list)) boolean)
(defun application-present (application entry)
  "Append non-conversation ENTRY once to APPLICATION's normal scrollback."
  (let ((ui (application-ui application)))
    (with-terminal-ui-locked (ui)
      (let ((identifier (incf (application-presentation-counter application))))
        (terminal-ui-append-finalized
         ui
         (list :presentation identifier)
         entry)))))

(-> application--assistant-message-record-text (list) (option string))
(defun application--assistant-message-record-text (record)
  "Return the visible assistant text carried by durable RECORD, when present."
  (when (eq (first record) :provider-item)
    (let ((wire-json (getf (rest record) :wire-json)))
      (when (stringp wire-json)
        (let ((item (json-decode wire-json)))
          (and (json-object-p item)
               (response-item-assistant-text item)))))))

(-> application--reasoning-record-text (list) (option string))
(defun application--reasoning-record-text (record)
  "Return the visible reasoning summary carried by durable RECORD, when present."
  (when (eq (first record) :provider-item)
    (let ((wire-json (getf (rest record) :wire-json)))
      (when (stringp wire-json)
        (let ((item (json-decode wire-json)))
          (and (json-object-p item)
               (response-item-reasoning-summary item)))))))

(-> application-render-records
    (application &key (:streamed-assistant-text (option string))
                      (:streamed-reasoning-text (option string)))
    null)
(defun application-render-records
    (application &key streamed-assistant-text streamed-reasoning-text)
  "Append APPLICATION's not-yet-rendered durable transcript records once.

Assistant and reasoning records are suppressed only when their joined durable
text exactly matches the corresponding streamed text. Suppressed identifiers
remain finalized so later conversation replay cannot duplicate streamed rows."
  (let* ((ui (application-ui application))
         (conversation (application-conversation application))
         (conversation-id (conversation-identifier conversation))
         (records
           (loop for record in (rest (conversation--read-records
                                      (conversation-pathname conversation)))
                 for sequence = (getf (rest record) :seq)
                 when (and (integerp sequence)
                           (> sequence
                              (application-rendered-sequence application)))
                   collect record))
         (assistant-texts
           (loop for record in records
                 for text = (application--assistant-message-record-text record)
                 when text
                   collect text))
         (reasoning-texts
           (loop for record in records
                 for text = (application--reasoning-record-text record)
                 when text
                   collect text))
         (assistant-stream-match-p
           (and streamed-assistant-text
                assistant-texts
                (string= streamed-assistant-text
                         (format nil "~{~A~^~%~}" assistant-texts))))
         (reasoning-stream-match-p
           (and streamed-reasoning-text
                reasoning-texts
                (string= streamed-reasoning-text
                         (format nil "~{~A~^~2%~}" reasoning-texts)))))
    (dolist (record records)
      (let* ((sequence (getf (rest record) :seq))
             (identifier (list :conversation conversation-id sequence))
             (assistant-text
               (application--assistant-message-record-text record))
             (reasoning-text
               (application--reasoning-record-text record)))
        (if (or (and assistant-stream-match-p assistant-text)
                (and reasoning-stream-match-p reasoning-text))
            (terminal-ui-mark-finalized ui identifier)
            (let ((entry (conversation-record-entry application record)))
              (when entry
                (terminal-ui-append-finalized ui identifier entry))))
        (setf (application-rendered-sequence application) sequence))))
  nil)


;;;; -- Session Goal --

(define-constant +application-goal-continuation-limit+ 8
  :documentation "The automatic goal continuation turns allowed per user message.")

(define-constant +application-goal-continuation-prompt+
  "Continue working toward the session goal."
  :test #'string=
  :documentation "The synthetic user message driving one goal continuation turn.")

(define-constant +application-goal-complete-marker+ "[GOAL-COMPLETE]"
  :test #'string=
  :documentation "The literal marker the model includes once the goal is met.")

(-> application--record-goal (application) null)
(defun application--record-goal (application)
  "Append APPLICATION's current goal state to the durable conversation."
  (let ((goal (application-goal application)))
    (conversation-append-record
     (application-conversation application)
     (if goal
         (list :goal
               :objective (getf goal :objective)
               :status (getf goal :status)
               :continuations (getf goal :continuations)
               :created-at (getf goal :created-at))
         (list :goal
               :objective nil
               :status ':cleared
               :continuations 0
               :created-at nil))))
  nil)

(-> application--load-goal (application) null)
(defun application--load-goal (application)
  "Restore APPLICATION's goal from the newest durable goal record."
  (let ((goal nil))
    (dolist (record (rest (conversation--read-records
                           (conversation-pathname
                            (application-conversation application)))))
      (when (eq (first record) :goal)
        (let ((objective (getf (rest record) :objective))
              (status (getf (rest record) :status)))
          (setf goal
                (and (non-empty-string-p objective)
                     (member status '(:active :paused :complete))
                     (list :objective objective
                           :status status
                           :continuations
                           (let ((count (getf (rest record) :continuations)))
                             (if (integerp count)
                                 count
                                 0))
                           :created-at
                           (let ((time (getf (rest record) :created-at)))
                             (if (integerp time)
                                 time
                                 (getf (rest record) :time)))))))))
    (setf (application-goal application) goal))
  nil)

(-> application-goal-context (application) (option string))
(defun application-goal-context (application)
  "Return the transient goal instructions for the next provider request."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (format nil
              "<goal_context>~%The session goal: ~A~%Work autonomously ~
               toward this goal every turn. When the goal is genuinely ~
               complete, include the literal marker ~A in your final ~
               message. If you cannot continue without the user, state ~
               plainly what you need and stop.~%</goal_context>"
              (getf goal :objective)
              +application-goal-complete-marker+))))

(-> application--goal-continuation-message-p (string) boolean)
(defun application--goal-continuation-message-p (content)
  "Return true when CONTENT is the synthetic goal continuation prompt."
  (string= content +application-goal-continuation-prompt+))


;;;; -- Agent Presentation --

(defparameter *application-thinking-words*
  '("pondering" "exploring" "untangling" "crafting" "verifying" "connecting")
  "One-word activity labels sampled while the model prepares its next action.")

(-> application--git-branch (pathname) (option string))
(defun application--git-branch (directory)
  "Return the enclosing Git worktree branch for DIRECTORY, when attached."
  (handler-case
      (let ((branch
              (string-trim
               '(#\Space #\Tab #\Newline #\Return)
               (uiop:run-program
                (list "git" "-C" (namestring directory)
                      "symbolic-ref" "--quiet" "--short" "HEAD")
                :output :string
                :error-output :string
                :ignore-error-status t))))
        (and (non-empty-string-p branch) branch))
    (error ()
      nil)))

(-> application--status-details (application) terminal-styled-text)
(defun application--status-details (application)
  "Return cached-phase model, effort, and repository status spans."
  (when (slot-boundp application 'configuration)
    (let* ((configuration (application-configuration application))
           (branch
             (application--git-branch
              (configuration-working-directory configuration))))
      (append
       (list (terminal-span ':status-dim "  ")
             (terminal-span ':status-model
                            (configuration-model configuration))
             (terminal-span ':status-dim " · ")
             (terminal-span ':status-effort
                            (configuration-reasoning-effort configuration)))
       (when branch
         (list (terminal-span ':status-dim " · git ")
               (terminal-span ':status-branch branch)))))))

(-> application-set-activity (application (option string)) terminal-ui)
(defun application-set-activity (application status)
  "Set APPLICATION's live STATUS and snapshot its contextual details once."
  (terminal-ui-set-status
   (application-ui application)
   status
   :details (and status (application--status-details application))))

(-> application-thinking-label () string)
(defun application-thinking-label ()
  "Return one interesting activity word for the next provider step."
  (if *application-thinking-words*
      (nth (random (length *application-thinking-words*))
           *application-thinking-words*)
      "pondering"))

(-> application-agent-observer
    (application &key (:steering-function (option function)))
    agent-observer)
(defun application-agent-observer (application &key steering-function)
  "Return a terminal observer streaming one APPLICATION turn as stable lines."
  (let ((ui (application-ui application))
        (activity-label (application-thinking-label))
        (reasoning-text "")
        (presented-reasoning-text nil)
        (stream-text "")
        (stream-pending "")
        (stream-open-p nil)
        (stream-renderer nil))
    (labels ((reasoning-flush ()
               "Finalize the visible reasoning summary before assistant output."
               (terminal-ui-set-preview-rows ui nil)
               (when (and (application-reasoning-traces-p application)
                          (plusp (length reasoning-text))
                          (null presented-reasoning-text))
                 (application-present
                  application
                  (application--reasoning-summary-entry application reasoning-text))
                 (setf presented-reasoning-text reasoning-text)))

             (stream-text-delta (delta)
               "Commit DELTA's completed markdown rows and repaint the fluid tail."
               (when (plusp (length delta))
                 (terminal-ui-note-status-progress ui)
                 (setf stream-text
                       (concatenate 'string stream-text delta)
                       stream-pending
                       (sanitize-text
                        (concatenate 'string stream-pending delta)))
                 (let ((rows nil))
                   (unless stream-open-p
                     (reasoning-flush)
                     (setf stream-open-p t
                           stream-renderer (application--markdown-renderer
                                            application))
                     (application-set-activity application "receiving response")
                     (push (list (terminal-span ':brand "● autolith")) rows))
                   (loop for newline = (position #\Newline stream-pending)
                         while newline
                         do (setf rows
                                  (append rows
                                          (markdown-render-line
                                           stream-renderer
                                           (subseq stream-pending 0 newline)))
                                  stream-pending
                                  (subseq stream-pending (1+ newline))))
                   (multiple-value-bind (overflow-rows tail retained)
                       (markdown-render-partial stream-renderer stream-pending)
                     (setf stream-pending retained)
                     (terminal-ui-stream-update
                      ui
                      :rows (append rows overflow-rows)
                      :tail tail)))))

             (stream-flush ()
               "Finish the streamed block with its remaining text and separator."
               (when stream-open-p
                 (terminal-ui-stream-update
                  ui
                  :rows (append (when (plusp (length stream-pending))
                                  (markdown-render-line stream-renderer
                                                        stream-pending))
                                (list nil))
                  :tail nil)
                 (setf stream-pending ""
                       stream-open-p nil
                       stream-renderer nil))))
      (callback-agent-observer-create
       :text-callback #'stream-text-delta
       :reasoning-callback
       (lambda (delta)
         (when (plusp (length delta))
           (terminal-ui-note-status-progress ui))
         (when (and (application-reasoning-traces-p application)
                    (null presented-reasoning-text)
                    (plusp (length delta)))
           (setf reasoning-text (concatenate 'string reasoning-text delta))
           (terminal-ui-set-preview-rows
            ui
            (application--reasoning-preview-rows application reasoning-text))))
       :status-callback
       (lambda (status details)
         (case status
           (:provider-progress
            (terminal-ui-note-status-progress ui))
           (:provider-request-started
            (terminal-ui-set-preview-rows ui nil)
            (setf reasoning-text ""
                  presented-reasoning-text nil
                  stream-text ""
                  activity-label (application-thinking-label))
            (application-set-activity application activity-label))
           (:provider-retrying
            (let ((partial-output-p
                    (or stream-open-p
                        (plusp (length reasoning-text)))))
              (stream-flush)
              (terminal-ui-set-preview-rows ui nil)
              (when partial-output-p
                (application-present
                 application
                 (list
                  (terminal-span
                   ':hint
                   (format nil
                           "∙ provider stream interrupted; retrying ~D/~D"
                           (getf details :attempt)
                           (getf details :maximum-attempts))))))
              (setf reasoning-text ""
                    presented-reasoning-text nil
                    stream-text ""
                    stream-pending ""
                    stream-open-p nil
                    stream-renderer nil)
              (application-set-activity
               application
               (format nil "reconnecting ~D/~D in ~Ds"
                       (getf details :attempt)
                       (getf details :maximum-attempts)
                       (getf details :delay)))))
           (:provider-request-completed
            (reasoning-flush)
            (let ((completed-stream-text (and stream-open-p stream-text))
                  (completed-reasoning-text presented-reasoning-text))
              (stream-flush)
              (application-render-records
               application
               :streamed-assistant-text completed-stream-text
               :streamed-reasoning-text completed-reasoning-text)))
           (:tool-call-started
            (application-set-activity
             application
             (format nil "running ~A" (getf details :tool))))
           (:tool-call-completed
            (application-render-records application)
            (application-set-activity application activity-label))
           (:steering-applied
            (application-render-records application)
            (application-set-activity application activity-label))
           (:compaction-started
            (application-set-activity application "compacting the conversation"))
           (:compaction-completed
            (application-render-records application)
            (application-set-activity application activity-label))
           (:turn-completed
            (terminal-ui-set-preview-rows ui nil)
            (application-set-activity application nil))))
       :steering-callback steering-function
       :command-authorization-callback
       (lambda (command directory)
         (application-authorize-command application command directory))))))

(-> application--turn-final-text (provider-result) (option string))
(defun application--turn-final-text (result)
  "Return the joined assistant text of RESULT's final provider step."
  (let ((parts (loop for item in (provider-result-output-items result)
                     for text = (and (json-object-p item)
                                     (response-item-assistant-text item))
                     when text
                       collect text)))
    (when parts
      (format nil "~{~A~^~%~}" parts))))

(-> application--note-goal-turn (application provider-result) null)
(defun application--note-goal-turn (application result)
  "Mark the goal complete when RESULT's final message carries the marker."
  (let ((goal (application-goal application))
        (text (application--turn-final-text result)))
    (when (and goal
               (eq (getf goal :status) ':active)
               text
               (search +application-goal-complete-marker+ text))
      (setf (getf (application-goal application) :status) ':complete)
      (application--record-goal application)
      (application-present
       application
       (list (terminal-span ':success "✓ goal complete")
             (terminal-span ':plain (string #\Newline))
             (terminal-span ':dim
                            (format nil "  ~A" (getf goal :objective)))))))
  nil)

(-> application--run-turn
    (application (or string user-message-input)
     &key (:continuation-p boolean)
          (:steering-function (option function)))
    null)
(defun application--run-turn
    (application content &key continuation-p steering-function)
  "Persist and run one model turn for CONTENT while retaining editable input."
  (let* ((submission
           (etypecase content
             (string (user-message-input-create :text content))
             (user-message-input content)))
         (conversation (application-conversation application))
         (ui (application-ui application))
         (sequence (conversation-next-sequence conversation))
         (identifier (list :conversation
                           (conversation-identifier conversation)
                           sequence)))
    (unwind-protect
         (progn
           (terminal-ui-append-finalized
            ui
            identifier
            (if continuation-p
                (list (terminal-span ':hint "∙ goal continues"))
                (application--transcript-entry application
                                               :style ':user
                                               :header "❯ you"
                                               :body
                                               (user-message-input-preview
                                                submission))))
           (setf (application-rendered-sequence application) sequence)
           (application-set-activity application (application-thinking-label))
           (application--note-goal-turn
            application
            (agent-run-user-turn
             (application-agent application)
             submission
             :observer (application-agent-observer
                        application
                        :steering-function steering-function)
             :goal-context (application-goal-context application))))
      (terminal-ui-set-preview-rows ui nil)
      (application-set-activity application nil)
      (terminal-ui-stream-update ui :tail nil)
      (application-render-records application)))
  nil)

(-> application--run-goal-continuations
    (application &key (:steering-function (option function)))
    null)
(defun application--run-goal-continuations
    (application &key steering-function)
  "Run bounded automatic continuation turns while the session goal is active."
  (loop
    (let ((goal (application-goal application)))
      (unless (and goal (eq (getf goal :status) ':active))
        (return))
      (when (>= (getf goal :continuations)
                +application-goal-continuation-limit+)
        (setf (getf (application-goal application) :status) ':paused)
        (application--record-goal application)
        (application-present
         application
         (format nil
                 "The goal paused after ~D automatic continuations. ~
                  Use /goal resume or send a message to keep going."
                 +application-goal-continuation-limit+))
        (return))
      (incf (getf (application-goal application) :continuations))
      (application--run-turn application
                             +application-goal-continuation-prompt+
                             :continuation-p t
                             :steering-function steering-function)))
  nil)

(-> application-run-message
    (application (or string user-message-input)
     &key (:steering-function (option function)))
    null)
(defun application-run-message (application content &key steering-function)
  "Run one user turn for CONTENT plus any automatic goal continuation turns."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (setf (getf (application-goal application) :continuations) 0)))
  (application--run-turn application
                         content
                         :steering-function steering-function)
  (application--run-goal-continuations
   application
   :steering-function steering-function)
  nil)
