(in-package #:frob)

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
    :type (option lisp-worker)
    :documentation "The reconnectable disposable Lisp worker.")
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
   (rendered-sequence
    :initform 0
    :accessor application-rendered-sequence
    :type integer
    :documentation "The last durable conversation sequence printed to scrollback.")
   (presentation-counter
    :initform 0
    :accessor application-presentation-counter
    :type integer
    :documentation "The identifier source for non-conversation terminal notices."))
  (:documentation "The globally rooted logical state and reconnectable resources of Frob."))

(defvar *active-application* nil
  "The live application root retained in saved generations.")

(defvar *terminal-resize-pending-p* nil
  "True after SIGWINCH until the active UI recomputes its width.")


;;;; -- Fatal Control Path --

(define-condition fatal-control-path-error (frob-error)
  ((cause
    :initarg :cause
    :reader fatal-control-path-error-cause
    :type serious-condition
    :documentation "The unexpected condition that made the active path untrustworthy.")
   (capsule-pathname
    :initarg :capsule-pathname
    :reader fatal-control-path-error-capsule-pathname
    :type pathname
    :documentation "The best-effort crash capsule written before recovery."))
  (:documentation "An unexpected active-agent failure requiring stable recovery."))

(-> application-write-crash-capsule (application serious-condition) pathname)
(defun application-write-crash-capsule (application condition)
  "Write a secret-free crash capsule for CONDITION and return its pathname."
  (let* ((configuration (application-configuration application))
         (identifier (make-identifier))
         (pathname (merge-pathnames
                    (make-pathname :name identifier :type "sexp")
                    (merge-pathnames "crashes/"
                                     (configuration-state-root configuration))))
         (commit
           (handler-case
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (self-git-command configuration '("rev-parse" "HEAD")))
             (error ()
               nil)))
         (backtrace
           (bounded-string
            (with-output-to-string (stream)
              (sb-debug:print-backtrace :stream stream :count 30))
            :limit 12000)))
    (generation--write-form-atomically
     pathname
     (list :crash
           :version 1
           :id identifier
           :time (get-universal-time)
           :condition-type (string (type-of condition))
           :condition (bounded-string (princ-to-string condition) :limit 3000)
           :backtrace backtrace
           :conversation-id
           (conversation-identifier (application-conversation application))
           :git-commit commit
           :journal-position
           (let ((journal (configuration-journal-path configuration)))
             (if (probe-file journal)
                 (with-open-file (stream journal
                                         :direction :input
                                         :element-type '(unsigned-byte 8))
                   (file-length stream))
                 0))))
    pathname))


;;;; -- Construction and Reconnection --

(-> terminal-current-columns () integer)
(defun terminal-current-columns ()
  "Return the current terminal width, falling back to the restrained default."
  (labels ((positive-integer-or-nil (value)
             "Parse VALUE as a positive integer, returning NIL on failure."
             (handler-case
                 (let ((parsed (and (non-empty-string-p value)
                                    (parse-integer value :junk-allowed t))))
                   (and parsed (plusp parsed) parsed))
               (error ()
                 nil))))
    (or (positive-integer-or-nil (uiop:getenv "COLUMNS"))
        (and (interactive-stream-p *terminal-io*)
             (handler-case
                 (positive-integer-or-nil
                  (uiop:run-program '("tput" "cols")
                                    :output :string
                                    :error-output :output))
               (error ()
                 nil)))
        +terminal-default-columns+)))

(-> application-create
    (configuration &key (:conversation-id (option string)))
    application)
(defun application-create (configuration &key conversation-id)
  "Create a connected application, loading CONVERSATION-ID when supplied."
  (configuration-ensure-directories configuration)
  (let* ((conversation (if conversation-id
                           (conversation-load-by-id configuration conversation-id)
                           (conversation-create configuration)))
         (provider (provider-create configuration))
         (registry (make-default-tool-registry))
         (worker (lisp-worker-create configuration))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (terminal-ui-create
              :terminal (stream-terminal-create
                         :columns (terminal-current-columns))
              :prompt "frob> ")))
    (make-instance 'application
                   :configuration configuration
                   :conversation conversation
                   :provider provider
                   :tool-registry registry
                   :worker worker
                   :agent agent
                   :ui ui)))

(-> application-reconnect (application) application)
(defun application-reconnect (application)
  "Reconnect APPLICATION's ephemeral resources after a retained core boots."
  (let* ((previous (application-configuration application))
         (configuration
           (configuration-create
            :working-directory (uiop:getcwd)
            :model (configuration-model previous)
            :reasoning-effort (configuration-reasoning-effort previous)))
         (conversation
           (conversation-load-by-id
            configuration
            (conversation-identifier (application-conversation application))))
         (provider (provider-create configuration))
         (worker (lisp-worker-create configuration))
         (registry (application-tool-registry application))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (terminal-ui-create
              :terminal (stream-terminal-create
                         :columns (terminal-current-columns))
              :prompt "frob> ")))
    (setf (application-configuration application) configuration
          (application-conversation application) conversation
          (application-provider application) provider
          (application-worker application) worker
          (application-agent application) agent
          (application-ui application) ui
          (application-rendered-sequence application) 0)
    application))

(-> application-prepare-checkpoint (application) application)
(defun application-prepare-checkpoint (application)
  "Detach APPLICATION's ephemeral object graph in a checkpoint saver child."
  (setf (application-provider application) nil
        (application-worker application) nil
        (application-agent application) nil
        (application-ui application) nil)
  application)

(-> application-install-conversation (application conversation) application)
(defun application-install-conversation (application conversation)
  "Make CONVERSATION active in APPLICATION and reconnect its agent coordinator."
  (let ((agent
          (agent-create
           :configuration (application-configuration application)
           :provider (application-provider application)
           :conversation conversation
           :tool-registry (application-tool-registry application)
           :worker (application-worker application))))
    (setf (application-conversation application) conversation
          (application-agent application) agent
          (application-rendered-sequence application) 0)
    application))


;;;; -- Transcript Projection --

(-> response-item-text (json-object) (option string))
(defun response-item-text (item)
  "Return a human-readable transcript entry for completed provider ITEM."
  (let ((type (json-get item "type")))
    (cond
      ((and (string= (or type "") "message")
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
               (format nil "assistant~%~{~A~^~%~}" parts))))))
      ((string= (or type "") "function_call")
       (format nil "tool request~%~A~@[~%~A~]"
               (function-call-canonical-name item)
               (let ((arguments (json-get item "arguments")))
                 (and (non-empty-string-p arguments)
                      (bounded-string arguments :limit 2000)))))
      (t
       nil))))

(-> conversation-record-text (list) (option string))
(defun conversation-record-text (record)
  "Return the terminal transcript text represented by durable RECORD."
  (case (first record)
    (:message
     (when (eq (getf (rest record) :role) :user)
       (format nil "you~%~A" (getf (rest record) :content))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (response-item-text (json-decode wire-json)))))
    (:tool-result
     (format nil "tool result: ~A (~(~A~))~%~A"
             (getf (rest record) :tool)
             (getf (rest record) :status)
             (getf (rest record) :output)))
    (otherwise
     nil)))

(-> application-present (application string) boolean)
(defun application-present (application text)
  "Append non-conversation TEXT once to APPLICATION's normal scrollback."
  (let ((identifier (incf (application-presentation-counter application))))
    (terminal-ui-append-finalized
     (application-ui application)
     (list :presentation identifier)
     text)))

(-> application-render-records (application) null)
(defun application-render-records (application)
  "Append APPLICATION's not-yet-rendered durable transcript records once."
  (let* ((conversation (application-conversation application))
         (conversation-id (conversation-identifier conversation)))
    (dolist (record (rest (conversation--read-records
                           (conversation-pathname conversation))))
      (let ((sequence (getf (rest record) :seq)))
        (when (and (integerp sequence)
                   (> sequence (application-rendered-sequence application)))
          (let ((text (conversation-record-text record)))
            (when text
              (terminal-ui-append-finalized
               (application-ui application)
               (list :conversation conversation-id sequence)
               text)))
          (setf (application-rendered-sequence application) sequence)))))
  nil)


;;;; -- Agent Presentation --

(-> application-stream-status (application string string) null)
(defun application-stream-status (application label text)
  "Show LABEL and the bounded single-line tail of streaming TEXT."
  (let* ((safe (terminal-sanitize-text text :single-line-p t))
         (start (max 0 (- (length safe) 240))))
    (terminal-ui-set-status
     (application-ui application)
     (format nil "~A: ~A" label (subseq safe start))))
  nil)

(-> application-agent-observer (application) agent-observer)
(defun application-agent-observer (application)
  "Return a terminal observer for one APPLICATION user turn."
  (let ((assistant-tail "")
        (reasoning-tail ""))
    (callback-agent-observer-create
     :text-callback
     (lambda (delta)
       (setf assistant-tail
             (bounded-string (concatenate 'string assistant-tail delta)
                             :limit 500))
       (application-stream-status application "assistant" assistant-tail))
     :reasoning-callback
     (lambda (delta)
       (setf reasoning-tail
             (bounded-string (concatenate 'string reasoning-tail delta)
                             :limit 500))
       (application-stream-status application "thinking" reasoning-tail))
     :status-callback
     (lambda (status details)
       (case status
         (:provider-request-started
          (terminal-ui-set-status (application-ui application) "thinking"))
         (:tool-call-started
          (terminal-ui-set-status
           (application-ui application)
           (format nil "running ~A" (getf details :tool))))
         (:tool-call-completed
          (terminal-ui-set-status
           (application-ui application)
           (format nil "completed ~A" (getf details :tool))))
         (:turn-completed
          (terminal-ui-set-status (application-ui application) nil)))))))

(-> application-run-message (application string) null)
(defun application-run-message (application content)
  "Persist and run one model turn for CONTENT, presenting durable results once."
  (let* ((conversation (application-conversation application))
         (sequence (conversation-next-sequence conversation))
         (identifier (list :conversation
                           (conversation-identifier conversation)
                           sequence)))
    (terminal-ui-append-finalized
     (application-ui application)
     identifier
     (format nil "you~%~A" content))
    (setf (application-rendered-sequence application) sequence)
    (unwind-protect
         (progn
           (terminal-ui-set-status (application-ui application) "thinking")
           (agent-run-user-turn
            (application-agent application)
            content
            :observer (application-agent-observer application)))
      (terminal-ui-set-status (application-ui application) nil)
      (application-render-records application)))
  nil)


;;;; -- Interactive Commands --

(-> application-help () string)
(defun application-help ()
  "Return the concise interactive command reference."
  "/help                 show this reference
/new                  start a new conversation
/resume ID            load a saved conversation
/conversations         list saved conversations
/auth                  authenticate Frob with ChatGPT
/checkpoint            save a retained live generation
/generations           list retained generations
/rollback ID           select a generation for recovery
/quit                  leave Frob")

(-> application-list-conversations (application) string)
(defun application-list-conversations (application)
  "Return known conversation identifiers newest first."
  (let ((pathnames (conversation-list (application-configuration application))))
    (if pathnames
        (format nil "conversations~%~{~A~%~}"
                (mapcar #'pathname-name pathnames))
        "No saved conversations exist.")))

(-> application-authenticate (application) null)
(defun application-authenticate (application)
  "Run Frob-owned device authentication outside raw terminal mode."
  (let* ((ui (application-ui application))
         (provider (application-provider application)))
    (unless (typep provider 'codex-subscription-provider)
      (error 'authentication-error
             :message "The active provider does not support ChatGPT device login."))
    (terminal-ui-stop ui)
    (unwind-protect
         (device-authentication-login
          (device-authentication-client-create)
          (provider-credential-manager provider)
          :stream *standard-output*
          :open-browser-p t)
      (terminal-ui-start ui))
    (application-present application "ChatGPT authentication was saved by Frob."))
  nil)

(-> application-checkpoint (application) null)
(defun application-checkpoint (application)
  "Begin a non-stopping retained generation for APPLICATION."
  (terminal-ui-set-status (application-ui application)
                          "checking source before checkpoint")
  (unwind-protect
       (let ((generation
               (checkpoint-create
                (checkpoint-backend-create
                 (application-configuration application)
                 (application-worker application)))))
         (application-present
          application
          (format nil "Checkpoint ~A is publishing in process ~D."
                  (generation-identifier generation)
                  (generation-coordinator-pid generation))))
    (terminal-ui-set-status (application-ui application) nil))
  nil)

(-> application-command (application string) keyword)
(defun application-command (application input)
  "Execute slash command INPUT for APPLICATION and return its loop action."
  (let* ((parts (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string input :separator '(#\Space #\Tab))))
         (command (string-downcase (or (first parts) "")))
         (argument (second parts))
         (configuration (application-configuration application)))
    (cond
      ((member command '("/quit" "/exit") :test #'string=)
       :quit)
      ((string= command "/help")
       (application-present application (application-help))
       :continue)
      ((string= command "/new")
       (application-install-conversation application
                                         (conversation-create configuration))
       (application-present
        application
        (format nil "Started conversation ~A."
                (conversation-identifier
                 (application-conversation application))))
       :continue)
      ((string= command "/resume")
       (unless (non-empty-string-p argument)
         (error 'conversation-error
                :message "Usage: /resume ID"
                :pathname (configuration-conversation-root configuration)
                :sequence nil))
       (application-install-conversation
        application
        (conversation-load-by-id configuration argument))
       (application-render-records application)
       :continue)
      ((string= command "/conversations")
       (application-present application
                            (application-list-conversations application))
       :continue)
      ((string= command "/auth")
       (application-authenticate application)
       :continue)
      ((string= command "/checkpoint")
       (application-checkpoint application)
       :continue)
      ((string= command "/generations")
       (application-present application
                            (generation-render-list configuration))
       :continue)
      ((string= command "/rollback")
       (unless (non-empty-string-p argument)
         (error 'checkpoint-error
                :message "Usage: /rollback ID"
                :stage ':selection
                :pathname nil))
       (let ((generation (generation-find configuration argument)))
         (unless generation
           (error 'checkpoint-error
                  :message (format nil "Unknown retained generation ~A." argument)
                  :stage ':selection
                  :pathname nil))
         (generation-select configuration generation)
         (application-present
          application
          (format nil "Selected ~A. Run frob --recovery to boot it." argument)))
       :continue)
      (t
       (application-present application
                            (format nil "Unknown command ~A. Use /help." command))
       :continue))))

(-> application-handle-input (application string) keyword)
(defun application-handle-input (application input)
  "Handle submitted INPUT and return :CONTINUE or :QUIT."
  (cond
    ((not (non-empty-string-p input))
     :continue)
    ((uiop:string-prefix-p "//" input)
     (application-run-message application (subseq input 1))
     :continue)
    ((uiop:string-prefix-p "/" input)
     (application-command application input))
    (t
     (application-run-message application input)
     :continue)))


;;;; -- Application Lifecycle --

(-> application-banner (application) string)
(defun application-banner (application)
  "Return APPLICATION's restrained startup banner and security notice."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (format nil
            "Frob ~A | ~A | effort ~A~%conversation ~A~%~%Frob executes model-generated code with your user privileges. It is not a security sandbox.~%Use /help for commands."
            +frob-version+
            (configuration-model configuration)
            (configuration-reasoning-effort configuration)
            (conversation-identifier conversation))))

(-> application-handle-expected-error (application frob-error) null)
(defun application-handle-expected-error (application condition)
  "Present expected CONDITION without abandoning APPLICATION's active path."
  (terminal-ui-set-status (application-ui application) nil)
  (application-render-records application)
  (application-present
   application
   (if (typep condition 'credentials-unavailable)
       (format nil "error~%~A~%Use /auth to authenticate Frob directly."
               condition)
       (format nil "error~%~A" condition)))
  nil)

(-> application-update-size (application) null)
(defun application-update-size (application)
  "Apply a pending terminal resize to APPLICATION's unfinished rows."
  (when *terminal-resize-pending-p*
    (setf *terminal-resize-pending-p* nil)
    (terminal-ui-resize (application-ui application)
                        (terminal-current-columns)))
  nil)

(-> application-run (application) null)
(defun application-run (application)
  "Run APPLICATION until explicit exit, always restoring terminal and worker state."
  (let ((ui (application-ui application))
        (worker (application-worker application)))
    (sb-sys:enable-interrupt
     sb-unix:sigwinch
     (lambda (signal code context)
       (declare (ignore signal code context))
       (setf *terminal-resize-pending-p* t)))
    (unwind-protect
         (with-terminal-ui (active-ui ui)
           (declare (ignore active-ui))
           (application-present application (application-banner application))
           (application-render-records application)
           (loop
             (application-update-size application)
             (let ((event (terminal-ui-read-event ui)))
               (multiple-value-bind (action payload)
                   (terminal-ui-process-event ui event)
                 (case action
                   (:submit
                    (handler-case
                        (when (eq (application-handle-input application payload) :quit)
                          (return))
                      (frob-error (condition)
                        (application-handle-expected-error application condition))
                      (serious-condition (condition)
                        (let ((capsule
                                (application-write-crash-capsule
                                 application condition)))
                          (error 'fatal-control-path-error
                                 :message "The active agent path failed unexpectedly."
                                 :cause condition
                                 :capsule-pathname capsule)))))
                   ((:end-of-input :interrupt)
                    (return)))))))
      (sb-sys:enable-interrupt sb-unix:sigwinch :default)
      (when worker
        (lisp-worker-stop worker))))
  nil)


;;;; -- Command-Line Entry --

(-> main-usage () string)
(defun main-usage ()
  "Return the command-line usage text."
  "Usage: frob [--resume ID]
       frob --auth
       frob --version
       frob --recovery [--generation ID | --list]")

(-> main-authenticate (configuration) null)
(defun main-authenticate (configuration)
  "Run Frob-owned device authentication without starting the conversation UI."
  (configuration-ensure-directories configuration)
  (device-authentication-login
   (device-authentication-client-create)
   (credential-manager-create configuration)
   :stream *standard-output*
   :open-browser-p t)
  (format t "~&ChatGPT authentication was saved by Frob.~%")
  nil)

(-> main (list) null)
(defun main (arguments)
  "Run the Frob command described by ARGUMENTS."
  (cond
    ((member "--worker" arguments :test #'string=)
     (worker-main))
    ((member "--version" arguments :test #'string=)
     (format t "frob ~A~%" +frob-version+))
    ((or (member "--help" arguments :test #'string=)
         (member "-h" arguments :test #'string=))
     (format t "~A~%" (main-usage)))
    (t
     (let* ((configuration (configuration-create))
            (resume-position (position "--resume" arguments :test #'string=))
            (resume-id (and resume-position
                            (nth (1+ resume-position) arguments))))
       (when (and resume-position (not (non-empty-string-p resume-id)))
         (error 'configuration-error :message "--resume requires an identifier."))
       (cond
         ((member "--auth" arguments :test #'string=)
          (main-authenticate configuration))
         (t
          (setf *active-application*
                (if (typep *active-application* 'application)
                    (application-reconnect *active-application*)
                    (application-create configuration :conversation-id resume-id)))
          (when (member "--simulate-crash" arguments :test #'string=)
            (let ((capsule
                    (application-write-crash-capsule
                     *active-application*
                     (make-condition 'simple-error
                                     :format-control "Intentional recovery test."
                                     :format-arguments nil))))
              (format *error-output* "Intentional crash capsule: ~A~%" capsule)
              (uiop:quit 70)))
          (handler-case
              (application-run *active-application*)
            (fatal-control-path-error (condition)
              (format *error-output*
                      "Frob entered recovery after a fatal error. Capsule: ~A~%"
                      (fatal-control-path-error-capsule-pathname condition))
              (uiop:quit 70))))))))
  nil)
