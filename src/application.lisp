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


;;;; -- Interactive Command Table --

(define-constant +application-commands+
  '((:name "/help"          :argument nil :description "show this reference")
    (:name "/new"           :argument nil :description "start a new conversation")
    (:name "/resume"        :argument nil :description "pick a saved conversation to resume")
    (:name "/conversations" :argument nil :description "list saved conversations")
    (:name "/auth"          :argument nil :description "authenticate Frob with ChatGPT")
    (:name "/checkpoint"    :argument nil :description "save a retained live generation")
    (:name "/generations"   :argument nil :description "list retained generations")
    (:name "/rollback"      :argument nil :description "pick a generation for recovery")
    (:name "/quit"          :argument nil :description "leave Frob"))
  :test #'equal
  :documentation "The interactive commands offered by completion and /help.")


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

(define-constant +application-prompt+ "❯ "
  :test #'string=
  :documentation "The styled input prompt shown on the live editor row.")

(define-constant +application-placeholder+
  "Ask Frob anything. Type /help for commands."
  :test #'string=
  :documentation "The dim hint shown on the prompt row while input is empty.")

(-> application-terminal-ui-create () terminal-ui)
(defun application-terminal-ui-create ()
  "Create the standard interactive terminal UI at the current terminal width."
  (terminal-ui-create
   :terminal (stream-terminal-create :columns (terminal-current-columns))
   :prompt +application-prompt+
   :placeholder +application-placeholder+
   :completions +application-commands+))

(-> application-create
    (configuration &key (:conversation-id (option string)))
    application)
(defun application-create (configuration &key conversation-id)
  "Create a connected application, loading CONVERSATION-ID when supplied."
  (configuration-ensure-directories configuration)
  (durable-mutations-load configuration)
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
         (ui (application-terminal-ui-create)))
    (make-instance 'application
                   :configuration configuration
                   :conversation conversation
                   :provider provider
                   :tool-registry registry
                   :worker worker
                   :agent agent
                   :ui ui)))

(-> application-reconnect
    (application &key (:conversation-id (option string)))
    application)
(defun application-reconnect (application &key conversation-id)
  "Reconnect retained APPLICATION resources, optionally selecting CONVERSATION-ID."
  (let* ((previous (application-configuration application))
         (configuration
           (configuration-create
            :working-directory (uiop:getcwd)
            :model (configuration-model previous)
            :reasoning-effort (configuration-reasoning-effort previous)))
         (recovery-conversation-id
           (uiop:getenv "FROB_RECOVERY_CONVERSATION_ID"))
         (selected-conversation-id
           (or conversation-id
               (and (non-empty-string-p recovery-conversation-id)
                    recovery-conversation-id)
               (conversation-identifier
                (application-conversation application))))
         (conversation
           (conversation-load-by-id
            configuration
            selected-conversation-id))
         (provider (provider-create configuration))
         (worker (lisp-worker-create configuration))
         (registry (application-tool-registry application))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (application-terminal-ui-create))
         (recovery-rendered-sequence
           (handler-case
               (let ((value (uiop:getenv "FROB_RECOVERY_RENDERED_SEQUENCE")))
                 (and (non-empty-string-p value)
                      (parse-integer value :junk-allowed nil)))
             (error ()
               nil))))
    (setf (application-configuration application) configuration
          (application-conversation application) conversation
          (application-provider application) provider
          (application-worker application) worker
          (application-agent application) agent
          (application-ui application) ui
          (application-rendered-sequence application)
          (if (and recovery-rendered-sequence
                   (string= selected-conversation-id
                            (or recovery-conversation-id "")))
              recovery-rendered-sequence
              0))
    application))

(defmethod checkpoint-detach-state ((application application))
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

(define-constant +application-tool-output-lines+ 12
  :documentation "The maximum tool output lines shown in the terminal transcript.")

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
          (terminal--wrap-text (string-right-trim
                                '(#\Space #\Tab #\Newline #\Return)
                                (terminal-sanitize-text text))
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
                                    (terminal-sanitize-text text))))
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
                          (terminal--text-width header)))
            (safe-detail (terminal-sanitize-text detail :single-line-p t)))
        (when (> available 1)
          (let ((visible (terminal--prefix-within-width safe-detail
                                                        (1- available))))
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

(-> application--bounded-tool-output (t) (option string))
(defun application--bounded-tool-output (output)
  "Return displayable tool OUTPUT bounded to a readable number of lines."
  (let ((text (bounded-string output :limit 2000)))
    (when (non-empty-string-p text)
      (let ((lines (uiop:split-string text :separator '(#\Newline))))
        (if (<= (length lines) +application-tool-output-lines+)
            text
            (format nil "~{~A~^~%~}~%… +~D more lines"
                    (subseq lines 0 +application-tool-output-lines+)
                    (- (length lines) +application-tool-output-lines+)))))))

(-> response-item-entry (application json-object) (option list))
(defun response-item-entry (application item)
  "Return a styled transcript entry for completed provider ITEM."
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
               (append (list (terminal-span ':brand "● frob")
                             (terminal-span ':plain (string #\Newline)))
                       (application--markdown-body
                        application
                        (format nil "~{~A~^~%~}" parts))))))))
      ((string= (or type "") "function_call")
       (application--transcript-entry
        application
        :style ':tool
        :header (format nil "▸ ~A" (function-call-canonical-name item))
        :detail (let ((arguments (json-get item "arguments")))
                  (and (non-empty-string-p arguments)
                       arguments))))
      (t
       nil))))

(-> conversation-record-entry (application list) (option list))
(defun conversation-record-entry (application record)
  "Return the styled transcript entry represented by durable RECORD."
  (case (first record)
    (:message
     (when (eq (getf (rest record) :role) :user)
       (application--transcript-entry application
                                      :style ':user
                                      :header "❯ you"
                                      :body (getf (rest record) :content))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (response-item-entry application (json-decode wire-json)))))
    (:tool-result
     (let ((success-p (eq (getf (rest record) :status) ':ok)))
       (application--transcript-entry
        application
        :style (if success-p
                   ':success
                   ':failure)
        :header (format nil "~:[✗ ~A failed~;✓ ~A~]"
                        success-p
                        (getf (rest record) :tool))
        :body (application--bounded-tool-output (getf (rest record) :output))
        :body-style ':dim)))
    (otherwise
     nil)))

(-> application-present (application (or string list)) boolean)
(defun application-present (application entry)
  "Append non-conversation ENTRY once to APPLICATION's normal scrollback."
  (let ((identifier (incf (application-presentation-counter application))))
    (terminal-ui-append-finalized
     (application-ui application)
     (list :presentation identifier)
     entry)))

(-> application--assistant-message-record-p (list) boolean)
(defun application--assistant-message-record-p (record)
  "Return true when durable RECORD carries an assistant message item."
  (and (eq (first record) :provider-item)
       (let ((wire-json (getf (rest record) :wire-json)))
         (and (stringp wire-json)
              (let ((item (json-decode wire-json)))
                (and (json-object-p item)
                     (string= (or (json-get item "type") "") "message")
                     (string= (or (json-get item "role") "") "assistant")))))))

(-> application-render-records
    (application &key (:skip-assistant-messages-p boolean))
    null)
(defun application-render-records (application &key skip-assistant-messages-p)
  "Append APPLICATION's not-yet-rendered durable transcript records once.

When SKIP-ASSISTANT-MESSAGES-P is true, new assistant message records advance
the rendered sequence without emitting entries, because their text already
streamed into the transcript."
  (let* ((conversation (application-conversation application))
         (conversation-id (conversation-identifier conversation)))
    (dolist (record (rest (conversation--read-records
                           (conversation-pathname conversation))))
      (let ((sequence (getf (rest record) :seq)))
        (when (and (integerp sequence)
                   (> sequence (application-rendered-sequence application)))
          (unless (and skip-assistant-messages-p
                       (application--assistant-message-record-p record))
            (let ((entry (conversation-record-entry application record)))
              (when entry
                (terminal-ui-append-finalized
                 (application-ui application)
                 (list :conversation conversation-id sequence)
                 entry))))
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
  "Return a terminal observer streaming one APPLICATION turn as stable lines."
  (let ((ui (application-ui application))
        (reasoning-tail "")
        (stream-pending "")
        (stream-open-p nil)
        (stream-renderer nil))
    (labels ((stream-text-delta (delta)
               "Commit DELTA's completed markdown rows and repaint the fluid tail."
               (setf stream-pending
                     (terminal-sanitize-text
                      (concatenate 'string stream-pending delta)))
               (let ((rows nil))
                 (unless stream-open-p
                   (setf stream-open-p t
                         reasoning-tail ""
                         stream-renderer (application--markdown-renderer
                                          application))
                   (terminal-ui-set-status ui nil)
                   (push (list (terminal-span ':brand "● frob")) rows))
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
                   (terminal-ui-stream-update ui
                                              :rows (append rows overflow-rows)
                                              :tail tail))))

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
         (let ((combined (concatenate 'string reasoning-tail delta)))
           (setf reasoning-tail
                 (subseq combined (max 0 (- (length combined) 500)))))
         (application-stream-status application "thinking" reasoning-tail))
       :status-callback
       (lambda (status details)
         (case status
           (:provider-request-started
            (setf reasoning-tail "")
            (terminal-ui-set-status ui "thinking"))
           (:provider-request-completed
            (let ((streamed-p stream-open-p))
              (stream-flush)
              (application-render-records
               application
               :skip-assistant-messages-p streamed-p)))
           (:tool-call-started
            (terminal-ui-set-status
             ui
             (format nil "running ~A" (getf details :tool))))
           (:tool-call-completed
            (application-render-records application)
            (terminal-ui-set-status ui "thinking"))
           (:turn-completed
            (terminal-ui-set-status ui nil))))))))

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
     (application--transcript-entry application
                                    :style ':user
                                    :header "❯ you"
                                    :body content))
    (setf (application-rendered-sequence application) sequence)
    (unwind-protect
         (progn
           (terminal-ui-set-status (application-ui application) "thinking")
           (agent-run-user-turn
            (application-agent application)
            content
            :observer (application-agent-observer application)))
      (terminal-ui-set-status (application-ui application) nil)
      (terminal-ui-stream-update (application-ui application) :tail nil)
      (application-render-records application)))
  nil)
