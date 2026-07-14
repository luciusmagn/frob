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
    :documentation "The identifier source for non-conversation terminal notices."))
  (:documentation "The globally rooted logical state and reconnectable resources of Autolith."))

(defvar *active-application* nil
  "The live application root retained in saved generations.")

(defvar *terminal-resize-pending-p* nil
  "True after SIGWINCH until the active UI recomputes its dimensions.")


;;;; -- Interactive Command Table --

(define-constant +application-commands+
  '((:name "/help"          :argument nil :description "show this reference")
    (:name "/new"           :argument nil :description "start a new conversation")
    (:name "/resume"        :argument nil :description "pick a saved conversation to resume")
    (:name "/conversations" :argument nil :description "list saved conversations")
    (:name "/auth"          :argument nil :description "authenticate Autolith with ChatGPT")
    (:name "/model"         :argument nil :description "pick the 5.6 model")
    (:name "/effort"        :argument nil :description "pick the reasoning effort")
    (:name "/goal"          :argument "OBJECTIVE" :description "set or view the session goal")
    (:name "/checkpoint"    :argument nil :description "save a retained live generation")
    (:name "/generations"   :argument nil :description "list retained generations")
    (:name "/rollback"      :argument nil :description "pick a generation for recovery")
    (:name "/status"        :argument nil :description "show usage and rate limits")
    (:name "/compact"       :argument nil :description "summarize earlier context now")
    (:name "/quit"          :argument nil :description "leave Autolith"))
  :test #'equal
  :documentation "The interactive commands offered by completion and /help.")


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

(-> application-create
    (configuration &key (:conversation-id (option string)))
    application)
(defun application-create (configuration &key conversation-id)
  "Create a connected application, loading CONVERSATION-ID when supplied."
  (configuration-ensure-directories configuration)
  (durable-mutations-load configuration)
  (let* ((overlay-failures (image-state-load configuration))
         (conversation (if conversation-id
                           (conversation-load-by-id configuration conversation-id)
                           (conversation-create configuration)))
         (provider (provider-create configuration))
         (registry (make-default-tool-registry))
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
                                     :ui ui)))
    (setf (application-overlay-failures application) overlay-failures)
    (application--load-goal application)
    application))

(-> application-reconnect
    (application &key (:conversation-id (option string)))
    application)
(defun application-reconnect (application &key conversation-id)
  "Reconnect retained APPLICATION resources, optionally selecting CONVERSATION-ID."
  (image-state-reconnect)
  (let* ((previous (application-configuration application))
         (retained-conversation (application-conversation application))
         (configuration
           (configuration-create
            :working-directory (uiop:getcwd)
            :model (configuration-model previous)
            :reasoning-effort (configuration-reasoning-effort previous)))
         (recovery-conversation-id
           (let ((value (uiop:getenv "AUTOLITH_RECOVERY_CONVERSATION_ID")))
             (and (non-empty-string-p value) value)))
         (overlay-failures nil)
         (conversation
           (cond
             (conversation-id
              (conversation-load-by-id configuration conversation-id))
             (recovery-conversation-id
              (conversation-load-by-id configuration recovery-conversation-id))
             ((conversation-persisted-p retained-conversation)
              (conversation-load-by-id
               configuration
               (conversation-identifier retained-conversation)))
             (t
              (conversation-create configuration))))
         (provider (provider-create configuration))
         (worker (lisp-worker-pool-create configuration))
         (registry (application-tool-registry application))
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
          (application-worker application) worker
          (application-agent application) agent
          (application-ui application) ui
          (application-rendered-sequence application)
          (if (and recovery-rendered-sequence
                   (string= (conversation-identifier conversation)
                            (or recovery-conversation-id "")))
              recovery-rendered-sequence
              0)
          (application-overlay-failures application) overlay-failures)
    (application--load-goal application)
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
    (application--load-goal application)
    application))


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
       (application-tool-result-entry tool application record)))
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

(-> application-render-records
    (application &key (:streamed-assistant-text (option string)))
    null)
(defun application-render-records (application &key streamed-assistant-text)
  "Append APPLICATION's not-yet-rendered durable transcript records once.

Assistant records are suppressed only when their joined durable text exactly
matches STREAMED-ASSISTANT-TEXT. Suppressed identifiers remain finalized so a
later conversation replay cannot duplicate their streamed transcript rows."
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
         (stream-match-p
           (and streamed-assistant-text
                assistant-texts
                (string= streamed-assistant-text
                         (format nil "~{~A~^~%~}" assistant-texts)))))
    (dolist (record records)
      (let ((sequence (getf (rest record) :seq)))
        (let ((identifier (list :conversation conversation-id sequence)))
          (if (and stream-match-p
                   (application--assistant-message-record-text record))
              (terminal-ui-mark-finalized ui identifier)
              (let ((entry (conversation-record-entry application record)))
                (when entry
                  (terminal-ui-append-finalized ui identifier entry)))))
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

(-> application-thinking-label () string)
(defun application-thinking-label ()
  "Return one interesting activity word for the next provider step."
  (if *application-thinking-words*
      (nth (random (length *application-thinking-words*))
           *application-thinking-words*)
      "pondering"))

(-> application-stream-status (application string string) null)
(defun application-stream-status (application label text)
  "Show LABEL and the bounded single-line tail of streaming TEXT."
  (let* ((safe (sanitize-text text :single-line-p t))
         (start (max 0 (- (length safe) 240))))
    (terminal-ui-set-status
     (application-ui application)
     (format nil "~A: ~A" label (subseq safe start))))
  nil)

(-> application-agent-observer (application) agent-observer)
(defun application-agent-observer (application)
  "Return a terminal observer streaming one APPLICATION turn as stable lines."
  (let ((ui (application-ui application))
        (activity-label (application-thinking-label))
        (reasoning-tail "")
        (stream-text "")
        (stream-pending "")
        (stream-open-p nil)
        (stream-renderer nil))
    (labels ((stream-text-delta (delta)
               "Commit DELTA's completed markdown rows and repaint the fluid tail."
               (when (plusp (length delta))
                 (setf stream-text
                       (concatenate 'string stream-text delta)
                       stream-pending
                       (sanitize-text
                        (concatenate 'string stream-pending delta)))
                 (let ((rows nil))
                   (unless stream-open-p
                     (setf stream-open-p t
                           reasoning-tail ""
                           stream-renderer (application--markdown-renderer
                                            application))
                     (terminal-ui-set-status ui nil)
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
         (let ((combined (concatenate 'string reasoning-tail delta)))
           (setf reasoning-tail
                 (subseq combined (max 0 (- (length combined) 500)))))
         (application-stream-status application activity-label reasoning-tail))
       :status-callback
       (lambda (status details)
         (case status
           (:provider-request-started
            (setf reasoning-tail ""
                  stream-text ""
                  activity-label (application-thinking-label))
            (terminal-ui-set-status ui activity-label))
           (:provider-request-completed
            (let ((completed-stream-text (and stream-open-p stream-text)))
              (stream-flush)
              (application-render-records
               application
               :streamed-assistant-text completed-stream-text)))
           (:tool-call-started
            (terminal-ui-set-status
             ui
             (format nil "running ~A" (getf details :tool))))
           (:tool-call-completed
            (application-render-records application)
            (terminal-ui-set-status ui activity-label))
           (:compaction-started
            (terminal-ui-set-status ui "compacting the conversation"))
           (:compaction-completed
            (application-render-records application)
            (terminal-ui-set-status ui activity-label))
           (:turn-completed
            (terminal-ui-set-status ui nil))))))))

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
    (application string &key (:continuation-p boolean))
    null)
(defun application--run-turn (application content &key continuation-p)
  "Persist and run one model turn for CONTENT while retaining editable input."
  (let* ((conversation (application-conversation application))
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
                                               :body content)))
           (setf (application-rendered-sequence application) sequence)
           (terminal-ui-set-status ui (application-thinking-label))
           (application--note-goal-turn
            application
            (agent-run-user-turn
             (application-agent application)
             content
             :observer (application-agent-observer application)
             :goal-context (application-goal-context application))))
      (terminal-ui-set-status ui nil)
      (terminal-ui-stream-update ui :tail nil)
      (application-render-records application)))
  nil)

(-> application--run-goal-continuations (application) null)
(defun application--run-goal-continuations (application)
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
                             :continuation-p t)))
  nil)

(-> application-run-message (application string) null)
(defun application-run-message (application content)
  "Run one user turn for CONTENT plus any automatic goal continuation turns."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (setf (getf (application-goal application) :continuations) 0)))
  (application--run-turn application content)
  (application--run-goal-continuations application)
  nil)
