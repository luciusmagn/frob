(in-package #:autolith)

;;;; -- Interactive Commands --

(-> application-help () string)
(defun application-help ()
  "Return the concise interactive command reference."
  (let ((label-width
          (loop for entry in +application-commands+
                maximize (length (terminal-completion-label entry)))))
    (format nil "~{~A~^~%~}"
            (loop for entry in +application-commands+
                  collect (format nil "~vA  ~A"
                                  label-width
                                  (terminal-completion-label entry)
                                  (getf entry :description))))))

(-> application-list-conversations (application) string)
(defun application-list-conversations (application)
  "Return saved conversations newest first with their times and origins."
  (let ((items (application--conversation-items application)))
    (if items
        (format nil "conversations~%~{~A~%~}"
                (loop for item in items
                      collect (format nil "~A  ~A"
                                      (getf item :name)
                                      (getf item :description))))
        "No saved conversations exist.")))


;;;; -- Usage Status --

(-> application--conversation-usage (application) list)
(defun application--conversation-usage (application)
  "Return summed (:input N :output N :total N) usage for the active conversation."
  (let ((input 0)
        (output 0)
        (total 0))
    (labels ((usage-count (usage key)
               "Return the integer usage value stored under KEY, or zero."
               (let ((value (second (assoc key usage :test #'string=))))
                 (if (integerp value)
                     value
                     0))))
      (dolist (record (rest (conversation--read-records
                             (conversation-pathname
                              (application-conversation application)))))
        (when (eq (first record) :provider)
          (let ((usage (getf (getf (rest record) :metadata) :usage)))
            (when (listp usage)
              (incf input (usage-count usage "input_tokens"))
              (incf output (usage-count usage "output_tokens"))
              (incf total (usage-count usage "total_tokens")))))))
    (list :input input :output output :total total)))

(-> application--token-count-description (integer) string)
(defun application--token-count-description (count)
  "Return COUNT as a compact human-readable token quantity."
  (cond
    ((< count 1000)
     (format nil "~D" count))
    ((< count 1000000)
     (format nil "~,1FK" (/ count 1000)))
    (t
     (format nil "~,2FM" (/ count 1000000)))))

(-> application--window-label ((option integer) string) string)
(defun application--window-label (minutes fallback)
  "Return the human name of a MINUTES-long rate limit window."
  (labels ((approximately-p (expected)
             "Return true when MINUTES is within five percent of EXPECTED."
             (and minutes
                  (<= (* expected 95/100) minutes (* expected 105/100)))))
    (cond
      ((approximately-p 300) "5h")
      ((approximately-p 1440) "daily")
      ((approximately-p 10080) "weekly")
      ((approximately-p 43200) "monthly")
      ((null minutes) fallback)
      ((>= minutes 60) (format nil "~Dh" (round minutes 60)))
      (t (format nil "~Dm" minutes)))))

(-> application--reset-description ((option integer)) (option string))
(defun application--reset-description (resets-at)
  "Return when RESETS-AT universal time occurs, as a compact local time."
  (when resets-at
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time resets-at)
      (declare (ignore second))
      (if (< (- resets-at (get-universal-time)) 86400)
          (format nil "~2,'0D:~2,'0D" hour minute)
          (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
                  year month date hour minute)))))

(-> application--limit-spans (string list) list)
(defun application--limit-spans (fallback-label window)
  "Return one rate limit WINDOW as an aligned transcript row with a usage bar."
  (let* ((used (min 100 (max 0 (getf window :used-percent))))
         (left (- 100 used))
         (cells 20)
         (filled (round (* cells left) 100))
         (bar (concatenate 'string
                           (make-string filled :initial-element #\█)
                           (make-string (- cells filled) :initial-element #\░))))
    (list (terminal-span :dim
                         (format nil "  ~13A "
                                 (format nil "~A limit"
                                         (application--window-label
                                          (getf window :window-minutes)
                                          fallback-label))))
          (terminal-span :plain (format nil "[~A] ~D% left" bar (round left)))
          (terminal-span :dim
                         (format nil "~@[ (resets ~A)~]~%"
                                 (application--reset-description
                                  (getf window :resets-at)))))))

(-> application-status-entry (application) list)
(defun application-status-entry (application)
  "Return the styled /status summary of APPLICATION's session and rate limits."
  (let* ((configuration (application-configuration application))
         (provider (application-provider application))
         (snapshot (and provider (provider-rate-limits provider)))
         (usage (application--conversation-usage application)))
    (append
     (list (terminal-span :brand "autolith")
           (terminal-span :dim (format nil " v~A~%" +autolith-version+)))
     (application--field-spans "model"
                               (format nil "~A (effort ~A)"
                                       (configuration-model configuration)
                                       (configuration-reasoning-effort
                                        configuration)))
     (application--field-spans "reasoning trace"
                               (if (application-reasoning-traces-p application)
                                   "visible summaries"
                                   "hidden"))
     (application--field-spans "conversation"
                               (conversation-identifier
                                (application-conversation application)))
     (application--field-spans "workspace"
                               (or (application--abbreviated-directory
                                    (namestring
                                     (configuration-working-directory
                                      configuration)))
                                   ""))
     (application--field-spans "path"
                               "standard (the fast path is never requested)")
     (application--field-spans "web search"
                               (configuration-web-search-mode configuration))
     (application--field-spans "goal"
                               (let ((goal (application-goal application)))
                                 (if goal
                                     (format nil "~(~A~): ~A"
                                             (getf goal :status)
                                             (getf goal :objective))
                                     "none")))
     (application--field-spans "token usage"
                               (format nil "~A total (~A input + ~A output)"
                                       (application--token-count-description
                                        (getf usage :total))
                                       (application--token-count-description
                                        (getf usage :input))
                                       (application--token-count-description
                                        (getf usage :output))))
     (application--field-spans
      "context"
      (let ((used (conversation-last-total-tokens
                   (application-conversation application)))
            (window (configuration-context-window configuration)))
        (format nil "~A of ~A used (~D%), compacts at ~D%"
                (application--token-count-description used)
                (application--token-count-description window)
                (round (* 100 used) (max 1 window))
                (configuration-compaction-threshold-percent configuration))))
     (cond
       ((null snapshot)
        (list (terminal-span :dim
                             "  No rate limit data yet; send a message first.")))
       (t
        (append
         (let ((primary (getf snapshot :primary)))
           (when primary
             (application--limit-spans "primary" primary)))
         (let ((secondary (getf snapshot :secondary)))
           (when secondary
             (application--limit-spans "weekly" secondary)))))))))


;;;; -- Interactive Pickers --

(-> application--calendar-description (integer) string)
(defun application--calendar-description (universal-time)
  "Return UNIVERSAL-TIME as a compact local calendar description."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year month date hour minute)))

(-> application--abbreviated-directory ((option string)) (option string))
(defun application--abbreviated-directory (namestring)
  "Return NAMESTRING with the user home directory abbreviated to a tilde."
  (when (non-empty-string-p namestring)
    (let ((home (namestring (user-homedir-pathname))))
      (if (and (uiop:string-prefix-p home namestring)
               (> (length namestring) (length home)))
          (concatenate 'string "~/" (subseq namestring (length home)))
          namestring))))

(define-constant +conversation-preview-width+ 48
  :documentation "The cell width of the newest-message excerpt in pickers.")

(-> application--conversation-preview (pathname) (option string))
(defun application--conversation-preview (pathname)
  "Return a one-line excerpt of PATHNAME's newest user or assistant message."
  (handler-case
      (let ((preview nil))
        (dolist (record (rest (conversation--read-records pathname)))
          (case (first record)
            (:message
             (let ((content (getf (rest record) :content)))
               (when (and (eq (getf (rest record) :role) :user)
                          (stringp content))
                 (setf preview content))))
            (:provider-item
             (let ((wire-json (getf (rest record) :wire-json)))
               (when (stringp wire-json)
                 (let ((item (json-decode wire-json)))
                   (when (json-object-p item)
                     (let ((text (response-item-assistant-text item)))
                       (when text
                         (setf preview text))))))))))
        (when preview
          (text-cell-prefix
           (sanitize-text preview :single-line-p t)
           +conversation-preview-width+)))
    (error ()
      nil)))

(-> application--conversation-items (application) list)
(defun application--conversation-items (application)
  "Return picker items for saved conversations, newest first."
  (let ((current (conversation-identifier
                  (application-conversation application))))
    (loop for pathname in (conversation-list
                           (application-configuration application))
          for identifier = (pathname-name pathname)
          for header = (conversation-peek-header pathname)
          collect (list :name identifier
                        :argument nil
                        :description
                        (format nil "~A~@[, ~A~]~:[~;, current~]~@[ · ~A~]"
                                (application--calendar-description
                                 (or (file-write-date pathname) 0))
                                (application--abbreviated-directory
                                 (getf (rest header) :directory))
                                (string= identifier current)
                                (application--conversation-preview
                                 pathname))))))

(-> application--effort-items (application) list)
(defun application--effort-items (application)
  "Return picker items for the supported reasoning efforts."
  (let ((current (configuration-reasoning-effort
                  (application-configuration application))))
    (loop for effort in +supported-reasoning-efforts+
          collect (list :name effort
                        :argument nil
                        :description (if (string= effort current)
                                         "current"
                                         "")))))

(-> application--install-configuration (application configuration) null)
(defun application--install-configuration (application configuration)
  "Switch APPLICATION to CONFIGURATION, reconnecting its provider and agent."
  (let* ((previous-provider (application-provider application))
         (previous-agent (application-agent application))
         (provider
           (if previous-provider
               (provider-with-configuration previous-provider configuration)
               (provider-create configuration)))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation (application-conversation
                                             application)
                              :tool-registry (application-tool-registry
                                              application)
                              :worker (application-worker application)
                              :maximum-provider-steps
                              (if previous-agent
                                  (agent-maximum-provider-steps previous-agent)
                                  +default-maximum-provider-steps+)
                              :provider-step-warning
                              (if previous-agent
                                  (agent-provider-step-warning previous-agent)
                                  +default-provider-step-warning+)
                              :maximum-tool-calls
                              (if previous-agent
                                  (agent-maximum-tool-calls previous-agent)
                                  +default-maximum-tool-calls+))))
    (setf (application-configuration application) configuration
          (application-provider application) provider
          (application-agent application) agent))
  nil)

(-> application-set-reasoning-effort (application string) null)
(defun application-set-reasoning-effort (application effort)
  "Switch APPLICATION to reasoning EFFORT for this session's next turns."
  (application--install-configuration
   application
   (configuration-with-reasoning-effort (application-configuration application)
                                        effort)))

(-> application-set-reasoning-traces (application boolean) null)
(defun application-set-reasoning-traces (application enabled-p)
  "Persist and apply whether future reasoning summaries are visible."
  (preferences-set-reasoning-traces
   (application-configuration application)
   enabled-p)
  (let ((provider (application-provider application)))
    (when provider
      (provider-set-reasoning-summaries provider enabled-p)))
  (setf (application-reasoning-traces-p application) enabled-p)
  (unless enabled-p
    (terminal-ui-set-preview-rows (application-ui application) nil))
  nil)

(-> application-trace-command (application (option string)) null)
(defun application-trace-command (application argument)
  "Show or change APPLICATION's visible reasoning-summary setting."
  (let ((mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil
                "Reasoning summaries are ~:[hidden~;shown~]. This setting ~
                 persists across restarts."
                (application-reasoning-traces-p application))))
      ((string= mode "on")
       (application-set-reasoning-traces application t)
       (application-present
        application
        "Visible reasoning summaries are enabled and saved."))
      ((string= mode "off")
       (application-set-reasoning-traces application nil)
       (application-present application "Reasoning summaries are hidden and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /trace on or /trace off."))))
  nil)

(-> application--model-items (application) list)
(defun application--model-items (application)
  "Return picker items for the supported 5.6 model family."
  (let ((current (configuration-model
                  (application-configuration application))))
    (loop for model in +supported-models+
          collect (list :name model
                        :argument nil
                        :description (if (string= model current)
                                         "current"
                                         "")))))

(-> application-set-model (application string) null)
(defun application-set-model (application model)
  "Switch APPLICATION to MODEL for this session's next turns."
  (application--install-configuration
   application
   (configuration-with-model (application-configuration application) model)))

;;;; -- Manual Compaction --

(-> application-compact (application) null)
(defun application-compact (application)
  "Manually compact the active conversation into a durable summary."
  (let ((agent (application-agent application))
        (conversation (application-conversation application)))
    (unless agent
      (error 'configuration-error
             :message "No connected agent can compact the conversation."))
    (if (null (conversation-input-items conversation))
        (application-present application "Nothing to compact yet.")
        (progn
          (unwind-protect
               (agent-compact-conversation
                agent
                (application-agent-observer application))
            (terminal-ui-set-status (application-ui application) nil))
          (application-render-records application)
          (application-present
           application
           "Compacted; a summary now stands in for the earlier history."))))
  nil)


;;;; -- Session Goal Command --

(-> application--goal-remainder (string) string)
(defun application--goal-remainder (input)
  "Return INPUT's trimmed text after the /goal command word."
  (let ((space (position-if (lambda (character)
                              (find character '(#\Space #\Tab)))
                            input)))
    (if space
        (string-trim '(#\Space #\Tab) (subseq input space))
        "")))

(-> application--goal-description (application) string)
(defun application--goal-description (application)
  "Return a one-line description of APPLICATION's session goal."
  (let ((goal (application-goal application)))
    (if goal
        (format nil "Goal ~(~A~)~@[ since ~A~]: ~A"
                (getf goal :status)
                (let ((created (getf goal :created-at)))
                  (and (integerp created)
                       (application--calendar-description created)))
                (getf goal :objective))
        "No session goal is set. Use /goal OBJECTIVE to set one.")))

(-> application-goal-command (application string) null)
(defun application-goal-command (application remainder)
  "Apply the /goal REMAINDER: show, set, clear, pause, or resume the goal."
  (let ((goal (application-goal application))
        (word (string-downcase remainder)))
    (cond
      ((zerop (length remainder))
       (application-present application
                            (application--goal-description application)))
      ((string= word "clear")
       (setf (application-goal application) nil)
       (application--record-goal application)
       (application-present application "The session goal was cleared."))
      ((string= word "pause")
       (if (and goal (eq (getf goal :status) ':active))
           (progn
             (setf (getf (application-goal application) :status) ':paused)
             (application--record-goal application)
             (application-present application "The session goal is paused."))
           (application-present application "No active goal to pause.")))
      ((string= word "resume")
       (if (and goal (eq (getf goal :status) ':paused))
           (progn
             (setf (getf (application-goal application) :status) ':active
                   (getf (application-goal application) :continuations) 0)
             (application--record-goal application)
             (application-present application
                                  "The session goal is active again.")
             (application--run-turn application
                                    +application-goal-continuation-prompt+
                                    :continuation-p t)
             (application--run-goal-continuations application))
           (application-present application "No paused goal to resume.")))
      ((uiop:string-prefix-p "/" remainder)
       (application-present
        application
        (format nil "~S looks like a command, not an objective. ~
                     Usage: /goal [OBJECTIVE|clear|pause|resume]"
                remainder)))
      (t
       (setf (application-goal application)
             (list :objective remainder
                   :status ':active
                   :continuations 0
                   :created-at (get-universal-time)))
       (application--record-goal application)
       (application-present
        application
        (format nil
                "Goal set: ~A~%Autolith keeps working toward it after every ~
                 message. Use /goal to inspect it and /goal clear to stop."
                remainder)))))
  nil)


(-> application--generation-items (application) list)
(defun application--generation-items (application)
  "Return picker items for retained generations, newest first."
  (loop for generation in (generation-list
                           (application-configuration application))
        collect (list :name (generation-identifier generation)
                      :argument nil
                      :description
                      (format nil "~A~:[, incompatible~;~]"
                              (application--calendar-description
                               (generation-created-at generation))
                              (generation-compatible-p generation)))))

(-> application--pick-identifier
    (application &key (:title string) (:items list) (:usage string)
                 (:empty-notice string))
    (option string))
(defun application--pick-identifier
    (application &key (title "select") items (usage "") (empty-notice ""))
  "Pick one identifier from ITEMS interactively, or explain why none was picked.

Signals a usage error on non-interactive terminals, presents EMPTY-NOTICE
when ITEMS is empty, and returns NIL when the picker is cancelled."
  (block nil
    (let ((ui (application-ui application)))
      (unless (terminal-interactive-p (terminal-ui-terminal ui))
        (error 'configuration-error :message usage))
      (unless items
        (application-present application empty-notice)
        (return nil))
      (terminal-ui-select
       ui
       :title title
       :items items
       :resize-callback #'application-pending-terminal-size))))

(-> application-authenticate (application) null)
(defun application-authenticate (application)
  "Run Autolith-owned device authentication outside raw terminal mode."
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
    (application-present application "ChatGPT authentication was saved by Autolith."))
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
      ((member command '("/status" "/usage") :test #'string=)
       (application-present application (application-status-entry application))
       :continue)
      ((string= command "/compact")
       (application-compact application)
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
       (let ((identifier
               (or argument
                   (application--pick-identifier
                    application
                    :title "resume conversation"
                    :items (application--conversation-items application)
                    :usage "Usage: /resume ID"
                    :empty-notice "No saved conversations exist."))))
         (when identifier
           (application-install-conversation
            application
            (conversation-load-by-id configuration identifier))
           (application-render-records application)))
       :continue)
      ((string= command "/conversations")
       (application-present application
                            (application-list-conversations application))
       :continue)
      ((string= command "/auth")
       (application-authenticate application)
       :continue)
      ((string= command "/effort")
       (let ((effort
               (or argument
                   (application--pick-identifier
                    application
                    :title "pick the reasoning effort"
                    :items (application--effort-items application)
                    :usage "Usage: /effort LEVEL"
                    :empty-notice "No supported reasoning efforts exist."))))
         (when effort
           (application-set-reasoning-effort application effort)
           (application-present
            application
            (format nil "Reasoning effort is now ~A."
                    (configuration-reasoning-effort
                     (application-configuration application))))))
       :continue)
      ((string= command "/trace")
       (application-trace-command application argument)
       :continue)
      ((string= command "/goal")
       (application-goal-command application
                                 (application--goal-remainder input))
       :continue)
      ((string= command "/model")
       (let ((model
               (or argument
                   (application--pick-identifier
                    application
                    :title "pick the model"
                    :items (application--model-items application)
                    :usage "Usage: /model NAME"
                    :empty-notice "No supported models exist."))))
         (when model
           (application-set-model application model)
           (application-present
            application
            (format nil "The model is now ~A."
                    (configuration-model
                     (application-configuration application))))))
       :continue)
      ((string= command "/checkpoint")
       (application-checkpoint application)
       :continue)
      ((string= command "/generations")
       (application-present application
                            (generation-render-list configuration))
       :continue)
      ((string= command "/rollback")
       (let ((identifier
               (or argument
                   (application--pick-identifier
                    application
                    :title "select a generation for recovery"
                    :items (application--generation-items application)
                    :usage "Usage: /rollback ID"
                    :empty-notice "No retained generations exist."))))
         (when identifier
           (generation-request-rollback configuration identifier)))
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
