(in-package #:autolith)

;;;; -- Presentation Test Support --

(-> application-tests--ui-application (&key (:columns integer)) application)
(defun application-tests--ui-application (&key (columns 40))
  "Return a minimal application presenting into a recording terminal."
  (make-instance 'application
                 :ui (terminal-ui-create
                      :terminal (make-instance 'recording-terminal
                                               :columns columns))))

(defclass cursor-observing-provider (scripted-provider)
  ((visibility-function
    :initarg :visibility-function
    :reader cursor-observing-provider-visibility-function
    :type function
    :documentation "Function reporting live-region cursor visibility.")
   (visible-during-request-p
    :initform t
    :accessor cursor-observing-provider-visible-during-request-p
    :type boolean
    :documentation "Cursor visibility observed when a provider request begins."))
  (:documentation "A scripted provider recording cursor state during a request."))

(defmethod provider-stream-turn :before
    ((provider cursor-observing-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function)
     &key turn-budget-state goal-context compaction-p)
  "Record cursor visibility immediately before PROVIDER starts streaming."
  (declare (ignore conversation tool-namespaces event-callback
                   turn-budget-state goal-context compaction-p))
  (setf (cursor-observing-provider-visible-during-request-p provider)
        (funcall (cursor-observing-provider-visibility-function provider))))


;;;; -- Focused Presentation Tests --

(-> test-application-banner-version () null)
(defun test-application-banner-version ()
  "Test the Cosmic mark, adjacent metadata, narrow layout, and configured version."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "banner"))
         (terminal (make-instance 'recording-terminal :columns 80))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui (terminal-ui-create
                                          :terminal terminal))))
    (unwind-protect
         (let* ((spans (application-banner application))
                (text (format nil "~{~A~}"
                              (mapcar #'terminal-span-text spans)))
                (lines (uiop:split-string text :separator '(#\Newline)))
                (gradient-styles
                  (loop for span in spans
                        for style = (terminal-span-style span)
                        when (member style
                                     '(:brand-gradient-1 :brand-gradient-2
                                       :brand-gradient-3 :brand-gradient-4
                                       :brand-gradient-5 :brand-gradient-6))
                          collect style)))
           (test-assert
            (equal gradient-styles
                   '(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
                     :brand-gradient-4 :brand-gradient-5 :brand-gradient-6))
           "the Cosmic AL mark assigns one gradient style to each row")
           (test-assert (string= (first lines) "")
                        "the banner begins with one empty row")
           (test-assert (and (search "  :::.      :::" (second lines))
                             (search (format nil "AUTOLITH v~A"
                                             +autolith-version+)
                                     (second lines))
                             (search "────" (third lines))
                             (search "model" (fourth lines))
                             (search "workspace" (fifth lines)))
                        "wide banners divide identity from aligned runtime data")
           (test-assert (not (search "conversation" text))
                        "the startup banner omits the internal conversation identifier")
           (test-assert (search (format nil "v~A" +autolith-version+) text)
                        "the startup banner uses the configured version")
           (test-assert (not (search "v6.6.6" text))
                        "the startup banner contains no stale display version")
           (let ((logo-end (search "YUMMM" text))
                 (notice-start (search "Autolith executes" text)))
             (test-assert (and logo-end
                               notice-start
                               (< logo-end notice-start))
                          "the security notice follows the complete header"))
           (setf (terminal-columns terminal) 40)
           (let* ((narrow-spans (application-banner application))
                  (narrow-text (format nil "~{~A~}"
                                       (mapcar #'terminal-span-text
                                               narrow-spans)))
                  (logo-end (search "YUMMM" narrow-text))
                  (metadata-start (search "AUTOLITH" narrow-text)))
             (test-assert (and logo-end
                               metadata-start
                               (< logo-end metadata-start))
                          "narrow banners stack metadata below the AL mark")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-thinking-label-selection () null)
(defun test-thinking-label-selection ()
  "Test provider activity uses one self-modifiable word from the configured set."
  (loop repeat 20
        for label = (application-thinking-label)
        do (test-assert (member label *application-thinking-words*
                                :test #'string=)
                        "thinking labels come from the documented word set")
           (test-assert (not (find #\Space label))
                        "every thinking label is exactly one word"))
  (let ((*application-thinking-words* '("musing")))
    (test-assert (string= (application-thinking-label) "musing")
                 "changing the active word set immediately changes presentation"))
  (let ((*application-thinking-words* nil))
    (test-assert (string= (application-thinking-label) "pondering")
                 "an empty exploratory word set retains a safe fallback"))
  nil)

(-> test-interrupt-resume-instruction () null)
(defun test-interrupt-resume-instruction ()
  "Test that Ctrl-C exits with an exact command only for durable conversations."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (labels ((interrupt-application (conversation)
               "Run one CONVERSATION until a scripted Ctrl-C and return its output."
               (let* ((terminal (make-instance 'scripted-terminal
                                               :columns 80
                                               :events (list :interrupt)))
                      (application
                        (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider nil
                                       :tool-registry (make-instance 'tool-registry)
                                       :worker nil
                                       :agent nil
                                       :ui (terminal-ui-create
                                            :terminal terminal))))
                 (application-run application)
                 (recording-terminal-output terminal))))
      (unwind-protect
           (let ((durable (conversation-create configuration
                                               :identifier "resume-this"))
                 (empty (conversation-create configuration
                                             :identifier "discard-this")))
             (conversation-append-user-message durable "keep this conversation")
             (let ((output (interrupt-application durable)))
               (test-assert (search "To resume this conversation, run:" output)
                            "Ctrl-C explains how to resume a durable conversation")
               (test-assert (search "autolith --resume resume-this" output)
                            "the Ctrl-C instruction carries the exact resume command"))
             (let ((output (interrupt-application empty)))
               (test-assert (not (search "autolith --resume" output))
                            "Ctrl-C gives no resume command for an empty conversation")
               (test-assert (not (probe-file (conversation-pathname empty)))
                            "Ctrl-C does not persist an empty conversation")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)

(-> test-transcript-entries () null)
(defun test-transcript-entries ()
  "Test styled transcript entry construction, wrapping, and output bounds."
  (let ((application (application-tests--ui-application :columns 40)))
    (let ((entry (conversation-record-entry
                  application
                  '(:message :seq 1 :time 0 :role :user :content "hello there"))))
      (test-assert (equal (first entry) (terminal-span :user "❯ you"))
                   "user records present a styled you header")
      (test-assert (search "  hello there"
                           (terminal-span-text (first (last entry))))
                   "user bodies are indented beneath their header"))
    (let ((entry (conversation-record-entry
                  application
                  (list :message :seq 1 :time 0 :role :user
                        :content (make-string 50 :initial-element #\a)))))
      (test-assert (= (count #\Newline
                             (terminal-span-text (first (last entry))))
                      1)
                   "long bodies wrap at the terminal width"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"message\",\"role\":\"assistant\",
                     \"content\":[{\"type\":\"output_text\",\"text\":\"hi\"}]}"))))
      (test-assert (equal (first entry) (terminal-span :brand "● autolith"))
                   "assistant items present a styled autolith header"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"message\",\"role\":\"assistant\",
                     \"content\":[{\"type\":\"output_text\",
                                   \"text\":\"see **bold** move\"}]}"))))
      (test-assert (find (terminal-span :strong "bold") entry :test #'equal)
                   "assistant bodies render markdown emphasis"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"function_call\",\"namespace\":\"self\",
                     \"name\":\"eval\",
                     \"arguments\":\"{\\\"form\\\":\\\"(+ 1 2)\\\"}\"}"))))
      (test-assert (equal (first entry) (terminal-span :tool "▸ self.eval"))
                   "tool requests present a styled tool header")
      (test-assert (eq (terminal-span-style (second entry)) ':dim)
                   "tool arguments render as a dim detail")
      (test-assert (= (length entry) 2)
                   "tool requests stay on one header row"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"web_search_call\",
                     \"action\":{\"type\":\"search\",
                                 \"query\":\"live lisp images\"}}"))))
      (test-assert (equal (first entry) (terminal-span :tool "▸ web search"))
                   "web search calls present a styled search header")
      (test-assert (search "live lisp images"
                           (markdown-tests--row-text entry))
                   "web search entries show their query"))
    (let ((entry (conversation-record-entry
                  application
                  '(:tool-result :seq 2 :time 0 :call-id 1 :tool "self.eval"
                    :status :ok :output "42"))))
      (test-assert (equal (first entry) (terminal-span :success "✓ self.eval"))
                   "successful tool results present a success header"))
    (let ((entry (conversation-record-entry
                  application
                  '(:tool-result :seq 3 :time 0 :call-id 2 :tool "self.eval"
                    :status :error :output "boom"))))
      (test-assert (equal (first entry)
                          (terminal-span :failure "✗ self.eval failed"))
                   "failed tool results present a failure header")))
  (let* ((output (format nil "~{line ~D~^~%~}"
                         (loop for index from 1 to 20
                               collect index)))
         (bounded (application--bounded-tool-output output)))
    (test-assert (search "… +8 more lines" bounded)
                 "long tool output is bounded with a truncation note"))
  (test-assert (null (application--bounded-tool-output ""))
               "empty tool output produces no transcript body")
  (let ((application (application-tests--ui-application :columns 40)))
    (test-assert (string= (application--indented-body application
                                                      (format nil "3~%"))
                          "  3")
                 "trailing output newlines leave no blank body row"))
  (let ((help (application-help)))
    (test-assert (search "/rollback" help)
                 "help lists every interactive command")
    (test-assert (search "pick a generation for recovery" help)
                 "help lists command descriptions"))
  nil)

(-> test-streaming-presentation () null)
(defun test-streaming-presentation ()
  "Test safe streaming, exact record reconciliation, and live tool entries."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "stream-test"))
                (terminal (make-instance 'recording-terminal :columns 30))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :ui (terminal-ui-create
                                                 :terminal terminal)))
                (observer (application-agent-observer application))
                (send-text (callback-agent-observer-text-callback observer))
                (send-status (callback-agent-observer-status-callback observer))
                (streamed-text (format nil
                                       "The quick brown fox jumps over~%the lazy dog")))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-text (format nil
                                      "The quick brown fox jumps over~%"))
           (funcall send-text "the lazy dog")
           (let ((streamed (recording-terminal-output terminal)))
             (test-assert (search "● autolith" streamed)
                          "streaming opens a autolith transcript block")
             (test-assert (search "The quick brown fox" streamed)
                          "newline-terminated logical lines commit while streaming"))
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" streamed-text))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-completed nil)
           (let ((completion (recording-terminal-output terminal)))
             (test-assert (search "the lazy dog" completion)
                          "completing a request commits the fluid tail")
             (test-assert (not (search "● autolith" completion))
                          "streamed message records do not render again"))
           (setf (application-rendered-sequence application) 0)
           (recording-terminal-reset terminal)
           (application-render-records application)
           (test-assert
            (not (search "The quick brown fox"
                         (recording-terminal-output terminal)))
            "replaying a conversation does not duplicate streamed messages")
           (conversation-append-tool-result
            conversation "call-1" "self.eval" "42" t)
           (recording-terminal-reset terminal)
           (funcall send-status :tool-call-completed (list :tool "self.eval"))
           (test-assert (search "✓ self.eval"
                                (recording-terminal-output terminal))
                        "tool results render as soon as they complete")
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "plain answer"))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-started nil)
           (funcall send-text "")
           (funcall send-status :provider-request-completed nil)
           (test-assert (search "plain answer"
                                (recording-terminal-output terminal))
                        "empty deltas cannot suppress a durable assistant message")
           (funcall send-status :provider-request-started nil)
           (funcall send-text "provisional answer")
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "corrected answer"))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-completed nil)
           (test-assert (search "corrected answer"
                                (recording-terminal-output terminal))
                        "mismatched stream text cannot hide the durable answer")
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-started nil)
           (funcall send-text (format nil "```lisp~%(+ 1 2)~%"))
           (test-assert (search "1 │ (+ 1 2)"
                                (recording-terminal-output terminal))
                        "streamed code blocks render numbered gutters")
           (funcall send-status :provider-request-completed nil)
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-turn-cursor-visibility () null)
(defun test-turn-cursor-visibility ()
  "Test model turns hide cursor motion and restore the input cursor afterward."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "cursor-turn"))
                (terminal (make-instance 'recording-terminal :columns 50))
                (ui (terminal-ui-create :terminal terminal))
                (provider
                  (make-instance
                   'cursor-observing-provider
                   :visibility-function
                   (lambda ()
                     (live-region-cursor-visible-p
                      (terminal-ui-live-region ui)))
                   :results
                   (list
                    (agent-test-result
                     "cursor-response"
                     (list (agent-test-message "finished"))
                     :turn-completion :end))))
                (registry (make-instance 'tool-registry))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker t))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :provider provider
                                            :tool-registry registry
                                            :worker t
                                            :agent agent
                                            :ui ui)))
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (recording-terminal-reset terminal)
             (application--run-turn application "hello")
             (test-assert
              (not (cursor-observing-provider-visible-during-request-p
                    provider))
              "the cursor is hidden before the provider starts streaming")
             (test-assert
              (live-region-cursor-visible-p (terminal-ui-live-region ui))
              "the input cursor is restored after the model turn")
             (let* ((output (recording-terminal-output terminal))
                    (show (format nil "~C[?25h"
                                  +terminal-escape-character+)))
               (test-assert
                (= (terminal-tests--substring-count show output) 1)
                "one cursor reveal follows the complete model turn")
               (test-assert
                (< (or (search "finished" output) most-positive-fixnum)
                   (or (search show output :from-end t) -1))
                "the cursor is revealed only after the final answer is painted"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-picker () null)
(defun test-conversation-picker ()
  "Test saved-conversation picker items and interactive selection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((older (conversation-create configuration :identifier "older"))
                (active (conversation-create configuration :identifier "active"))
                (terminal (make-instance 'scripted-terminal :columns 60))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation active
                                            :ui (terminal-ui-create
                                                 :terminal terminal))))
           (conversation-append-user-message older "older saved conversation")
           (conversation-append-user-message active "active saved conversation")
           (let ((items (application--conversation-items application)))
             (test-assert (= (length items) 2)
                          "every saved conversation is offered")
             (test-assert (find "older" items
                                :key (lambda (item)
                                       (getf item :name))
                                :test #'string=)
                          "older conversations appear in the picker")
             (test-assert (search ", current"
                                  (getf (find "active" items
                                              :key (lambda (item)
                                                     (getf item :name))
                                              :test #'string=)
                                        :description))
                          "the active conversation is marked current")
             (test-assert (search (application--abbreviated-directory
                                   (namestring
                                    (configuration-working-directory
                                     configuration)))
                                  (getf (first items) :description))
                          "picker items show the conversation origin directory")
             (conversation-append-user-message
              active
              "please refresh the transcript colors")
             (test-assert
              (search "· please refresh the transcript colors"
                      (getf (find "active"
                                  (application--conversation-items application)
                                  :key (lambda (item)
                                         (getf item :name))
                                  :test #'string=)
                            :description))
              "picker items preview the newest message")
             (terminal-ui-start (application-ui application))
             (setf (scripted-terminal-events terminal) (list :submit))
             (test-assert (string= (application--pick-identifier
                                    application
                                    :title "resume conversation"
                                    :items items
                                    :usage "Usage: /resume ID"
                                    :empty-notice "none")
                                   (getf (first items) :name))
                          "enter picks the highlighted conversation")
             (terminal-ui-stop (application-ui application))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let ((application (application-tests--ui-application :columns 60)))
    (test-assert (handler-case
                     (progn
                       (application--pick-identifier application
                                                     :title "resume"
                                                     :items nil
                                                     :usage "Usage: /resume ID"
                                                     :empty-notice "none")
                       nil)
                   (configuration-error (condition)
                     (not (null (search "Usage: /resume"
                                        (format nil "~A" condition))))))
                 "non-interactive pickers demand an explicit identifier"))
  nil)

(-> test-effort-switch () null)
(defun test-effort-switch ()
  "Test reasoning effort picker items and in-place configuration switching."
  (let* ((base (test-configuration))
         (configuration
           (make-instance
            'configuration
            :source-root (configuration-source-root base)
            :working-directory (configuration-working-directory base)
            :data-root (configuration-data-root base)
            :state-root (configuration-state-root base)
            :cache-root (configuration-cache-root base)
            :codex-auth-path (configuration-codex-auth-path base)
            :model (configuration-model base)
            :reasoning-effort (configuration-reasoning-effort base)
            :web-search-mode "live"
            :provider-endpoint "https://provider.test/responses"))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "effort"))
                (provider (provider-create configuration))
                (registry (make-default-tool-registry))
                (worker (lisp-worker-create configuration))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker worker
                                     :maximum-provider-steps 7
                                     :provider-step-warning 3
                                     :maximum-tool-calls 9))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :provider provider
                                 :tool-registry registry
                                 :worker worker
                                 :agent agent
                                 :ui (terminal-ui-create
                                      :terminal (make-instance
                                                 'recording-terminal
                                                 :columns 60)))))
           (setf (provider-rate-limits provider) '(:primary (:used-percent 25)))
           (let ((items (application--effort-items application)))
             (test-assert (= (length items)
                             (length +supported-reasoning-efforts+))
                          "every supported effort is offered")
             (test-assert (find "current" items
                                :key (lambda (item)
                                       (getf item :description))
                                :test #'string=)
                          "the active effort is marked current"))
           (application-set-reasoning-effort application "low")
           (test-assert (string= (configuration-reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "switching effort replaces the configuration")
           (let ((updated (application-configuration application)))
             (test-assert (equal (configuration-source-root updated)
                                 (configuration-source-root configuration))
                          "effort switching preserves the source root")
             (test-assert (equal (configuration-state-root updated)
                                 (configuration-state-root configuration))
                          "effort switching preserves private state paths")
             (test-assert (string= (configuration-provider-endpoint updated)
                                   "https://provider.test/responses")
                          "effort switching preserves the provider endpoint")
             (test-assert (string= (configuration-web-search-mode updated) "live")
                          "effort switching preserves hosted web search mode"))
           (test-assert
            (string= (provider-session-id (application-provider application))
                     (provider-session-id provider))
            "effort switching preserves the provider session identity")
           (test-assert (equal (provider-rate-limits
                                (application-provider application))
                               '(:primary (:used-percent 25)))
                        "effort switching preserves the latest rate snapshot")
           (test-assert (typep (application-agent application) 'agent)
                        "switching effort reconnects the agent")
           (test-assert (= (agent-maximum-provider-steps
                            (application-agent application))
                           7)
                        "effort switching preserves the active turn budget")
           (let ((items (application--model-items application)))
             (test-assert (= (length items) (length +supported-models+))
                          "every 5.6 family model is offered")
             (test-assert (string= (getf (find "current" items
                                               :key (lambda (item)
                                                      (getf item :description))
                                               :test #'string=)
                                         :name)
                                   "gpt-5.6-sol")
                          "the active model is marked current"))
           (application-set-model application "gpt-5.6-terra")
           (test-assert (string= (configuration-model
                                  (application-configuration application))
                                 "gpt-5.6-terra")
                        "switching the model replaces the configuration")
           (test-assert (string= (configuration-reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "model switching preserves the reasoning effort")
           (test-assert (handler-case
                            (progn
                              (application-set-model application "gpt-4")
                              nil)
                          (configuration-error ()
                            t))
                        "unsupported models are rejected with the choices"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-status-entry () null)
(defun test-status-entry ()
  "Test /status token accounting and rate limit presentation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "status"))
                (provider (provider-create configuration))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :provider provider
                                            :ui (terminal-ui-create
                                                 :terminal (make-instance
                                                            'recording-terminal
                                                            :columns 80)))))
           (test-assert (search "No rate limit data yet"
                                (markdown-tests--row-text
                                 (application-status-entry application)))
                        "status explains missing rate limit data")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "one"
                  :usage '(("input_tokens" 1000)
                           ("output_tokens" 500)
                           ("total_tokens" 1500))))
           (conversation-append-provider-metadata
            conversation
            (list :request-number 2
                  :response-id "two"
                  :usage '(("input_tokens" 2000)
                           ("output_tokens" 300)
                           ("total_tokens" 2300))))
           (setf (provider-rate-limits provider)
                 (list :captured-at (get-universal-time)
                       :primary (list :used-percent 28
                                      :window-minutes 300
                                      :resets-at nil)
                       :secondary (list :used-percent 45.5
                                        :window-minutes 10080
                                        :resets-at nil)))
           (let ((text (markdown-tests--row-text
                        (application-status-entry application))))
             (test-assert (search "3.8K total (3.0K input + 800 output)" text)
                          "status sums token usage across requests")
             (test-assert (search "5h limit" text)
                          "the primary window is named by its duration")
             (test-assert (search "weekly limit" text)
                          "the secondary window is named by its duration")
             (test-assert (search "72% left" text)
                          "status reports the remaining primary percentage")
             (test-assert (search "█" text)
                          "status draws usage bars")
             (test-assert (search "standard" text)
                          "status names the standard service path")
             (test-assert (search "compacts at 80%" text)
                          "status reports the compaction threshold")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-session-goal () null)
(defun test-session-goal ()
  "Test goal persistence, context injection, continuation, and completion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "goal"))
                (terminal (make-instance 'recording-terminal :columns 60))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :tool-registry
                                            (make-default-tool-registry)
                                            :worker nil
                                            :ui (terminal-ui-create
                                                 :terminal terminal))))
           (terminal-ui-start (application-ui application))
           (application-goal-command application "polish the terminal")
           (test-assert (eq (getf (application-goal application) :status)
                            ':active)
                        "setting a goal activates it")
           (let ((context (application-goal-context application)))
             (test-assert (search "polish the terminal" context)
                          "the goal context carries the objective")
             (test-assert (search "[GOAL-COMPLETE]" context)
                          "the goal context teaches the completion marker"))
           (let ((sibling (make-instance
                           'application
                           :configuration configuration
                           :conversation (conversation-load-by-id configuration
                                                                  "goal")
                           :ui (terminal-ui-create
                                :terminal (make-instance 'recording-terminal
                                                         :columns 60)))))
             (application--load-goal sibling)
             (test-assert (string= (getf (application-goal sibling) :objective)
                                   "polish the terminal")
                          "goals reload from durable conversation records"))
           (application-goal-command application "pause")
           (test-assert (null (application-goal-context application))
                        "paused goals inject no context")
           (let* ((completion-item
                    (json-object
                     "type" "message"
                     "role" "assistant"
                     "content" (json-array
                                (json-object
                                 "type" "output_text"
                                 "text" "All polished. [GOAL-COMPLETE]"))))
                  (working-item
                    (json-object
                     "type" "message"
                     "role" "assistant"
                     "content" (json-array
                                (json-object "type" "output_text"
                                             "text" "Still working."))))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results (list (agent-test-result "goal-1"
                                                       (list working-item)
                                                       :turn-completion :end)
                                    (agent-test-result "goal-2"
                                                       (list completion-item)
                                                       :turn-completion :end))))
                  (agent (agent-create :configuration configuration
                                       :provider provider
                                       :conversation conversation
                                       :tool-registry
                                       (application-tool-registry application)
                                       :worker nil)))
             (setf (application-provider application) provider
                   (application-agent application) agent)
             (application-goal-command application "resume")
             (test-assert (eq (getf (application-goal application) :status)
                              ':complete)
                          "the continuation loop stops at the marker")
             (test-assert (every #'non-empty-string-p
                                 (scripted-provider-goal-contexts provider))
                          "active goals ride along every provider request")
             (test-assert (search "✓ goal complete"
                                  (recording-terminal-output terminal))
                          "completing a goal presents a notice"))
           (setf (application-goal application)
                 (list :objective "endless"
                       :status ':active
                       :continuations +application-goal-continuation-limit+
                       :created-at (get-universal-time)))
           (recording-terminal-reset terminal)
           (application--run-goal-continuations application)
           (test-assert (eq (getf (application-goal application) :status)
                            ':paused)
                        "the continuation limit pauses the goal")
           (test-assert (search "paused after"
                                (recording-terminal-output terminal))
                        "pausing explains the continuation budget")
           (test-assert (equal (conversation-record-entry
                                application
                                (list :message :seq 99 :time 0 :role :user
                                      :content
                                      +application-goal-continuation-prompt+))
                               (list (terminal-span :hint "∙ goal continues")))
                        "continuation prompts render as dim notices")
           (application-goal-command application "clear")
           (test-assert (null (application-goal application))
                        "clearing removes the goal")
           (application-goal-command application "/status")
           (test-assert (null (application-goal application))
                        "command-shaped objectives are rejected")
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-application-banner-version)
  (test-thinking-label-selection)
  (test-interrupt-resume-instruction)
  (test-transcript-entries)
  (test-streaming-presentation)
  (test-turn-cursor-visibility)
  (test-conversation-picker)
  (test-effort-switch)
  (test-status-entry)
  (test-session-goal)
  t)
