(in-package #:frob)

;;;; -- Presentation Test Support --

(-> application-tests--ui-application (&key (:columns integer)) application)
(defun application-tests--ui-application (&key (columns 40))
  "Return a minimal application presenting into a recording terminal."
  (make-instance 'application
                 :ui (terminal-ui-create
                      :terminal (make-instance 'recording-terminal
                                               :columns columns))))


;;;; -- Focused Presentation Tests --

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
      (test-assert (equal (first entry) (terminal-span :brand "● frob"))
                   "assistant items present a styled frob header"))
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
  "Test progressive line commits, streamed record skipping, and live tool entries."
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
             (test-assert (search "● frob" streamed)
                          "streaming opens a frob transcript block")
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
             (test-assert (not (search "● frob" completion))
                          "streamed message records do not render again"))
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
           (funcall send-status :provider-request-completed nil)
           (test-assert (search "plain answer"
                                (recording-terminal-output terminal))
                        "unstreamed assistant messages render from records")
           (recording-terminal-reset terminal)
           (funcall send-text (format nil "```lisp~%(+ 1 2)~%"))
           (test-assert (search "1 │ (+ 1 2)"
                                (recording-terminal-output terminal))
                        "streamed code blocks render numbered gutters")
           (funcall send-status :provider-request-completed nil)
           (terminal-ui-stop (application-ui application)))
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
           (declare (ignore older))
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
                        "effort switching preserves the active turn budget"))
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
                          "status names the standard service path")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-thinking-label-selection)
  (test-transcript-entries)
  (test-streaming-presentation)
  (test-conversation-picker)
  (test-effort-switch)
  (test-status-entry)
  t)
