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
                (streamed-text "The quick brown fox jumps over the lazy dog"))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-text "The quick brown fox jumps ")
           (funcall send-text "over the lazy dog")
           (let ((streamed (recording-terminal-output terminal)))
             (test-assert (search "● frob" streamed)
                          "streaming opens a frob transcript block")
             (test-assert (search "  The quick brown fox jumps" streamed)
                          "completed wrapped lines commit while streaming"))
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
             (test-assert (search "over the lazy dog" completion)
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

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-transcript-entries)
  (test-streaming-presentation)
  (test-conversation-picker)
  t)
