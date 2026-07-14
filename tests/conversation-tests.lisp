(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-conversation-compaction () null)
(defun test-conversation-compaction ()
  "Test summary records, projection replacement, and usage tracking."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "compact")))
           (conversation-append-user-message conversation "first question")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "one"
                  :usage '(("total_tokens" 4321))))
           (test-assert (= (conversation-last-total-tokens conversation) 4321)
                        "usage totals track the newest provider step")
           (conversation-append-summary conversation
                                        "summary of the earlier work")
           (test-assert (= (length (conversation-input-items conversation)) 1)
                        "compaction replaces the projection with one bridge")
           (test-assert (zerop (conversation-last-total-tokens conversation))
                        "compaction resets the tracked usage")
           (test-assert (null (conversation-turn-state conversation))
                        "compaction drops the provider turn state")
           (conversation-append-user-message conversation "later question")
           (let* ((reloaded (conversation-load-by-id configuration "compact"))
                  (items (conversation-input-items reloaded))
                  (bridge-text (json-get
                                (aref (json-get (first items) "content") 0)
                                "text")))
             (test-assert (= (length items) 2)
                          "replay reproduces the compacted projection")
             (test-assert (search "summary of the earlier work" bridge-text)
                          "the bridge item carries the summary")
             (test-assert (search "compacted" bridge-text)
                          "the bridge item explains its provenance")
             (test-assert (zerop (conversation-last-total-tokens reloaded))
                          "replay resets usage tracked before the summary")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-origin-directory () null)
(defun test-conversation-origin-directory ()
  "Test origin directory persistence, peeking, and legacy header tolerance."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "origin"))
                (expected (namestring
                           (configuration-working-directory configuration))))
           (test-assert (string= (conversation-origin-directory conversation)
                                 expected)
                        "a new conversation records its origin directory")
           (conversation-append-user-message conversation "remember this workspace")
           (test-assert (string= (conversation-origin-directory
                                  (conversation-load-by-id configuration
                                                           "origin"))
                                 expected)
                        "a reloaded conversation preserves its origin directory")
           (test-assert (string= (getf (rest (conversation-peek-header
                                              (conversation-pathname
                                               conversation)))
                                       :directory)
                                 expected)
                        "peeking reads the origin directory cheaply")
           (let ((legacy (conversation-pathname-for-id configuration "legacy")))
             (conversation--write-form
              legacy
              (list :conversation :version 1 :id "legacy" :created-at 1))
             (test-assert (null (conversation-origin-directory
                                 (conversation-load-by-id configuration
                                                          "legacy")))
                          "legacy conversations without an origin still load")
             (test-assert
              (not (find legacy (conversation-list configuration) :test #'equal))
              "header-only legacy conversations stay out of saved listings")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-model-selection () null)
(defun test-conversation-model-selection ()
  "Test model selection headers, append-only changes, and legacy loading."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "model-choice")))
           (test-assert
            (string= (conversation-model conversation) "gpt-5.6-sol")
            "new conversations inherit the configured model")
           (test-assert
            (string= (conversation-reasoning-effort conversation) "ultra")
            "new conversations inherit the configured effort")
           (conversation-set-model-selection conversation "gpt-5.6-luna" "high")
           (test-assert (not (probe-file (conversation-pathname conversation)))
                        "selecting a model does not persist an empty conversation")
           (conversation-append-user-message conversation "remember this model")
           (let ((header (first (conversation--read-records
                                 (conversation-pathname conversation)))))
             (test-assert (string= (getf (rest header) :model) "gpt-5.6-luna")
                          "the initial model is stored in the header")
             (test-assert
              (string= (getf (rest header) :reasoning-effort) "high")
              "the initial effort is stored in the header"))
           (conversation-set-model-selection conversation "gpt-5.6-terra" "low")
           (let* ((records (conversation--read-records
                            (conversation-pathname conversation)))
                  (selection (first (last records))))
             (test-assert (eq (first selection) :configuration)
                          "later model changes append configuration records")
             (test-assert (string= (getf (rest selection) :model)
                                   "gpt-5.6-terra")
                          "the appended record carries the changed model"))
           (let ((reloaded (conversation-load-by-id configuration "model-choice")))
             (test-assert
              (string= (conversation-model reloaded) "gpt-5.6-terra")
              "conversation replay restores the latest model")
             (test-assert
              (string= (conversation-reasoning-effort reloaded) "low")
              "conversation replay restores the latest effort"))
           (let ((legacy (conversation-pathname-for-id configuration "legacy-model")))
             (conversation--write-form
              legacy
              (list :conversation :version 1 :id "legacy-model" :created-at 1))
             (let ((loaded (conversation-load-by-id configuration "legacy-model")))
               (test-assert (null (conversation-model loaded))
                            "legacy conversations load without a model")
               (test-assert (null (conversation-reasoning-effort loaded))
                            "legacy conversations load without an effort"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration :identifier "test-turn"))
                (assistant-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text" "text" "hello")))))
           (test-assert (not (conversation-persisted-p conversation))
                        "a new conversation begins only in memory")
           (test-assert (not (probe-file (conversation-pathname conversation)))
                        "an empty conversation has no file")
           (test-assert (null (conversation-list configuration))
                        "empty conversations never appear in saved listings")
           (conversation-append-user-message conversation "hi")
           (test-assert (conversation-persisted-p conversation)
                        "the first durable record publishes the conversation")
           (let ((records (conversation--read-records
                           (conversation-pathname conversation))))
             (test-assert (and (= (length records) 2)
                               (eq (first (first records)) :conversation)
                               (eq (first (second records)) :message))
                          "first persistence atomically publishes header and record"))
           (conversation-append-provider-item conversation assistant-item)
           (conversation-append-tool-result
            conversation "call-1" "lisp.eval" "42" t)
           (with-open-file (stream (conversation-pathname conversation)
                                   :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:incomplete" stream))
           (let ((loaded (conversation-load-by-id configuration "test-turn")))
             (test-assert (= (length (conversation-input-items loaded)) 3)
                          "conversation reload projects complete wire items")
             (test-assert (= (conversation-next-sequence loaded) 4)
                          "conversation reload restores its next sequence")
             (test-assert
              (string= (json-get (first (conversation-input-items loaded)) "role")
                       "user")
              "conversation reload preserves the first user message")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
