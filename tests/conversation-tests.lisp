(in-package #:autolith)

;;;; -- Subsystem Tests --

(define-constant +test-conversation-tiny-png+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGP4z8DwHwAFAAH/iZk9HQAAAABJRU5ErkJggg=="
  :test #'string=
  :documentation "A one-pixel PNG used to exercise durable image input.")

(-> test-conversation--write-tiny-png (pathname) pathname)
(defun test-conversation--write-tiny-png (pathname)
  "Write the valid one-pixel test PNG to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence
     (base64-string-to-usb8-array +test-conversation-tiny-png+)
     stream))
  pathname)

(-> test-conversation-image-input () null)
(defun test-conversation-image-input ()
  "Test image validation, durable artifacts, projection, and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (merge-pathnames "source image.png" root)))
    (unwind-protect
         (progn
           (test-conversation--write-tiny-png source)
           (test-assert
            (equal (image-input-recognize-pasted-path
                    (format nil "'~A'" (namestring source)))
                   (truename source))
            "quoted pasted image paths are recognized by file content")
           (let* ((conversation
                    (conversation-create configuration :identifier "images"))
                  (input
                    (user-message-input-create
                     :text "Describe [Image #1]."
                     :image-pathnames (list (truename source))))
                  (item (conversation-append-user-message conversation input))
                  (content (json-get item "content"))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record (second records))
                  (descriptor (first (getf (rest record) :images)))
                  (artifact
                    (merge-pathnames
                     (getf descriptor :artifact)
                     (conversation-image-artifact-root conversation))))
             (test-assert (= (length content) 4)
                          "one image contributes tags, image data, and user text")
             (test-assert
              (and (string= (json-get (aref content 0) "type") "input_text")
                   (search "<image name=[Image #1]"
                           (json-get (aref content 0) "text"))
                   (string= (json-get (aref content 1) "type") "input_image")
                   (uiop:string-prefix-p
                    "data:image/png;base64,"
                    (json-get (aref content 1) "image_url"))
                   (string= (json-get (aref content 1) "detail") "high")
                   (string= (json-get (aref content 2) "text") "</image>")
                   (string= (json-get (aref content 3) "text")
                            "Describe [Image #1]."))
              "image messages use the current Codex Responses wire shape")
             (test-assert (probe-file artifact)
                          "conversation images are copied into private artifacts")
             (test-assert
              (not (search "data:image"
                           (with-output-to-string (stream)
                             (prin1 records stream))))
              "conversation records never inline image bytes")
             (let* ((call
                      (json-object
                       "type" "function_call"
                       "call_id" "view-1"
                       "namespace" "fs"
                       "name" "view-image"
                       "arguments"
                       (json-encode
                        (json-object "path" (namestring source)))))
                    (tool-attachment
                      (image-input-prepare
                       source
                       (conversation-image-artifact-root conversation)))
                    (tool-item nil))
               (conversation-append-provider-item conversation call)
               (setf tool-item
                     (conversation-append-tool-result
                      conversation
                      "view-1"
                      :tool-name "fs.view-image"
                      :output "Viewed the image."
                      :image-attachments (list tool-attachment)
                      :success-p t))
               (let* ((tool-output (json-get tool-item "output"))
                      (durable-records
                        (conversation--read-records
                         (conversation-pathname conversation)))
                      (tool-record
                        (find :tool-result durable-records :key #'first)))
                 (test-assert
                  (and (vectorp tool-output)
                       (= (length tool-output) 1)
                       (string= (json-get (aref tool-output 0) "type")
                                "input_image")
                       (uiop:string-prefix-p
                        "data:image/png;base64,"
                        (json-get (aref tool-output 0) "image_url")))
                  "image tools return native image content in their output")
                 (test-assert
                  (and (getf (rest tool-record) :images)
                       (null (getf (rest tool-record) :wire-json))
                       (not (search
                             "data:image"
                             (with-output-to-string (stream)
                               (prin1 tool-record stream)))))
                  "image tool results persist descriptors instead of base64"))
             (let* ((loaded
                      (conversation-load-by-id configuration "images"))
                    (loaded-content
                      (json-get (first (conversation-input-items loaded))
                                "content"))
                    (loaded-tool-output
                      (json-get (third (conversation-input-items loaded))
                                "output")))
               (test-assert
                (string= (json-get (aref loaded-content 1) "image_url")
                         (json-get (aref content 1) "image_url"))
                "conversation replay reconstructs the exact user image")
               (test-assert
                (and (vectorp loaded-tool-output)
                     (string=
                      (json-get (aref loaded-tool-output 0) "image_url")
                      (json-get
                       (aref (json-get tool-item "output") 0)
                       "image_url")))
                "conversation replay reconstructs the exact image tool output")))
             (delete-file artifact)
             (test-assert
              (handler-case
                  (progn
                    (conversation-load-by-id configuration "images")
                    nil)
                (image-input-error (condition)
                  (eq (image-input-error-stage condition) ':loading)))
              "conversation replay rejects a missing image artifact")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

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
             (snapshot-write
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
             (snapshot-write
              legacy
              (list :conversation :version 1 :id "legacy-model" :created-at 1))
             (let ((loaded (conversation-load-by-id configuration "legacy-model")))
               (test-assert (null (conversation-model loaded))
                            "legacy conversations load without a model")
               (test-assert (null (conversation-reasoning-effort loaded))
                            "legacy conversations load without an effort"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-interrupted-tool-call () null)
(defun test-conversation-interrupted-tool-call ()
  "Test append-only repair of a function call whose process exited mid-tool."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "interrupted"))
                (call
                  (json-object
                   "type" "function_call"
                   "status" "completed"
                   "arguments" "{\"patterns\":[\"one\",\"two\"]}"
                   "call_id" "call-interrupted"
                   "name" "multi-content"
                   "namespace" "search")))
           (conversation-append-user-message conversation "continue the task")
           (conversation-append-provider-item conversation call)
           ;; Reproduce a user message persisted after restart but before the
           ;; malformed provider replay was rejected.
           (conversation-append-user-message conversation "carry on")
           (let* ((loaded
                    (conversation-load-by-id configuration "interrupted"))
                  (items (conversation-input-items loaded))
                  (records
                    (conversation--read-records
                     (conversation-pathname loaded))))
             (test-assert (= (length items) 4)
                          "replay adds exactly one interrupted tool output")
             (test-assert
              (and (string= (json-get (second items) "type") "function_call")
                   (string= (json-get (third items) "type")
                            "function_call_output")
                   (string= (json-get (third items) "call_id")
                            "call-interrupted")
                   (search "may have changed external state"
                           (json-get (third items) "output"))
                   (string= (json-get (fourth items) "role") "user"))
              "repair places an honest failure output before later user input")
             (let ((repair (first (last records))))
               (test-assert
                (and (eq (first repair) :tool-result)
                     (eq (getf (rest repair) :status) :error)
                     (string= (getf (rest repair) :call-id)
                              "call-interrupted")
                     (string= (getf (rest repair) :tool)
                              "search.multi-content"))
                "repair persists a correlated append-only failure record"))
             (let ((record-count (length records))
                   (reloaded
                     (conversation-load-by-id configuration "interrupted")))
               (test-assert
                (= (length (conversation--read-records
                            (conversation-pathname reloaded)))
                   record-count)
                "loading repaired history does not append duplicate outputs")
               (test-assert
                (string= (json-get
                          (third (conversation-input-items reloaded))
                          "type")
                         "function_call_output")
                "reloaded history keeps the repaired provider ordering"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (test-conversation-image-input)
  (test-conversation-interrupted-tool-call)
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
            conversation
            "call-1"
            :tool-name "lisp.eval"
            :output "42"
            :success-p t)
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
              "conversation reload preserves the first user message")
             (conversation-append-user-message loaded "after interrupted write")
             (multiple-value-bind (records incomplete-tail-p)
                 (conversation--read-records (conversation-pathname loaded))
               (test-assert
                (and (not incomplete-tail-p)
                     (= (length records) 5)
                     (string= (getf (rest (first (last records))) :content)
                              "after interrupted write"))
                "the next conversation append atomically repairs its tail"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
