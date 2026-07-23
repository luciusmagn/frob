(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-conversation--write-tiny-png (pathname) pathname)
(defun test-conversation--write-tiny-png (pathname)
  "Write the valid one-pixel test PNG to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence
     (base64-string-to-usb8-array *test-conversation-tiny-png*)
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
                      :content-blocks
                      (list "Before image."
                            tool-attachment
                            "After image.")
                      :success-p t))
               (let* ((tool-output (json-get tool-item "output"))
                      (durable-records
                        (conversation--read-records
                         (conversation-pathname conversation)))
                      (tool-record
                        (find :tool-result durable-records :key #'first)))
                 (test-assert
                  (and (vectorp tool-output)
                       (= (length tool-output) 3)
                       (string= (json-get (aref tool-output 0) "type")
                                "input_text")
                       (string= (json-get (aref tool-output 0) "text")
                                "Before image.")
                       (string= (json-get (aref tool-output 1) "type")
                                "input_image")
                       (uiop:string-prefix-p
                        "data:image/png;base64,"
                        (json-get (aref tool-output 1) "image_url"))
                       (string= (json-get (aref tool-output 2) "type")
                                "input_text")
                       (string= (json-get (aref tool-output 2) "text")
                                "After image."))
                  "image tools preserve exact multimodal content order")
                 (test-assert
                  (and (getf (rest tool-record) :content-blocks)
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
                      (json-get (aref loaded-tool-output 0) "text")
                      "Before image.")
                     (string=
                      (json-get (aref loaded-tool-output 1) "image_url")
                      (json-get
                       (aref (json-get tool-item "output") 1)
                       "image_url"))
                     (string=
                      (json-get (aref loaded-tool-output 2) "text")
                      "After image."))
                "conversation replay reconstructs ordered image tool output")))
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

(-> test-conversation-ephemeral-tool-projection () null)
(defun test-conversation-ephemeral-tool-projection ()
  "Test request-local tool correlation stays out of durable history and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "ephemeral-tool-projection"))
         (durable-call
           (json-object
            "type" "function_call"
            "call_id" "durable-call"
            "namespace" "test"
            "name" "echo"
            "arguments" "{\"value\":\"durable\"}"))
         (ephemeral-call
           (json-object
            "type" "function_call"
            "call_id" "ephemeral-call"
            "namespace" "skill"
            "name" "load"
            "arguments" "{\"name\":\"alpha-secret\"}")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "Run mixed calls.")
           (conversation-append-provider-item conversation durable-call)
           (conversation-append-provider-item
            conversation
            ephemeral-call
            :persistence ':next-response)
           (conversation-append-tool-result
            conversation
            "durable-call"
            :tool-name "test.echo"
            :output "echo: durable"
            :success-p t)
           (conversation-append-tool-result
            conversation
            "ephemeral-call"
            :tool-name "skill.load"
            :output "Selected alpha-secret."
            :success-p t
            :persistence ':next-response)
           (let* ((live
                    (conversation-input-items-for-request conversation))
                  (durable
                    (conversation-input-items-for-request
                     conversation
                     :include-ephemeral-p nil))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record-source
                    (with-output-to-string (stream)
                      (prin1 records stream))))
             (test-assert
              (and (= (length live) 5)
                   (string= (json-get (second live) "call_id")
                            "durable-call")
                   (string= (json-get (third live) "call_id")
                            "ephemeral-call")
                   (string= (json-get (fourth live) "call_id")
                            "durable-call")
                   (string= (json-get (fifth live) "call_id")
                            "ephemeral-call"))
              "mixed durable and request-local call items retain exact wire order")
             (test-assert
              (and (= (length durable) 3)
                   (string= (json-get (second durable) "call_id")
                            "durable-call")
                   (string= (json-get (third durable) "call_id")
                            "durable-call"))
              "compaction input excludes request-local call correlation")
             (test-assert
              (and (null (search "ephemeral-call" record-source))
                   (null (search "alpha-secret" record-source)))
              "request-local skill names, calls, and results never enter the append-only file")
             (let ((reloaded
                     (conversation-load-by-id
                      configuration
                      "ephemeral-tool-projection")))
               (test-assert
                (and (= (length (conversation-input-items reloaded)) 3)
                     (null
                      (find "ephemeral-call"
                            (conversation-input-items reloaded)
                            :key (lambda (item)
                                   (json-get item "call_id"))
                            :test #'string=)))
                "crash replay omits request-local call correlation without synthesizing repair")))
           (conversation-append-summary conversation "Durable mixed-call work.")
           (test-assert
            (and (= (length
                     (conversation-input-items-for-request conversation))
                    3)
                 (= (length
                     (conversation-input-items-for-request
                      conversation
                      :include-ephemeral-p nil))
                    1))
            "compaction preserves pending correlation only for the next normal request")
           (conversation-clear-ephemeral-input-items conversation)
           (test-assert
            (and (= (length (conversation-input-items conversation)) 1)
                 (null (conversation-ephemeral-input-entries conversation)))
            "a successful provider response consumes all request-local correlation")
           (let ((reloaded
                   (conversation-load-by-id
                    configuration
                    "ephemeral-tool-projection")))
             (test-assert
              (= (length (conversation-input-items reloaded)) 1)
              "replay after compaction contains only the durable summary bridge")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-ephemeral-append-interruption () null)
(defun test-conversation-ephemeral-append-interruption ()
  "Test interrupted request-local insertion remains owned and removable."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration :identifier "ephemeral-append-interruption"))
         (item
           (json-object
            "type" "function_call"
            "call_id" "interrupted-ephemeral-call"
            "namespace" "skill"
            "name" "load"
            "arguments" "{\"name\":\"interrupted-skill\"}"))
         (original-append
           (symbol-function 'conversation--append-input-item)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'conversation--append-input-item
            (lambda (target candidate)
              (funcall original-append target candidate)
              (error "Injected interruption after provider projection append."))))
          (lambda ()
            (test-assert
             (handler-case
                 (progn
                   (conversation--append-ephemeral-input-item conversation item)
                   nil)
               (simple-error ()
                 t))
             "an interruption after projection append reaches the caller")
            (test-assert
             (and
              (eq item (first (conversation-input-items conversation)))
              (eq
               item
               (getf
                (first
                 (conversation-ephemeral-input-entries conversation))
                :item)))
             "an interrupted provider item already has request-local ownership")
            (conversation-clear-ephemeral-input-items conversation)
            (test-assert
             (and
              (null (conversation-input-items conversation))
              (null (conversation-input-items-tail conversation))
              (null (conversation-ephemeral-input-entries conversation)))
             "request-local cleanup removes the interrupted provider item")))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
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

(-> test-conversation-late-duplicate-tool-output () null)
(defun test-conversation-late-duplicate-tool-output ()
  "Test replay keeps the result used before a stale writer appended another."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "duplicate-output"))
         (call
           (json-object
            "type" "function_call"
            "status" "completed"
            "arguments" "{}"
            "call_id" "call-duplicate"
            "name" "run"
            "namespace" "shell")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "run the check")
           (conversation-append-provider-item conversation call)
           (conversation-append-tool-result
            conversation
            "call-duplicate"
            :tool-name "shell.run"
            :output *conversation-interrupted-tool-output*
            :success-p nil)
           (conversation-append-user-message conversation "continue")
           (log-append
            (conversation-pathname conversation)
            `(:tool-result
              :seq 3
              :time ,(get-universal-time)
              :call-id "call-duplicate"
              :tool "shell.run"
              :status :error
              :output "The user denied this command."
              :wire-json
              ,(json-encode
                (function-call-output-item
                 "call-duplicate"
                 "The user denied this command."))))
           (let* ((record-count
                    (length
                     (conversation--read-records
                      (conversation-pathname conversation))))
                  (loaded
                    (conversation-load-by-id configuration "duplicate-output"))
                  (items (conversation-input-items loaded))
                  (outputs
                    (remove-if-not
                     (lambda (item)
                       (and
                        (json-object-p item)
                        (conversation--wire-item-type-p
                         item "function_call_output")))
                     items)))
             (test-assert
              (and (= (length outputs) 1)
                   (string=
                    (json-get (first outputs) "output")
                    *conversation-interrupted-tool-output*))
              "replay keeps the first tool output used by subsequent history")
             (test-assert
              (= (length
                  (conversation--read-records
                   (conversation-pathname loaded)))
                 record-count)
              "replay leaves the stale duplicate only in the append-only log")
             (test-assert
              (string= (json-get (fourth items) "role") "user")
              "replay retains history produced after the selected output")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      call
                      (function-call-output-item
                       "call-duplicate"
                       "first ordinary result")
                      (function-call-output-item
                       "call-duplicate"
                       "second ordinary result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "replay rejects arbitrary conflicting tool outputs")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      call
                      (function-call-output-item
                       "call-duplicate"
                       *conversation-interrupted-tool-output*)
                      (function-call-output-item
                       "call-duplicate"
                       "first stale result")
                      (function-call-output-item
                       "call-duplicate"
                       "second stale result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "replay tolerates only one stale result after a repair")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      (function-call-output-item
                       "call-duplicate"
                       *conversation-interrupted-tool-output*)
                      call
                      (function-call-output-item
                       "call-duplicate"
                       "late ordinary result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "duplicate tolerance requires the call before the repair")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-malformed-tool-projections () null)
(defun test-conversation-malformed-tool-projections ()
  "Test replay rejects impossible durable tool-output projections."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "malformed-tools")))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (conversation--apply-record
                   conversation
                   '(:tool-result
                     :seq 1
                     :call-id "failed-image"
                     :status :error
                     :content-blocks ((:text "impossible"))))
                  nil)
              (conversation-invariant-error ()
                t))
            "replay rejects image-form output on a failed tool result")
           (test-assert
            (handler-case
                (progn
                  (conversation--apply-record
                   conversation
                   '(:tool-result
                     :seq 2
                     :call-id "ambiguous"
                     :status :ok
                     :content-blocks ((:text "one"))
                     :wire-json "{}"))
                  nil)
              (conversation-invariant-error ()
                t))
            "replay rejects a tool result with competing wire projections"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-private-storage () null)
(defun test-conversation-private-storage ()
  "Test private transcript persistence outside public conversation discovery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (storage-root  (merge-pathnames "private/task-transcripts/" root)))
    (unwind-protect
         (let ((conversation
                 (conversation-create configuration
                                      :identifier "private-turn"
                                      :storage-root storage-root)))
           (test-assert (not (probe-file (conversation-pathname conversation)))
                        "an empty private conversation leaves no transcript")
           (conversation-append-user-message conversation "private assignment")
           (test-assert (probe-file (conversation-pathname conversation))
                        "the first private record persists its transcript")
           (let ((loaded (conversation-load
                          (conversation-pathname conversation))))
             (test-assert
              (string= (json-get (first (conversation-input-items loaded))
                                 "role")
                       "user")
              "a private transcript remains directly reloadable"))
           (test-assert
            (not (find (conversation-pathname conversation)
                       (conversation-list configuration)
                       :test #'equal))
            "private transcripts stay out of public conversation listings")
           (test-assert
            (handler-case
                (progn
                  (conversation-load-by-id configuration "private-turn")
                  nil)
              (conversation-error ()
                t))
            "public identifier loading cannot reach a private transcript"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-concurrent-appends () null)
(defun test-conversation-concurrent-appends ()
  "Test concurrent writers retain one contiguous durable sequence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "concurrent-appends"))
         (threads
           (loop for writer below 4
                 collect
                 (make-thread
                  (lambda ()
                    (dotimes (index 25)
                      (conversation-append-record
                       conversation
                       (list :goal
                             :writer writer
                             :index index))))
                  :name (format nil
                                "Autolith conversation writer ~D"
                                writer)))))
    (unwind-protect
         (progn
           (dolist (thread threads)
             (join-thread thread))
           (multiple-value-bind (records incomplete-tail-p)
               (conversation--read-records
                (conversation-pathname conversation))
             (let ((sequences
                     (mapcar (lambda (record)
                               (getf (rest record) :seq))
                             (rest records))))
               (test-assert
                (and (not incomplete-tail-p)
                     (= (length records) 101)
                     (equal sequences
                            (loop for sequence from 1 to 100
                                  collect sequence)))
                "concurrent appends preserve every unique sequence in order"))))
      (dolist (thread threads)
        (when (thread-alive-p thread)
          (join-thread thread)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation--descriptor-read-byte (integer) integer)
(defun test-conversation--descriptor-read-byte (descriptor)
  "Read one synchronization byte from DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type '(unsigned-byte 8)
           :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:read descriptor (sb-sys:vector-sap buffer) 1))))

(-> test-conversation--descriptor-write-byte (integer) integer)
(defun test-conversation--descriptor-write-byte (descriptor)
  "Write one synchronization byte to DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type '(unsigned-byte 8)
           :initial-element 1)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:write descriptor (sb-sys:vector-sap buffer) 1))))

(-> test-conversation--call-with-child-lease
    (configuration string function)
    null)
(defun test-conversation--call-with-child-lease
    (configuration identifier function)
  "Call FUNCTION while a child process holds IDENTIFIER until explicitly released."
  (multiple-value-bind (ready-read ready-write)
      (sb-posix:pipe)
    (multiple-value-bind (release-read release-write)
        (sb-posix:pipe)
      (let ((child-pid (sb-posix:fork))
            (child-status nil))
        (if (zerop child-pid)
            (progn
              (ignore-errors (sb-posix:close ready-read))
              (ignore-errors (sb-posix:close release-write))
              (handler-case
                  (progn
                    (conversation-lease-acquire configuration identifier)
                    (test-conversation--descriptor-write-byte ready-write)
                    (test-conversation--descriptor-read-byte release-read)
                    ;; Deliberately bypass release to exercise kernel cleanup
                    ;; after a dead conversation owner.
                    (sb-posix:_exit 0))
                (serious-condition ()
                  (sb-posix:_exit 1))))
            (progn
              (sb-posix:close ready-write)
              (sb-posix:close release-read)
              (unwind-protect
                   (progn
                     (test-assert
                      (= (test-conversation--descriptor-read-byte ready-read) 1)
                      "the child process acquired its conversation lease")
                     (funcall function))
                (ignore-errors
                  (test-conversation--descriptor-write-byte release-write))
                (ignore-errors
                  (sb-posix:close ready-read))
                (ignore-errors
                  (sb-posix:close release-write))
                (multiple-value-bind (waited-pid status)
                    (sb-posix:waitpid child-pid 0)
                  (test-assert
                   (= waited-pid child-pid)
                   "the conversation lease holder was reaped")
                  (setf child-status status)))
              (test-assert
               (and (sb-posix:wifexited child-status)
                    (zerop (sb-posix:wexitstatus child-status)))
               "the child conversation owner exited cleanly"))))))
  nil)

(-> test-conversation--child-can-acquire-lease-p
    (configuration string)
    boolean)
(defun test-conversation--child-can-acquire-lease-p
    (configuration identifier)
  "Return true when a separate child process can claim IDENTIFIER."
  (let ((child-pid (sb-posix:fork)))
    (if (zerop child-pid)
        (handler-case
            (progn
              (clrhash *conversation-leases*)
              (conversation-lease-acquire configuration identifier)
              (sb-posix:_exit 0))
          (conversation-in-use ()
            (sb-posix:_exit 2))
          (serious-condition ()
            (sb-posix:_exit 1)))
        (multiple-value-bind (waited-pid status)
            (sb-posix:waitpid child-pid 0)
          (and (= waited-pid child-pid)
               (sb-posix:wifexited status)
               (zerop (sb-posix:wexitstatus status)))))))

(-> test-conversation-process-lease () null)
(defun test-conversation-process-lease ()
  "Test live-owner exclusion and automatic lease release after process death."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "K8vQ2mp")
         (conversation
           (conversation-create configuration :identifier identifier)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "persist this session")
           (test-conversation--call-with-child-lease
            configuration
            identifier
            (lambda ()
              (test-assert
               (handler-case
                   (let ((lease
                           (conversation-lease-acquire
                            configuration identifier)))
                     (conversation-lease-release lease)
                     nil)
                 (conversation-in-use (condition)
                   (and
                    (string=
                     (conversation-in-use-identifier condition)
                     identifier)
                    (equal
                     (conversation-error-pathname condition)
                     (conversation-pathname conversation)))))
               "a second process cannot own the active conversation")
              (test-assert
               (and
                (find
                 (conversation-pathname conversation)
                 (conversation-list configuration)
                 :test #'equal)
                (conversation-peek-header
                 (conversation-pathname conversation)))
               "conversation picker enumeration remains read-only while leased")
              (let ((records-seen 0))
                (conversation--map-records
                 (conversation-pathname conversation)
                 (lambda (record)
                   (declare (ignore record))
                   (incf records-seen)))
                (test-assert
                 (= records-seen 2)
                 "history enumeration remains read-only while leased"))))
           (let ((lease
                   (conversation-lease-acquire configuration identifier)))
             (unwind-protect
                  (progn
                    (test-assert
                     (conversation-lease-held-p lease)
                     "a dead process releases its conversation lease")
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-lease-acquire
                            configuration identifier)
                           nil)
                       (conversation-in-use ()
                         t))
                     "one process cannot acquire the same lease twice")
                    (test-assert
                     (not
                      (test-conversation--child-can-acquire-lease-p
                       configuration identifier))
                     "the process-local guard retains the kernel lease"))
               (conversation-lease-release lease))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (test-conversation-image-input)
  (test-conversation-ephemeral-tool-projection)
  (test-conversation-ephemeral-append-interruption)
  (test-conversation-malformed-tool-projections)
  (test-conversation-concurrent-appends)
  (test-conversation-process-lease)
  (test-conversation-interrupted-tool-call)
  (test-conversation-late-duplicate-tool-output)
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
           (test-assert
            (eq (conversation-input-items-tail conversation)
                (last (conversation-input-items conversation)))
            "provider projection appends retain their constant-time tail")
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
             (test-assert (zerop (conversation-log-generation loaded))
                          "a loaded log begins at generation zero")
             (conversation-append-user-message loaded "after interrupted write")
             (test-assert (= (conversation-log-generation loaded) 1)
                          "tail repair invalidates incremental log positions")
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
