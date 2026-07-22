(in-package #:autolith)

;;;; -- Conversation Object --

(defclass conversation ()
  ((identifier
    :initarg :identifier
    :reader conversation-identifier
    :type non-empty-string
    :documentation "The stable conversation identifier.")
   (pathname
    :initarg :pathname
    :reader conversation-pathname
    :type pathname
    :documentation "The append-only S-expression file once persistence begins.")
   (persisted-p
    :initarg :persisted-p
    :accessor conversation-persisted-p
    :type boolean
    :documentation "True after the header and first durable record are published.")
   (incomplete-tail-p
    :initarg :incomplete-tail-p
    :initform nil
    :accessor conversation-incomplete-tail-p
    :type boolean
    :documentation "Whether the next append must repair an interrupted final form.")
   (created-at
    :initarg :created-at
    :reader conversation-created-at
    :type timestamp
    :documentation "The creation time as Common Lisp universal time.")
   (origin-directory
    :initarg :origin-directory
    :initform nil
    :reader conversation-origin-directory
    :type (option string)
    :documentation "The workspace directory in which this conversation began.")
   (model
    :initarg :model
    :initform nil
    :accessor conversation-model
    :type (option non-empty-string)
    :documentation "The provider model most recently selected for this conversation.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :accessor conversation-reasoning-effort
    :type (option non-empty-string)
    :documentation "The reasoning effort most recently selected for this conversation.")
   (next-sequence
    :initarg :next-sequence
    :accessor conversation-next-sequence
    :type integer
    :documentation "The sequence number assigned to the next appended event.")
   (input-items
    :initarg :input-items
    :accessor conversation-input-items
    :type list
    :documentation "Provider wire items in chronological order.")
   (turn-state
    :initform nil
    :accessor conversation-turn-state
    :type (option string)
    :documentation "The transient provider routing token for one user turn.")
   (last-total-tokens
    :initform 0
    :accessor conversation-last-total-tokens
    :type (integer 0)
    :documentation "The total token usage reported by the newest provider step.")
   (last-activity-at
    :initform nil
    :accessor conversation-last-activity-at
    :type (option timestamp)
    :documentation "The newest timestamp observed in a durable record.")
   (user-turn-count
    :initform 0
    :accessor conversation-user-turn-count
    :type (integer 0)
    :documentation "The number of durable user message records."))
  (:documentation "An append-only conversation and its provider projection."))

(-> conversation--note-activity (conversation list) null)
(defun conversation--note-activity (conversation record)
  "Project RECORD's activity metadata into CONVERSATION."
  (let ((time (and (consp record) (getf (rest record) :time))))
    (when (typep time 'timestamp)
      (setf (conversation-last-activity-at conversation)
            (max (or (conversation-last-activity-at conversation) 0) time))))
  (when (and (consp record)
             (eq (first record) ':message)
             (eq (getf (rest record) :role) ':user))
    (incf (conversation-user-turn-count conversation)))
  nil)

(-> conversation--header-record (conversation) list)
(defun conversation--header-record (conversation)
  "Return CONVERSATION's portable file header."
  (list :conversation
        :version 1
        :id (conversation-identifier conversation)
        :created-at (conversation-created-at conversation)
        :directory (conversation-origin-directory conversation)
        :model (conversation-model conversation)
        :reasoning-effort (conversation-reasoning-effort conversation)))

(-> conversation--write-initial-record (conversation list) null)
(defun conversation--write-initial-record (conversation record)
  "Atomically publish CONVERSATION's header and first durable RECORD."
  (let ((pathname (conversation-pathname conversation)))
    (when (probe-file pathname)
      (error 'conversation-invariant-error
             :message "A new conversation pathname became occupied."
             :pathname pathname
             :sequence (conversation-next-sequence conversation)))
    (log-append pathname
                record
                :initial-forms
                (list (conversation--header-record conversation)))
    (setf (conversation-persisted-p conversation) t
          (conversation-incomplete-tail-p conversation) nil))
  nil)

(-> conversation-create
    (configuration &key (:identifier (option string))
                        (:storage-root (option pathname)))
    conversation)
(defun conversation-create (configuration &key identifier storage-root)
  "Create an in-memory conversation that persists under optional STORAGE-ROOT."
  (let* ((created-at (get-universal-time))
         (root (uiop:ensure-directory-pathname
                (or storage-root
                    (configuration-conversation-root configuration))))
         (conversation-id
           (or identifier
               (conversation-identifier-generate root :timestamp created-at)))
         (origin-directory (namestring
                            (configuration-working-directory configuration)))
         (pathname (merge-pathnames
                    (make-pathname :name conversation-id :type "sexp")
                    root)))
    (when (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A already exists." conversation-id)
             :pathname pathname
             :sequence nil))
    (make-instance 'conversation
                   :identifier conversation-id
                   :pathname pathname
                   :persisted-p nil
                   :incomplete-tail-p nil
                   :created-at created-at
                   :origin-directory origin-directory
                   :model (configuration-model configuration)
                   :reasoning-effort
                   (configuration-reasoning-effort configuration)
                   :next-sequence 1
                   :input-items nil)))

(-> conversation-append-record (conversation list) list)
(defgeneric conversation-append-record (conversation record)
  (:documentation "Append portable RECORD to CONVERSATION and return the sequenced form."))

(defmethod conversation-append-record ((conversation conversation) (record list))
  "Assign metadata, initialize persistence if needed, and append RECORD."
  (unless (keywordp (first record))
    (error 'conversation-invariant-error
           :message "A conversation record must begin with a keyword."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let* ((sequence (conversation-next-sequence conversation))
         (sequenced (list* (first record)
                           :seq sequence
                           :time (get-universal-time)
                           (rest record))))
    (handler-case
        (if (conversation-persisted-p conversation)
            (progn
              (unless (probe-file (conversation-pathname conversation))
                (error 'conversation-invariant-error
                       :message "The persisted conversation file is missing."
                       :pathname (conversation-pathname conversation)
                       :sequence sequence))
              (log-append
               (conversation-pathname conversation)
               sequenced
               :repair-tail-p (conversation-incomplete-tail-p conversation))
              (setf (conversation-incomplete-tail-p conversation) nil))
            (conversation--write-initial-record conversation sequenced))
      (error (condition)
        (error 'conversation-invariant-error
               :message (format nil "Could not append conversation record: ~A" condition)
               :pathname (conversation-pathname conversation)
               :sequence sequence)))
    (incf (conversation-next-sequence conversation))
    (conversation--note-activity conversation sequenced)
    sequenced))

(-> conversation--append-input-item (conversation json-object) json-object)
(defun conversation--append-input-item (conversation item)
  "Append provider ITEM to CONVERSATION's in-memory chronological projection."
  (setf (conversation-input-items conversation)
        (nconc (conversation-input-items conversation) (list item)))
  item)

(-> conversation-image-artifact-root (conversation) pathname)
(defun conversation-image-artifact-root (conversation)
  "Return CONVERSATION's private binary image artifact directory."
  (let* ((conversation-root
           (uiop:pathname-directory-pathname
            (conversation-pathname conversation)))
         (data-root (uiop:pathname-parent-directory-pathname conversation-root)))
    (merge-pathnames
     (format nil "conversation-images/~A/"
             (conversation-identifier conversation))
     data-root)))

(-> user-message-item (string &optional list) json-object)
(defun user-message-item (content &optional attachments)
  "Return a Responses API user message containing CONTENT and ATTACHMENTS."
  (json-object
   "type" "message"
   "role" "user"
   "content"
   (coerce
    (append
     (loop for attachment in attachments
           for label-number from 1
           append (image-input-content-items attachment label-number))
     (when (non-empty-string-p content)
       (list (json-object
              "type" "input_text"
              "text" content))))
    'vector)))

(-> conversation--prepare-images (conversation list) list)
(defun conversation--prepare-images (conversation image-pathnames)
  "Prepare IMAGE-PATHNAMES transactionally for CONVERSATION."
  (let ((attachments nil))
    (handler-case
        (progn
          (dolist (pathname image-pathnames)
            (push (image-input-prepare
                   pathname
                   (conversation-image-artifact-root conversation))
                  attachments))
          (nreverse attachments))
      (error (condition)
        (dolist (attachment attachments)
          (when (probe-file (image-attachment-pathname attachment))
            (delete-file (image-attachment-pathname attachment))))
        (error condition)))))

(-> conversation--delete-image-attachments (list) null)
(defun conversation--delete-image-attachments (attachments)
  "Delete newly prepared ATTACHMENTS after a failed durable append."
  (dolist (attachment attachments)
    (when (probe-file (image-attachment-pathname attachment))
      (delete-file (image-attachment-pathname attachment))))
  nil)

(-> conversation-append-user-message
    (conversation (or string user-message-input))
    json-object)
(defun conversation-append-user-message (conversation input)
  "Persist user INPUT and its image descriptors before projecting it."
  (let* ((submission
           (etypecase input
             (string (user-message-input-create :text input))
             (user-message-input input)))
         (content (user-message-input-text submission))
         (attachments
           (conversation--prepare-images
            conversation
            (user-message-input-image-pathnames submission)))
         (item (user-message-item content attachments))
         (durable-p nil))
    (unwind-protect
         (progn
           (conversation-append-record
            conversation
            (append
             (list :message
                   :role :user
                   :content content)
             (when attachments
               (list :images (mapcar #'image-attachment-record attachments)))
             (unless attachments
               (list :wire-json (json-encode item)))))
           (setf durable-p t
                 (conversation-turn-state conversation) nil)
           (conversation--append-input-item conversation item))
      (unless durable-p
        (conversation--delete-image-attachments attachments)))))

(-> conversation-append-provider-item (conversation json-object) json-object)
(defun conversation-append-provider-item (conversation item)
  "Persist one authoritative completed provider ITEM in CONVERSATION."
  (conversation-append-record
   conversation
   (list :provider-item
         :wire-json (json-encode item)))
  (conversation--append-input-item conversation item))

(-> function-call-output-item (string (or string vector)) json-object)
(defun function-call-output-item (call-id output)
  "Return a Responses API function-call output correlated by CALL-ID."
  (json-object
   "type" "function_call_output"
   "call_id" call-id
   "output" output))

(-> conversation--image-tool-output (list) vector)
(defun conversation--image-tool-output (attachments)
  "Return ATTACHMENTS as native image content for a tool output."
  (coerce (mapcar #'image-input-content-item attachments) 'vector))

(define-constant +conversation-interrupted-tool-output+
  "Autolith restarted before recording this tool call's result. The call may have changed external state. Inspect the relevant state before deciding whether to retry it."
  :test #'string=
  :documentation
  "The provider-visible result synthesized for a tool call interrupted by exit.")

(-> conversation-append-tool-result
    (conversation string
     &key (:tool-name string)
          (:output string)
          (:image-attachments list)
          (:success-p boolean)
          (:cpu-microseconds (option (integer 0)))
          (:real-microseconds (option (integer 0))))
    json-object)
(defun conversation-append-tool-result
    (conversation call-id
     &key tool-name output image-attachments success-p
       cpu-microseconds real-microseconds)
  "Persist and append one tool OUTPUT, optional images, and optional timing."
  (let ((durable-p nil))
    (unwind-protect
         (progn
           (unless (or (and (null cpu-microseconds)
                            (null real-microseconds))
                       (and (typep cpu-microseconds '(integer 0))
                            (typep real-microseconds '(integer 0))))
             (error 'conversation-invariant-error
                    :message
                    "Tool timing must contain both nonnegative microsecond values."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (unless (every (lambda (attachment)
                            (typep attachment 'image-attachment))
                          image-attachments)
             (error 'conversation-invariant-error
                    :message "Tool image output contains an invalid attachment."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (when (and image-attachments (not success-p))
             (error 'conversation-invariant-error
                    :message "A failed tool result cannot contain image output."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (let* ((wire-output
                    (if image-attachments
                        (conversation--image-tool-output image-attachments)
                        output))
                  (item (function-call-output-item call-id wire-output)))
             (conversation-append-record
              conversation
              (append
               (list :tool-result
                     :call-id call-id
                     :tool tool-name
                     :status (if success-p :ok :error)
                     :output output)
               (when image-attachments
                 (list :images
                       (mapcar #'image-attachment-record image-attachments)))
               (when cpu-microseconds
                 (list :cpu-microseconds cpu-microseconds
                       :real-microseconds real-microseconds))
               (unless image-attachments
                 (list :wire-json (json-encode item)))))
             (setf durable-p t)
             (conversation--append-input-item conversation item)))
      (unless durable-p
        (conversation--delete-image-attachments image-attachments)))))

(-> conversation--wire-item-type-p (json-object string) boolean)
(defun conversation--wire-item-type-p (item type)
  "Return true when provider ITEM has wire TYPE."
  (string= (or (json-get item "type") "") type))

(-> conversation--tool-call-id (conversation json-object) string)
(defun conversation--tool-call-id (conversation item)
  "Return ITEM's non-empty tool call identifier or signal corrupted history."
  (let ((call-id (json-get item "call_id")))
    (unless (non-empty-string-p call-id)
      (error 'conversation-invariant-error
             :message "A persisted tool item has no call identifier."
             :pathname (conversation-pathname conversation)
             :sequence nil))
    call-id))

(-> conversation--tool-call-name (json-object) string)
(defun conversation--tool-call-name (item)
  "Return a readable canonical name for function call ITEM."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (cond
      ((and (non-empty-string-p namespace) (non-empty-string-p name))
       (format nil "~A.~A" namespace name))
      ((non-empty-string-p name)
       name)
      ((non-empty-string-p namespace)
       namespace)
      (t
       "unknown"))))

(-> conversation--tool-item-tables
    (conversation list)
    (values hash-table hash-table))
(defun conversation--tool-item-tables (conversation items)
  "Return unique function calls and correlated outputs found in ITEMS."
  (let ((calls (make-hash-table :test #'equal))
        (outputs (make-hash-table :test #'equal)))
    (dolist (item items)
      (when (json-object-p item)
        (cond
          ((conversation--wire-item-type-p item "function_call")
           (let ((call-id (conversation--tool-call-id conversation item)))
             (when (gethash call-id calls)
               (error 'conversation-invariant-error
                      :message
                      (format nil "Persisted history repeats tool call ~S."
                              call-id)
                      :pathname (conversation-pathname conversation)
                      :sequence nil))
             (setf (gethash call-id calls) item)))
          ((conversation--wire-item-type-p item "function_call_output")
           (let ((call-id (conversation--tool-call-id conversation item)))
             (when (gethash call-id outputs)
               (error 'conversation-invariant-error
                      :message
                      (format nil "Persisted history repeats output for tool call ~S."
                              call-id)
                      :pathname (conversation-pathname conversation)
                      :sequence nil))
             (setf (gethash call-id outputs) item))))))
    (values calls outputs)))

(-> conversation--repair-incomplete-tool-calls (conversation) null)
(defun conversation--repair-incomplete-tool-calls (conversation)
  "Pair every persisted function call with an output after an interrupted exit.

Existing outputs are moved beside their calls in the provider projection. A
missing output is recorded append-only as an explicit unknown-outcome failure
before the repaired projection can be sent to the provider."
  (let ((items (copy-list (conversation-input-items conversation))))
    (multiple-value-bind (calls outputs)
        (conversation--tool-item-tables conversation items)
      (let ((remaining items)
            (repaired nil))
        (loop while remaining
              for item = (pop remaining)
              do (cond
                   ((and (json-object-p item)
                         (conversation--wire-item-type-p
                          item "function_call_output"))
                    (let ((call-id
                            (conversation--tool-call-id conversation item)))
                      ;; Correlated outputs are emitted with their call group.
                      ;; Orphaned legacy outputs retain their original position.
                      (unless (gethash call-id calls)
                        (push item repaired))))
                   ((and (json-object-p item)
                         (conversation--wire-item-type-p item "function_call"))
                    (let ((group (list item)))
                      (loop while (and remaining
                                       (json-object-p (first remaining))
                                       (conversation--wire-item-type-p
                                        (first remaining) "function_call"))
                            do (setf group
                                     (nconc group (list (pop remaining)))))
                      (dolist (call group)
                        (push call repaired))
                      (dolist (call group)
                        (let* ((call-id
                                 (conversation--tool-call-id conversation call))
                               (output (gethash call-id outputs)))
                          (unless output
                            (setf output
                                  (conversation-append-tool-result
                                   conversation
                                   call-id
                                   :tool-name
                                   (conversation--tool-call-name call)
                                   :output +conversation-interrupted-tool-output+
                                   :success-p nil)
                                  (gethash call-id outputs) output))
                          (push output repaired)))))
                   (t
                    (push item repaired))))
        (setf (conversation-input-items conversation) (nreverse repaired)))))
  nil)

(-> conversation--usage-total (t) (option integer))
(defun conversation--usage-total (usage)
  "Return the total token count carried by portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (let ((total (json-get usage "total_tokens")))
       (and (integerp total) total)))
    ((listp usage)
     (let ((total (second (assoc "total_tokens" usage :test #'equal))))
       (and (integerp total) total)))
    (t
     nil)))

(-> conversation-append-provider-metadata (conversation list) list)
(defun conversation-append-provider-metadata (conversation metadata)
  "Persist portable provider METADATA that is not part of request history."
  (let ((total (conversation--usage-total (getf metadata :usage))))
    (when total
      (setf (conversation-last-total-tokens conversation) total)))
  (conversation-append-record
   conversation
   (list :provider :metadata metadata)))

(-> conversation--model-selection-p (t t) boolean)
(defun conversation--model-selection-p (model reasoning-effort)
  "Return true when MODEL and REASONING-EFFORT form a restorable selection."
  (and (non-empty-string-p model)
       (non-empty-string-p reasoning-effort)
       (not
        (null
         (member reasoning-effort
                 +supported-reasoning-efforts+
                 :test #'string=)))))

(-> conversation-set-model-selection (conversation string string) null)
(defun conversation-set-model-selection (conversation model reasoning-effort)
  "Remember MODEL and REASONING-EFFORT without persisting an empty conversation."
  (unless (conversation--model-selection-p model reasoning-effort)
    (error 'conversation-invariant-error
           :message "A conversation model selection is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (unless (and (string= model (or (conversation-model conversation) ""))
               (string= reasoning-effort
                        (or (conversation-reasoning-effort conversation) "")))
    (when (conversation-persisted-p conversation)
      (conversation-append-record
       conversation
       (list :configuration
             :model model
             :reasoning-effort reasoning-effort)))
    (setf (conversation-model conversation) model
          (conversation-reasoning-effort conversation) reasoning-effort))
  nil)

(define-constant +conversation-summary-prefix+
  "A previous segment of this conversation was compacted. The summary below replaces that segment; use it to continue seamlessly without repeating completed work."
  :test #'string=
  :documentation "The bridge text introducing a compaction summary to the model.")

(-> conversation-summary-item (string) json-object)
(defun conversation-summary-item (content)
  "Return the replayable wire item carrying a compaction summary CONTENT."
  (json-object
   "type" "message"
   "role" "user"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" (format nil "~A~2%~A"
                              +conversation-summary-prefix+
                              content)))))

(-> conversation-append-summary (conversation string) list)
(defun conversation-append-summary (conversation content)
  "Persist a compaction summary and replace CONVERSATION's projection with it.

The durable record covers every record before it, so replay reproduces the
same compacted projection. The provider turn-state token is dropped because
it described the uncompacted context."
  (let ((record (conversation-append-record
                 conversation
                 (list :summary
                       :through-seq (1- (conversation-next-sequence
                                         conversation))
                       :content content))))
    (setf (conversation-input-items conversation)
          (list (conversation-summary-item content))
          (conversation-turn-state conversation) nil
          (conversation-last-total-tokens conversation) 0)
    record))


;;;; -- Conversation Loading --

(-> conversation--read-records (pathname) (values list boolean))
(defun conversation--read-records (pathname)
  "Read complete forms and report whether PATHNAME has an incomplete tail."
  (handler-case
      (log-read pathname)
    (error (condition)
      (error 'conversation-invariant-error
             :message (format nil "Malformed conversation record: ~A"
                              condition)
             :pathname pathname
             :sequence nil))))

(-> conversation--apply-record (conversation list) null)
(defun conversation--apply-record (conversation record)
  "Project one persisted RECORD into CONVERSATION's in-memory state."
  (unless (and (listp record) (keywordp (first record)))
    (error 'conversation-invariant-error
           :message "A persisted conversation record is not a keyword list."
           :pathname (conversation-pathname conversation)
           :sequence nil))
  (let ((sequence (getf (rest record) :seq))
        (wire-json (getf (rest record) :wire-json)))
    (conversation--note-activity conversation record)
    (when (integerp sequence)
      (setf (conversation-next-sequence conversation)
            (max (conversation-next-sequence conversation) (1+ sequence))))
    (when (and (eq (first record) :message)
               (getf (rest record) :images))
      (let* ((content (getf (rest record) :content))
             (attachments
               (mapcar
                (lambda (descriptor)
                  (image-attachment-from-record
                   descriptor
                   (conversation-image-artifact-root conversation)))
                (getf (rest record) :images))))
        (unless (stringp content)
          (error 'conversation-invariant-error
                 :message "A persisted image message has invalid text content."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item
         conversation
         (user-message-item content attachments))))
    (when (and (eq (first record) :tool-result)
               (getf (rest record) :images))
      (let* ((call-id (getf (rest record) :call-id))
             (attachments
               (mapcar
                (lambda (descriptor)
                  (image-attachment-from-record
                   descriptor
                   (conversation-image-artifact-root conversation)))
                (getf (rest record) :images))))
        (unless (non-empty-string-p call-id)
          (error 'conversation-invariant-error
                 :message "A persisted image tool result has no call identifier."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item
         conversation
         (function-call-output-item
          call-id
          (conversation--image-tool-output attachments)))))
    (when (and (member (first record)
                       '(:message :provider-item :tool-result))
               (stringp wire-json)
               (not (getf (rest record) :images)))
      (let ((item (json-decode wire-json)))
        (unless (json-object-p item)
          (error 'conversation-invariant-error
                 :message "A persisted provider item is not a JSON object."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item conversation item)))
    (when (eq (first record) :summary)
      (let ((content (getf (rest record) :content)))
        (when (stringp content)
          (setf (conversation-input-items conversation)
                (list (conversation-summary-item content))
                (conversation-last-total-tokens conversation) 0))))
    (when (eq (first record) :provider)
      (let ((total (conversation--usage-total
                    (getf (getf (rest record) :metadata) :usage))))
        (when total
          (setf (conversation-last-total-tokens conversation) total))))
    (when (eq (first record) :configuration)
      (let ((model (getf (rest record) :model))
            (reasoning-effort (getf (rest record) :reasoning-effort)))
        (unless (conversation--model-selection-p model reasoning-effort)
          (error 'conversation-invariant-error
                 :message "A persisted conversation model selection is invalid."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (setf (conversation-model conversation) model
              (conversation-reasoning-effort conversation) reasoning-effort))))
  nil)

(-> conversation-peek-header (pathname) (option list))
(defun conversation-peek-header (pathname)
  "Return PATHNAME's leading conversation header form, or NIL when unreadable.

Only the first top-level form is read, so peeking stays cheap for large
conversation files."
  (handler-case
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker)))
          (if (and (listp form)
                   (eq (first form) :conversation))
              form
              nil)))
    (error ()
      nil)))

(-> conversation-load (pathname) conversation)
(defun conversation-load (pathname)
  "Load a conversation from PATHNAME and rebuild its provider input projection."
  (multiple-value-bind (records incomplete-tail-p)
      (conversation--read-records pathname)
    (let ((header (first records)))
    (unless (and (listp header)
                 (eq (first header) :conversation)
                 (= (or (getf (rest header) :version) 0) 1)
                 (non-empty-string-p (getf (rest header) :id)))
      (error 'conversation-invariant-error
             :message "The conversation header is missing or unsupported."
             :pathname pathname
             :sequence nil))
    (let* ((directory (getf (rest header) :directory))
           (model (getf (rest header) :model))
           (reasoning-effort (getf (rest header) :reasoning-effort))
           (conversation
             (make-instance 'conversation
                            :identifier (getf (rest header) :id)
                            :pathname pathname
                            :persisted-p t
                            :incomplete-tail-p incomplete-tail-p
                            :created-at (getf (rest header) :created-at)
                            :origin-directory (and (stringp directory)
                                                   directory)
                            :model (and (non-empty-string-p model) model)
                            :reasoning-effort
                            (and (non-empty-string-p reasoning-effort)
                                 reasoning-effort)
                            :next-sequence 1
                            :input-items nil)))
      (unless (or (and (null model) (null reasoning-effort))
                  (conversation--model-selection-p model reasoning-effort))
        (error 'conversation-invariant-error
               :message "The conversation header has an invalid model selection."
               :pathname pathname
               :sequence nil))
      (dolist (record (rest records))
        (conversation--apply-record conversation record))
      (conversation--repair-incomplete-tool-calls conversation)
      conversation))))

(-> conversation-pathname-for-id (configuration string) pathname)
(defun conversation-pathname-for-id (configuration identifier)
  "Return CONFIGURATION's conversation pathname for IDENTIFIER."
  (merge-pathnames (make-pathname
                    :name
                    (handler-case
                        (conversation-identifier-normalize identifier)
                      (conversation-identifier-error ()
                        identifier))
                    :type "sexp")
                   (configuration-conversation-root configuration)))

(-> conversation-load-by-id (configuration string) conversation)
(defun conversation-load-by-id (configuration identifier)
  "Load IDENTIFIER from CONFIGURATION's conversation directory."
  (let ((pathname (conversation-pathname-for-id configuration identifier)))
    (unless (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A does not exist." identifier)
             :pathname pathname
             :sequence nil))
    (conversation-load pathname)))

(-> conversation--pathname-non-empty-p (pathname) boolean)
(defun conversation--pathname-non-empty-p (pathname)
  "Return true when PATHNAME has a header and at least one complete record."
  (handler-case
      (let ((records (conversation--read-records pathname)))
        (not
         (null
          (and (consp records)
               (listp (first records))
               (eq (first (first records)) :conversation)
               (rest records)))))
    (error ()
      nil)))

(-> conversation-list (configuration) list)
(defun conversation-list (configuration)
  "Return non-empty conversation pathnames, newest first."
  (let ((root (configuration-conversation-root configuration)))
    (if (probe-file root)
        (sort
         (remove-if-not
          #'conversation--pathname-non-empty-p
          (uiop:directory-files root "*.sexp"))
         #'>
         :key (lambda (pathname)
                (or (file-write-date pathname) 0)))
        nil)))
