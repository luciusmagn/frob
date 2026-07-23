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
   (log-generation
    :initform 0
    :accessor conversation-log-generation
    :type (integer 0)
    :documentation "The count of whole-log replacements since this object loaded.")
   (append-lock
    :initform (make-recursive-lock "Autolith conversation append")
    :reader conversation-append-lock
    :type t
    :documentation "The lock serializing durable record sequence assignment.")
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
   (input-items-tail
    :initform nil
    :accessor conversation-input-items-tail
    :type list
    :documentation "The final cons of the provider projection for constant-time append.")
   (ephemeral-input-entries
    :initform nil
    :accessor conversation-ephemeral-input-entries
    :type list
    :documentation
    "Request-local provider items and owned attachments awaiting one response.")
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
    :documentation "The number of durable user message records.")
   (latest-goal-record
    :initform nil
    :accessor conversation-latest-goal-record
    :type (option list)
    :documentation "The newest durable goal record observed in this conversation."))
  (:documentation "An append-only conversation and its provider projection."))

(defmethod initialize-instance
    :after ((conversation conversation) &key &allow-other-keys)
  "Initialize CONVERSATION's constant-time provider projection tail."
  (setf (conversation-input-items-tail conversation)
        (last (conversation-input-items conversation))))

(defmethod (setf conversation-input-items)
    :after ((items list) (conversation conversation))
  "Keep CONVERSATION's projection tail synchronized after whole-list replacement."
  (setf (conversation-input-items-tail conversation) (last items)))


;;;; -- Primary Application Ownership --

(defclass conversation-lease ()
  ((identifier
    :initarg :identifier
    :reader conversation-lease-identifier
    :type non-empty-string
    :documentation "The normalized conversation identifier held by this lease.")
   (pathname
    :initarg :pathname
    :reader conversation-lease-pathname
    :type pathname
    :documentation "The persistent file carrying the process-shared advisory lock.")
   (descriptor
    :initarg :descriptor
    :accessor conversation-lease-descriptor
    :type (option integer)
    :documentation "The open descriptor holding the advisory lock, or NIL after release."))
  (:documentation
   "A process-lifetime exclusive lease on one primary conversation."))

(defvar *conversation-lease-lock*
  (make-lock "Autolith conversation leases")
  "Serialize process-local lease registration and descriptor release.")

(defvar *conversation-leases*
  (make-hash-table :test #'equal)
  "Map lease pathnames to held primary-conversation leases in this process.")

(-> conversation--lease-pathname (configuration string) pathname)
(defun conversation--lease-pathname (configuration identifier)
  "Return the process-shared lease pathname for normalized IDENTIFIER."
  (merge-pathnames
   (make-pathname :name identifier :type "lock")
   (merge-pathnames
    "conversation-leases/"
    (configuration-state-root configuration))))

(-> conversation--lease-in-use (string pathname pathname) null)
(defun conversation--lease-in-use
    (identifier conversation-pathname lease-pathname)
  "Signal that normalized IDENTIFIER already has a live primary owner."
  (error
   'conversation-in-use
   :message
   (format
    nil
    "Conversation ~A is already active in another Autolith process."
    (conversation-identifier-display identifier))
   :pathname conversation-pathname
   :sequence nil
   :identifier identifier
   :lease-pathname lease-pathname))

(-> conversation-lease-held-p (conversation-lease) boolean)
(defun conversation-lease-held-p (lease)
  "Return true when LEASE still owns an open lock descriptor."
  (not (null (conversation-lease-descriptor lease))))

(-> conversation-lease-matches-p (conversation-lease string) boolean)
(defun conversation-lease-matches-p (lease identifier)
  "Return true when held LEASE owns normalized IDENTIFIER."
  (and (conversation-lease-held-p lease)
       (string= (conversation-lease-identifier lease) identifier)))

(-> conversation-lease-acquire (configuration string) conversation-lease)
(defun conversation-lease-acquire (configuration identifier)
  "Acquire the primary process lease for IDENTIFIER without waiting.

The kernel lock is authoritative. Its empty file may remain after normal exit
or a crash, and a later process can immediately reuse it after the former
owner has exited."
  (let* ((normalized
           (conversation-identifier-migration-resolve
            configuration identifier))
         (conversation-pathname
           (conversation-pathname-for-id configuration normalized))
         (lease-pathname
           (conversation--lease-pathname configuration normalized))
         (lease-key (namestring lease-pathname))
         (descriptor nil)
         (acquired-p nil))
    (with-lock-held (*conversation-lease-lock*)
      (let ((existing (gethash lease-key *conversation-leases*)))
        (when (and existing (conversation-lease-held-p existing))
          (conversation--lease-in-use
           normalized conversation-pathname lease-pathname))
        (when existing
          (remhash lease-key *conversation-leases*)))
      (unwind-protect
           (handler-case
               (progn
                 (ensure-directories-exist lease-pathname)
                 (setf descriptor
                       (sb-posix:open
                        (namestring lease-pathname)
                        (logior sb-posix:o-creat sb-posix:o-rdwr)
                        #o600))
                 (sb-posix:lockf descriptor sb-posix:f-tlock 0)
                 (let ((lease
                         (make-instance
                          'conversation-lease
                          :identifier normalized
                          :pathname lease-pathname
                          :descriptor descriptor)))
                   (setf acquired-p t
                         (gethash lease-key *conversation-leases*) lease)
                   lease))
             (sb-posix:syscall-error (condition)
               (if (and
                    descriptor
                    (member
                     (sb-posix:syscall-errno condition)
                     (list sb-posix:eacces sb-posix:eagain)))
                   (conversation--lease-in-use
                    normalized conversation-pathname lease-pathname)
                   (error
                    'conversation-invariant-error
                    :message
                    (format nil
                            "Could not claim conversation ~A: ~A"
                            (conversation-identifier-display normalized)
                            condition)
                    :pathname conversation-pathname
                    :sequence nil)))
             (conversation-error (condition)
               (error condition))
             (error (condition)
               (error
                'conversation-invariant-error
                :message
                (format nil
                        "Could not claim conversation ~A: ~A"
                        (conversation-identifier-display normalized)
                        condition)
                :pathname conversation-pathname
                :sequence nil)))
        (unless acquired-p
          (when descriptor
            (ignore-errors
              (sb-posix:close descriptor))))))))

(-> conversation-lease-release (conversation-lease) null)
(defun conversation-lease-release (lease)
  "Release LEASE idempotently.

Closing the descriptor is the final authority even when an explicit unlock
reports an operating-system failure."
  (with-lock-held (*conversation-lease-lock*)
    (let ((descriptor (conversation-lease-descriptor lease))
          (lease-key (namestring (conversation-lease-pathname lease))))
      (when descriptor
        (when (eq (gethash lease-key *conversation-leases*) lease)
          (remhash lease-key *conversation-leases*))
        (setf (conversation-lease-descriptor lease) nil)
        (ignore-errors
          (sb-posix:lockf descriptor sb-posix:f-ulock 0))
        (ignore-errors
          (sb-posix:close descriptor)))))
  nil)


;;;; -- Durable Projection --

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
                        (:storage-root (option pathname))
                        (:created-at (option timestamp)))
    conversation)
(defun conversation-create
    (configuration &key identifier storage-root created-at)
  "Create an in-memory conversation that persists under optional STORAGE-ROOT."
  (let* ((created-at (or created-at (get-universal-time)))
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
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (unless (keywordp (first record))
      (error 'conversation-invariant-error
             :message "A conversation record must begin with a keyword."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (let* ((sequence (conversation-next-sequence conversation))
           (repair-tail-p (conversation-incomplete-tail-p conversation))
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
                 :repair-tail-p repair-tail-p)
                (setf (conversation-incomplete-tail-p conversation) nil)
                (when repair-tail-p
                  (incf (conversation-log-generation conversation))))
              (conversation--write-initial-record conversation sequenced))
        (error (condition)
          (error 'conversation-invariant-error
                 :message
                 (format nil
                         "Could not append conversation record: ~A"
                         condition)
                 :pathname (conversation-pathname conversation)
                 :sequence sequence)))
      (incf (conversation-next-sequence conversation))
      (conversation--note-activity conversation sequenced)
      (when (eq (first sequenced) :goal)
        (setf (conversation-latest-goal-record conversation) sequenced))
      sequenced)))

(-> conversation--append-input-item (conversation json-object) json-object)
(defun conversation--append-input-item (conversation item)
  "Append provider ITEM to CONVERSATION's in-memory chronological projection."
  (let ((cell (list item))
        (tail (conversation-input-items-tail conversation)))
    (if tail
        (setf (rest tail) cell)
        (setf (conversation-input-items conversation) cell))
    (setf (conversation-input-items-tail conversation) cell))
  item)

(-> conversation--append-ephemeral-input-item
    (conversation json-object &key (:attachments list))
    json-object)
(defun conversation--append-ephemeral-input-item
    (conversation item &key attachments)
  "Append request-local ITEM and record any owned ATTACHMENTS for cleanup."
  (let ((entries
          (append
           (conversation-ephemeral-input-entries conversation)
           (list (list :item item :attachments attachments)))))
    ;; Publish ownership before mutating the provider projection. An interrupt
    ;; after the projection append can then never leave an untagged item.
    (setf (conversation-ephemeral-input-entries conversation) entries)
    (conversation--append-input-item conversation item))
  item)

(-> conversation-input-items-for-request
    (conversation &key (:include-ephemeral-p boolean))
    list)
(defun conversation-input-items-for-request
    (conversation &key (include-ephemeral-p t))
  "Return a fresh provider projection, optionally excluding request-local items."
  (if include-ephemeral-p
      (copy-list (conversation-input-items conversation))
      (let ((ephemeral-items (make-hash-table :test #'eq)))
        (dolist (entry (conversation-ephemeral-input-entries conversation))
          (setf (gethash (getf entry :item) ephemeral-items) t))
        (remove-if
         (lambda (item)
           (gethash item ephemeral-items))
         (conversation-input-items conversation)))))

(-> conversation-clear-ephemeral-input-items (conversation) null)
(defun conversation-clear-ephemeral-input-items (conversation)
  "Remove all request-local provider items and their owned image artifacts."
  (let ((entries (conversation-ephemeral-input-entries conversation)))
    (when entries
      (let ((ephemeral-items (make-hash-table :test #'eq)))
        (dolist (entry entries)
          (setf (gethash (getf entry :item) ephemeral-items) t))
        (setf (conversation-input-items conversation)
              (remove-if
               (lambda (item)
                 (gethash item ephemeral-items))
               (conversation-input-items conversation))
              (conversation-ephemeral-input-entries conversation) nil))
      (dolist (entry entries)
        (dolist (attachment (getf entry :attachments))
          (ignore-errors
            (when (probe-file (image-attachment-pathname attachment))
              (delete-file (image-attachment-pathname attachment))))))))
  nil)

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
    (values json-object list))
(defun conversation-append-user-message (conversation input)
  "Persist user INPUT and return its provider item and sequenced record."
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
         (record nil)
         (durable-p nil))
    (unwind-protect
         (progn
           (setf record
                 (conversation-append-record
                  conversation
                  (append
                   (list :message
                         :role :user
                         :content content)
                   (when attachments
                     (list :images
                           (mapcar #'image-attachment-record attachments)))
                   (unless attachments
                     (list :wire-json (json-encode item))))))
           (setf durable-p t
                 (conversation-turn-state conversation) nil)
           (values (conversation--append-input-item conversation item)
                   record))
      (unless durable-p
        (conversation--delete-image-attachments attachments)))))

(-> conversation-append-provider-item
    (conversation json-object
     &key (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-provider-item
    (conversation item &key (persistence ':durable))
  "Append one authoritative provider ITEM with the requested PERSISTENCE."
  (ecase persistence
    (:durable
     (conversation-append-record
      conversation
      (list :provider-item
            :wire-json (json-encode item)))
     (conversation--append-input-item conversation item))
    (:next-response
     (conversation--append-ephemeral-input-item conversation item))))

(-> function-call-output-item (string (or string vector)) json-object)
(defun function-call-output-item (call-id output)
  "Return a Responses API function-call output correlated by CALL-ID."
  (json-object
   "type" "function_call_output"
   "call_id" call-id
   "output" output))

(-> conversation--tool-content-output (list) vector)
(defun conversation--tool-content-output (blocks)
  "Return ordered string and image BLOCKS as native tool-output content."
  (coerce
   (mapcar
    (lambda (block)
      (etypecase block
        (string
         (json-object "type" "input_text" "text" block))
        (image-attachment
         (image-input-content-item block))))
    blocks)
   'vector))

(-> conversation--tool-content-block-record (t) list)
(defun conversation--tool-content-block-record (block)
  "Return one portable durable descriptor for provider content BLOCK."
  (etypecase block
    (string
     (list :text block))
    (image-attachment
     (list :image (image-attachment-record block)))))

(-> conversation--tool-content-images (list) list)
(defun conversation--tool-content-images (blocks)
  "Return every image attachment in ordered provider BLOCKS."
  (remove-if-not
   (lambda (block)
     (typep block 'image-attachment))
   blocks))

(defparameter *conversation-interrupted-tool-output*
  "Autolith restarted before recording this tool call's result. The call may have changed external state. Inspect the relevant state before deciding whether to retry it."
  "The provider-visible result synthesized for a tool call interrupted by exit.")

(-> conversation-append-tool-result
    (conversation string
     &key (:tool-name string)
          (:output string)
          (:image-attachments list)
          (:content-blocks list)
          (:success-p boolean)
          (:cpu-microseconds (option (integer 0)))
          (:real-microseconds (option (integer 0)))
          (:persistence tool-conversation-persistence))
    json-object)
(defun conversation-append-tool-result
    (conversation call-id
     &key tool-name output image-attachments content-blocks success-p
       cpu-microseconds real-microseconds (persistence ':durable))
  "Append one tool OUTPUT, optional ordered content, timing, and PERSISTENCE."
  (when (and image-attachments content-blocks)
    (error 'conversation-invariant-error
           :message
           "Tool output cannot provide both image attachments and content blocks."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let* ((blocks
           (or content-blocks
               (when image-attachments
                 (append
                  (when (non-empty-string-p output)
                    (list output))
                  image-attachments))))
         (attachments (conversation--tool-content-images blocks))
         (retained-p nil))
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
           (unless (every
                    (lambda (block)
                      (or (stringp block)
                          (typep block 'image-attachment)))
                    blocks)
             (error 'conversation-invariant-error
                    :message "Tool output contains an invalid content block."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (when (and attachments (not success-p))
             (error 'conversation-invariant-error
                    :message "A failed tool result cannot contain image output."
                    :pathname (conversation-pathname conversation)
                    :sequence (conversation-next-sequence conversation)))
           (let* ((wire-output
                    (if attachments
                        (conversation--tool-content-output blocks)
                        output))
                  (item (function-call-output-item call-id wire-output)))
             (ecase persistence
               (:durable
                (conversation-append-record
                 conversation
                 (append
                  (list :tool-result
                        :call-id call-id
                        :tool tool-name
                        :status (if success-p :ok :error)
                        :output output)
                  (when attachments
                    (list
                     :content-blocks
                     (mapcar #'conversation--tool-content-block-record blocks)))
                  (when cpu-microseconds
                    (list :cpu-microseconds cpu-microseconds
                          :real-microseconds real-microseconds))
                  (unless attachments
                    (list :wire-json (json-encode item)))))
                (setf retained-p t)
                (conversation--append-input-item conversation item))
               (:next-response
                (conversation--append-ephemeral-input-item
                 conversation
                 item
                 :attachments attachments)
                (setf retained-p t)))
             item))
      (unless retained-p
        (conversation--delete-image-attachments attachments)))))

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
  "Return unique function calls and the first correlated outputs in ITEMS.

A late writer can append a real tool result after crash recovery has already
recorded an unknown-outcome result and continued the conversation. Tolerate
only that recognizable ordering and preserve the repair because subsequent
history was produced from its projection.  The late result remains in the
append-only log but must not enter provider replay."
  (let ((calls (make-hash-table :test #'equal))
        (outputs (make-hash-table :test #'equal))
        (outputs-after-call-p (make-hash-table :test #'equal))
        (stale-output-tolerated-p (make-hash-table :test #'equal)))
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
             (multiple-value-bind (existing present-p)
                 (gethash call-id outputs)
               (if (not present-p)
                   (setf (gethash call-id outputs) item
                         (gethash call-id outputs-after-call-p)
                         (not (null (gethash call-id calls))))
                   (let ((existing-output (json-get existing "output")))
                     (if (and
                          (gethash call-id outputs-after-call-p)
                          (not
                           (gethash call-id stale-output-tolerated-p))
                          (stringp existing-output)
                          (string=
                           existing-output
                           *conversation-interrupted-tool-output*))
                         (setf
                          (gethash call-id stale-output-tolerated-p)
                          t)
                         (error
                          'conversation-invariant-error
                          :message
                          (format
                           nil
                           "Persisted history repeats output for tool call ~S."
                           call-id)
                          :pathname (conversation-pathname conversation)
                          :sequence nil))))))))))
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
                                   :output *conversation-interrupted-tool-output*
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
                 *supported-reasoning-efforts*
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

(defparameter *conversation-summary-prefix*
  "A previous segment of this conversation was compacted. The summary below replaces that segment; use it to continue seamlessly without repeating completed work."
  "The bridge text introducing a compaction summary to the model.")

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
                              *conversation-summary-prefix*
                              content)))))

(-> conversation-append-summary (conversation string) list)
(defun conversation-append-summary (conversation content)
  "Persist a compaction summary and replace CONVERSATION's projection with it.

The durable record covers every record before it, so replay reproduces the
same compacted projection. The provider turn-state token is dropped because
it described the uncompacted context."
  (let* ((ephemeral-items
           (mapcar
            (lambda (entry)
              (getf entry :item))
            (conversation-ephemeral-input-entries conversation)))
         (record (conversation-append-record
                  conversation
                  (list :summary
                        :through-seq (1- (conversation-next-sequence
                                          conversation))
                        :content content))))
    (setf (conversation-input-items conversation)
          (cons (conversation-summary-item content) ephemeral-items)
          (conversation-turn-state conversation) nil
          (conversation-last-total-tokens conversation) 0)
    record))


;;;; -- Conversation Loading --

(-> conversation--map-records
    (pathname function &key (:start-position (integer 0)))
    (values integer boolean integer))
(defun conversation--map-records (pathname function &key (start-position 0))
  "Call FUNCTION for complete records in PATHNAME from START-POSITION.

Return the next readable file position, whether the final form is incomplete,
and the number of records visited. Storage failures become conversation
invariant errors while callback conditions propagate unchanged."
  (let ((callback-store-error nil))
    (handler-case
        (log-map
         (lambda (record)
           (handler-case
               (funcall function record)
             (store-error (condition)
               (setf callback-store-error condition)
               (error condition))))
         pathname
         :start-position start-position)
      (store-error (condition)
        (if (eq condition callback-store-error)
            (error condition)
            (error 'conversation-invariant-error
                   :message (format nil "Malformed conversation record: ~A"
                                    condition)
                   :pathname pathname
                   :sequence nil))))))

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

(-> conversation--tool-content-block-from-record
    (conversation list (option integer))
    t)
(defun conversation--tool-content-block-from-record
    (conversation descriptor sequence)
  "Restore one durable tool content DESCRIPTOR for CONVERSATION."
  (cond
    ((and (listp descriptor)
          (stringp (getf descriptor :text))
          (null (getf descriptor :image)))
     (getf descriptor :text))
    ((and (listp descriptor)
          (getf descriptor :image)
          (null (getf descriptor :text)))
     (image-attachment-from-record
      (getf descriptor :image)
      (conversation-image-artifact-root conversation)))
    (t
     (error 'conversation-invariant-error
            :message "A persisted tool content block is invalid."
            :pathname (conversation-pathname conversation)
            :sequence sequence))))

(-> conversation--property-present-p (list keyword) boolean)
(defun conversation--property-present-p (properties indicator)
  "Return true when property list PROPERTIES contains INDICATOR."
  (loop for tail on properties by #'cddr
        thereis (eq (first tail) indicator)))

(-> conversation--apply-record (conversation list) null)
(defun conversation--apply-record (conversation record)
  "Project one persisted RECORD into CONVERSATION's in-memory state."
  (unless (and (listp record) (keywordp (first record)))
    (error 'conversation-invariant-error
           :message "A persisted conversation record is not a keyword list."
           :pathname (conversation-pathname conversation)
           :sequence nil))
  (let* ((properties (rest record))
         (sequence (getf properties :seq))
         (wire-json (getf properties :wire-json))
         (content-blocks-p
           (conversation--property-present-p properties :content-blocks))
         (images-p
           (conversation--property-present-p properties :images))
         (wire-json-p
           (conversation--property-present-p properties :wire-json)))
    (when (eq (first record) :tool-result)
      (when (> (count t (list content-blocks-p images-p wire-json-p)) 1)
        (error 'conversation-invariant-error
               :message
               "A persisted tool result contains multiple wire projections."
               :pathname (conversation-pathname conversation)
               :sequence sequence))
      (when (and (or content-blocks-p images-p)
                 (not (eq (getf properties :status) :ok)))
        (error 'conversation-invariant-error
               :message
               "A failed persisted tool result cannot contain image output."
               :pathname (conversation-pathname conversation)
               :sequence sequence)))
    (conversation--note-activity conversation record)
    (when (eq (first record) :goal)
      (setf (conversation-latest-goal-record conversation) record))
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
               content-blocks-p)
      (let ((call-id (getf (rest record) :call-id))
            (descriptors (getf (rest record) :content-blocks)))
        (unless (consp descriptors)
          (error 'conversation-invariant-error
                 :message
                 "A persisted multimodal tool result has no content blocks."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (unless (non-empty-string-p call-id)
          (error 'conversation-invariant-error
                 :message
                 "A persisted multimodal tool result has no call identifier."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((blocks
                (mapcar
                 (lambda (descriptor)
                   (conversation--tool-content-block-from-record
                    conversation descriptor sequence))
                 descriptors)))
          (conversation--append-input-item
           conversation
           (function-call-output-item
            call-id
            (conversation--tool-content-output blocks))))))
    (when (and (eq (first record) :tool-result)
               images-p)
      (let ((call-id (getf (rest record) :call-id))
            (descriptors (getf (rest record) :images)))
        (unless (consp descriptors)
          (error 'conversation-invariant-error
                 :message "A persisted image tool result has no images."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (unless (non-empty-string-p call-id)
          (error 'conversation-invariant-error
                 :message "A persisted image tool result has no call identifier."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (let ((attachments
                (mapcar
                 (lambda (descriptor)
                   (image-attachment-from-record
                    descriptor
                    (conversation-image-artifact-root conversation)))
                 descriptors)))
          (conversation--append-input-item
           conversation
           (function-call-output-item
            call-id
            (conversation--tool-content-output
             (append
              (when (non-empty-string-p
                     (or (getf (rest record) :output) ""))
                (list (getf (rest record) :output)))
              attachments)))))))
    (when (and (member (first record)
                       '(:message :provider-item :tool-result))
               (stringp wire-json)
               (not images-p)
               (not content-blocks-p))
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

(-> conversation--from-header (pathname list) conversation)
(defun conversation--from-header (pathname header)
  "Validate HEADER and return its empty in-memory conversation projection."
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
         (reasoning-effort (getf (rest header) :reasoning-effort)))
    (unless (or (and (null model) (null reasoning-effort))
                (conversation--model-selection-p model reasoning-effort))
      (error 'conversation-invariant-error
             :message "The conversation header has an invalid model selection."
             :pathname pathname
             :sequence nil))
    (make-instance 'conversation
                   :identifier (getf (rest header) :id)
                   :pathname pathname
                   :persisted-p t
                   :incomplete-tail-p nil
                   :created-at (getf (rest header) :created-at)
                   :origin-directory (and (stringp directory) directory)
                   :model (and (non-empty-string-p model) model)
                   :reasoning-effort
                   (and (non-empty-string-p reasoning-effort)
                        reasoning-effort)
                   :next-sequence 1
                   :input-items nil)))

(-> conversation-load (pathname) conversation)
(defun conversation-load (pathname)
  "Load a conversation from PATHNAME and rebuild its provider input projection."
  (let ((conversation nil))
    (multiple-value-bind (position incomplete-tail-p count)
        (conversation--map-records
         pathname
         (lambda (record)
           (if conversation
               (conversation--apply-record conversation record)
               (setf conversation
                     (conversation--from-header pathname record)))))
      (declare (ignore position count))
      (unless conversation
        (error 'conversation-invariant-error
               :message "The conversation header is missing or unsupported."
               :pathname pathname
               :sequence nil))
      (setf (conversation-incomplete-tail-p conversation)
            incomplete-tail-p)
      (conversation--repair-incomplete-tool-calls conversation)
      conversation)))

(-> conversation-pathname-for-id (configuration string) pathname)
(defun conversation-pathname-for-id (configuration identifier)
  "Return CONFIGURATION's conversation pathname for IDENTIFIER."
  (merge-pathnames (make-pathname
                    :name
                    (conversation-identifier-migration-resolve
                     configuration identifier)
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
      (let ((header-seen-p nil))
        (conversation--map-records
         pathname
         (lambda (record)
           (cond
             ((not header-seen-p)
              (unless (and (listp record)
                           (eq (first record) :conversation))
                (return-from conversation--pathname-non-empty-p nil))
              (setf header-seen-p t))
             (t
              (return-from conversation--pathname-non-empty-p t)))))
        nil)
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
