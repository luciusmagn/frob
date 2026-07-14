(in-package #:autolith)

;;;; -- Private Image Commits --

(defclass image-commit ()
  ((identifier
    :initarg :identifier
    :reader image-commit-identifier
    :type non-empty-string
    :documentation "The immutable private commit identifier.")
   (directory
    :initarg :directory
    :reader image-commit-directory
    :type pathname
    :documentation "The directory containing this commit's artifacts.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader image-commit-manifest-pathname
    :type pathname
    :documentation "The portable commit manifest pathname.")
   (script-pathname
    :initarg :script-pathname
    :reader image-commit-script-pathname
    :type pathname
    :documentation "The complete Lisp replay script pathname.")
   (parent-identifier
    :initarg :parent-identifier
    :reader image-commit-parent-identifier
    :type (option string)
    :documentation "The preceding private image commit, or NIL at the base.")
   (title
    :initarg :title
    :reader image-commit-title
    :type non-empty-string
    :documentation "The short reason recorded for this private commit.")
   (source-commit
    :initarg :source-commit
    :reader image-commit-source-commit
    :type (option string)
    :documentation "The tracked base source revision, when known.")
   (entries
    :initarg :entries
    :reader image-commit-entries
    :type list
    :documentation "The complete effective replay entries at this commit.")
   (consumed-mutation-identifiers
    :initarg :consumed-mutation-identifiers
    :reader image-commit-consumed-mutation-identifiers
    :type list
    :documentation "Every exploratory mutation consumed by this lineage.")
   (journal-position
    :initarg :journal-position
    :reader image-commit-journal-position
    :type integer
    :documentation "The mutation journal byte position at publication.")
   (created-at
    :initarg :created-at
    :reader image-commit-created-at
    :type timestamp
    :documentation "The commit creation time as Common Lisp universal time."))
  (:documentation
   "An immutable private snapshot of reconstructible live-image mutations."))

(-> image-commit--identifier-p (t) boolean)
(defun image-commit--identifier-p (value)
  "Return true when VALUE is safe as one private commit path component."
  (and (non-empty-string-p value)
       (<= (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(-> image-commit--directory (configuration string) pathname)
(defun image-commit--directory (configuration identifier)
  "Return IDENTIFIER's private commit directory beneath CONFIGURATION."
  (unless (image-commit--identifier-p identifier)
    (error 'image-commit-error
           :message (format nil "Invalid private image commit identifier ~S."
                            identifier)
           :tool-name "self.commit"
           :pathname (configuration-image-commit-root configuration)
           :stage ':manifest))
  (merge-pathnames (format nil "~A/" identifier)
                   (configuration-image-commit-root configuration)))

(-> image-commit--journal-position (configuration) integer)
(defun image-commit--journal-position (configuration)
  "Return CONFIGURATION's current mutation-journal byte position."
  (let ((journal (configuration-journal-path configuration)))
    (if (probe-file journal)
        (with-open-file (stream journal
                                :direction :input
                                :element-type '(unsigned-byte 8))
          (file-length stream))
        0)))

(-> image-commit--write-form-atomically (pathname list) pathname)
(defun image-commit--write-form-atomically (pathname form)
  "Atomically write portable FORM to PATHNAME."
  (let ((temporary
          (make-pathname
           :name (format nil ".~A.~A"
                         (pathname-name pathname)
                         (make-identifier))
           :type "tmp"
           :defaults pathname)))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (let ((*print-circle* t)
                   (*print-readably* t)
                   (*print-pretty* t))
               (prin1 form stream)
               (terpri stream)
               (finish-output stream)))
           (uiop:rename-file-overwriting-target temporary pathname))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))

(-> image-commit--entry-p (t) boolean)
(defun image-commit--entry-p (entry)
  "Return true when ENTRY is one complete portable replay entry."
  (and (listp entry)
       (member (getf entry :kind) '(:definition :set :legacy) :test #'eq)
       (non-empty-string-p (getf entry :id))
       (non-empty-string-p (getf entry :target))
       (stringp (getf entry :source))
       t))

(-> image-commit--manifest-form
    (string (option string) string (option string) pathname list list integer integer)
    list)
(defun image-commit--manifest-form
    (identifier parent-identifier title source-commit script-pathname entries
     consumed-mutation-identifiers journal-position created-at)
  "Return a complete private image-commit manifest form."
  (list :image-commit
        :version 1
        :id identifier
        :parent parent-identifier
        :title title
        :source-commit source-commit
        :script (namestring script-pathname)
        :entries entries
        :consumed-mutations consumed-mutation-identifiers
        :journal-position journal-position
        :created-at created-at))

(-> image-commit-load (configuration string) image-commit)
(defun image-commit-load (configuration identifier)
  "Load and validate private image commit IDENTIFIER from CONFIGURATION."
  (let* ((directory (image-commit--directory configuration identifier))
         (manifest-pathname (merge-pathnames "manifest.sexp" directory)))
    (unless (probe-file manifest-pathname)
      (error 'image-commit-error
             :message (format nil "Private image commit ~A has no manifest."
                              identifier)
             :tool-name "self.commit"
             :pathname manifest-pathname
             :stage ':manifest))
    (let* ((form (read-portable-form manifest-pathname))
           (properties (and (listp form) (rest form)))
           (script-value (and properties (getf properties :script)))
           (script-pathname (and (non-empty-string-p script-value)
                                 (pathname script-value)))
           (entries (and properties (getf properties :entries)))
           (consumed (and properties
                          (getf properties :consumed-mutations))))
      (unless (and (listp form)
                   (eq (first form) :image-commit)
                   (= (or (getf properties :version) 0) 1)
                   (string= (or (getf properties :id) "") identifier)
                   (or (null (getf properties :parent))
                       (image-commit--identifier-p
                        (getf properties :parent)))
                   (non-empty-string-p (getf properties :title))
                   (or (null (getf properties :source-commit))
                       (non-empty-string-p (getf properties :source-commit)))
                   script-pathname
                   (uiop:subpathp script-pathname directory)
                   (probe-file script-pathname)
                   (listp entries)
                   (every #'image-commit--entry-p entries)
                   (listp consumed)
                   (every #'non-empty-string-p consumed)
                   (integerp (getf properties :journal-position))
                   (not (minusp (getf properties :journal-position)))
                   (integerp (getf properties :created-at)))
        (error 'image-commit-error
               :message (format nil "Invalid private image commit manifest at ~A."
                                manifest-pathname)
               :tool-name "self.commit"
               :pathname manifest-pathname
               :stage ':manifest))
      (make-instance 'image-commit
                     :identifier identifier
                     :directory directory
                     :manifest-pathname manifest-pathname
                     :script-pathname script-pathname
                     :parent-identifier (getf properties :parent)
                     :title (getf properties :title)
                     :source-commit (getf properties :source-commit)
                     :entries entries
                     :consumed-mutation-identifiers consumed
                     :journal-position (getf properties :journal-position)
                     :created-at (getf properties :created-at)))))

(-> image-commit--pointer-identifier (configuration) (option string))
(defun image-commit--pointer-identifier (configuration)
  "Return the atomically selected private image commit identifier, if valid."
  (let ((pathname (configuration-current-image-commit-path configuration)))
    (when (probe-file pathname)
      (let ((form (read-portable-form pathname)))
        (unless (and (listp form)
                     (eq (first form) :current-image-commit)
                     (= (or (getf (rest form) :version) 0) 1)
                     (image-commit--identifier-p (getf (rest form) :id)))
          (error 'image-commit-error
                 :message "The current private image-commit pointer is invalid."
                 :tool-name "self.commit"
                 :pathname pathname
                 :stage ':selection))
        (getf (rest form) :id)))))

(-> image-commit-current (configuration) (option image-commit))
(defun image-commit-current (configuration)
  "Return the private commit represented by the running image."
  (let ((identifier
          (if *image-state-initialized-p*
              *active-image-commit-identifier*
              (image-commit--pointer-identifier configuration))))
    (and identifier (image-commit-load configuration identifier))))

(-> image-commit--legacy-entries (configuration) list)
(defun image-commit--legacy-entries (configuration)
  "Return deterministic replay entries for pre-image-commit overlay files."
  (let ((root (configuration-overlay-root configuration)))
    (if (uiop:directory-exists-p root)
        (loop for pathname in (sort (uiop:directory-files root "*.lisp")
                                    #'string<
                                    :key #'namestring)
              collect (list :kind ':legacy
                            :id (format nil "legacy-~A" (pathname-name pathname))
                            :target (enough-namestring pathname root)
                            :source (uiop:read-file-string pathname)))
        nil)))

(-> image-commit-base-entries (configuration) list)
(defun image-commit-base-entries (configuration)
  "Return current committed entries or legacy overlays at the base."
  (let ((current (image-commit-current configuration)))
    (if current
        (copy-tree (image-commit-entries current))
        (image-commit--legacy-entries configuration))))

(-> image-commit-definition-source (configuration string) (option string))
(defun image-commit-definition-source (configuration target)
  "Return TARGET's current private committed definition source, if present."
  (let ((entry
          (find-if (lambda (candidate)
                     (and (eq (getf candidate :kind) :definition)
                          (string= (getf candidate :target) target)))
                   (image-commit-base-entries configuration))))
    (and entry (getf entry :source))))

(-> image-commit--record-p (t string) boolean)
(defun image-commit--record-p (record lineage)
  "Return true when RECORD is a successful reconstructible LINEAGE mutation."
  (and (listp record)
       (eq (first record) :mutation)
       (member (getf (rest record) :kind) '(:definition :set) :test #'eq)
       (eq (getf (rest record) :result) :installed)
       (non-empty-string-p (getf (rest record) :id))
       (string= (or (getf (rest record) :lineage) "") lineage)
       (non-empty-string-p (getf (rest record) :target))
       (stringp (getf (rest record) :proposed))
       t))

(-> image-commit-pending-records (configuration) list)
(defun image-commit-pending-records (configuration)
  "Return successful uncommitted mutations from the running image lineage."
  (unless (and *image-state-initialized-p*
               (non-empty-string-p *active-image-lineage-identifier*))
    (error 'image-commit-error
           :message "The running image has not initialized its mutation lineage."
           :tool-name "self.commit"
           :pathname (configuration-current-image-commit-path configuration)
           :stage ':selection))
  (let* ((current (image-commit-current configuration))
         (consumed (and current
                        (image-commit-consumed-mutation-identifiers current)))
         (seen (make-hash-table :test #'equal))
         (records nil))
    (dolist (record (mutation-journal-read-records configuration))
      (when (image-commit--record-p record
                                    *active-image-lineage-identifier*)
        (let ((identifier (getf (rest record) :id)))
          (unless (or (member identifier consumed :test #'string=)
                      (gethash identifier seen))
            (setf (gethash identifier seen) t)
            (push record records)))))
    (nreverse records)))

(-> image-commit--record->entry (list) list)
(defun image-commit--record->entry (record)
  "Convert one installed mutation journal RECORD to a replay entry."
  (let ((properties (rest record)))
    (list :kind (getf properties :kind)
          :id (getf properties :id)
          :target (getf properties :target)
          :source (getf properties :proposed))))

(-> image-commit--merge-entries (list list) list)
(defun image-commit--merge-entries (base additions)
  "Apply ADDITIONS in order to effective replay entries BASE."
  (let ((result (copy-tree base)))
    (dolist (addition additions)
      (setf result
            (remove-if
             (lambda (entry)
               (and (eq (getf entry :kind) (getf addition :kind))
                    (string= (getf entry :target)
                             (getf addition :target))))
             result))
      (setf result (nconc result (list addition))))
    result))

(-> image-commit--write-entry (stream list) null)
(defun image-commit--write-entry (stream entry)
  "Write one replay ENTRY as executable Common Lisp to STREAM."
  (format stream ";;;; Mutation ~A: ~A~2%"
          (getf entry :id)
          (getf entry :target))
  (case (getf entry :kind)
    (:definition
     (if (overlay--constant-target-p (getf entry :target))
         (overlay--write-constant-form stream (getf entry :source))
         (progn
           (write-string (getf entry :source) stream)
           (fresh-line stream))))
    (:set
     (format stream
             "(setf (symbol-value (quote ~A))~%~6T~A)~%"
             (getf entry :target)
             (getf entry :source)))
    (:legacy
     (write-string (getf entry :source) stream)
     (fresh-line stream)))
  (terpri stream)
  nil)

(-> image-commit-write-script (pathname string string list) pathname)
(defun image-commit-write-script (pathname identifier title entries)
  "Atomically write ENTRIES as IDENTIFIER's complete reconstruction script."
  (let ((temporary
          (make-pathname
           :name (format nil ".~A.~A"
                         (pathname-name pathname)
                         (make-identifier))
           :type "tmp"
           :defaults pathname)))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (format stream ";;;; Autolith image reconstruction script~%")
             (format stream ";;;; Commit ~A: ~A~2%" identifier title)
             (format stream "(in-package #:autolith)~2%")
             (dolist (entry entries)
               (image-commit--write-entry stream entry))
             (finish-output stream))
           (uiop:rename-file-overwriting-target temporary pathname))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))

(-> image-commit--base-source-commit ((option image-commit)) (option string))
(defun image-commit--base-source-commit (parent)
  "Return the known tracked source revision beneath PARENT or this image."
  (or (and parent (image-commit-source-commit parent))
      (and (boundp '*checkpoint-core-probe-record*)
           (let ((record (symbol-value '*checkpoint-core-probe-record*)))
             (and (listp record)
                  (getf (rest record) :git-commit))))
      (and (boundp '*active-image-build-record*)
           (let ((record (symbol-value '*active-image-build-record*)))
             (and (listp record)
                  (getf (rest record) :source-commit))))))

(-> image-commit-publish
    (configuration string list list &key (:identifier (option string)))
    image-commit)
(defun image-commit-publish
    (configuration title mutation-records additional-entries &key identifier)
  "Publish one immutable private commit from mutations and explicit entries."
  (let* ((identifier (or identifier (make-identifier)))
         (parent (image-commit-current configuration))
         (directory (image-commit--directory configuration identifier))
         (script-pathname (merge-pathnames "reconstruct.lisp" directory))
         (manifest-pathname (merge-pathnames "manifest.sexp" directory))
         (record-entries (mapcar #'image-commit--record->entry
                                 mutation-records))
         (entries (image-commit--merge-entries
                   (image-commit-base-entries configuration)
                   (append record-entries additional-entries)))
         (mutation-identifiers
           (append (and parent
                        (copy-list
                         (image-commit-consumed-mutation-identifiers parent)))
                   (mapcar (lambda (record)
                             (getf (rest record) :id))
                           mutation-records)
                   (mapcar (lambda (entry) (getf entry :id))
                           additional-entries)))
         (journal-position (image-commit--journal-position configuration))
         (created-at (get-universal-time))
         (source-commit (image-commit--base-source-commit parent)))
    (when (probe-file directory)
      (error 'image-commit-error
             :message (format nil "Private image commit ~A already exists."
                              identifier)
             :tool-name "self.commit"
             :pathname directory
             :stage ':publish))
    (handler-case
        (progn
          (image-commit-write-script script-pathname identifier title entries)
          (image-commit--write-form-atomically
           manifest-pathname
           (image-commit--manifest-form
            identifier
            (and parent (image-commit-identifier parent))
            title
            source-commit
            script-pathname
            entries
            (remove-duplicates mutation-identifiers :test #'string=)
            journal-position
            created-at))
          (image-commit--write-form-atomically
           (configuration-current-image-commit-path configuration)
           (list :current-image-commit
                 :version 1
                 :id identifier
                 :manifest (namestring manifest-pathname)))
          (setf *active-image-commit-identifier* identifier)
          (image-commit-load configuration identifier))
      (image-commit-error (condition)
        (error condition))
      (error (condition)
        (error 'image-commit-error
               :message (format nil "Could not publish private image commit: ~A"
                                condition)
               :tool-name "self.commit"
               :pathname directory
               :stage ':publish)))))

(-> image-commit-contains-mutation-p (configuration string) boolean)
(defun image-commit-contains-mutation-p (configuration identifier)
  "Return true when the selected private commit contains mutation IDENTIFIER."
  (let ((selected (image-commit--pointer-identifier configuration)))
    (and selected
         (member identifier
                 (image-commit-consumed-mutation-identifiers
                  (image-commit-load configuration selected))
                 :test #'string=)
         t)))

(-> image-state-load (configuration) list)
(defun image-state-load (configuration)
  "Load normal startup mutation state and begin a fresh journal lineage."
  (let ((identifier (image-commit--pointer-identifier configuration))
        (failures nil))
    (setf *active-image-commit-identifier* identifier
          *active-image-lineage-identifier* (make-identifier)
          *image-state-initialized-p* t)
    (if identifier
        (let* ((commit (image-commit-load configuration identifier))
               (pathname (image-commit-script-pathname commit)))
          (handler-case
              (let ((*package* (find-package '#:autolith)))
                (load pathname))
            (error (condition)
              (push (cons pathname (format nil "~A" condition)) failures))))
        (return-from image-state-load (overlay-load-all configuration)))
    (nreverse failures)))

(-> image-state-reconnect () null)
(defun image-state-reconnect ()
  "Preserve the checkpointed commit while beginning a new branch lineage."
  (setf *active-image-lineage-identifier* (make-identifier)
        *image-state-initialized-p* t)
  nil)

(-> image-commit-prepare-checkpoint
    (configuration string &key (:checker mutation-checker))
    (option image-commit))
(defun image-commit-prepare-checkpoint
    (configuration generation-identifier
     &key (checker (make-instance 'standard-mutation-checker)))
  "Commit pending mutations for GENERATION-IDENTIFIER and return current state."
  (let ((records (image-commit-pending-records configuration)))
    (if (null records)
        (image-commit-current configuration)
        (let* ((identifier (make-identifier))
               (title (format nil "Checkpoint generation ~A"
                              generation-identifier))
               (mutation-identifiers
                 (mapcar (lambda (record) (getf (rest record) :id)) records)))
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :image-commit
                 :id identifier
                 :parent *active-image-commit-identifier*
                 :title title
                 :mutations mutation-identifiers
                 :generation generation-identifier
                 :result :pending))
          (handler-case
              (progn
                (mutation-checker-check-active
                 checker
                 configuration
                 (image-commit-render-pending configuration))
                (let ((commit
                        (image-commit-publish
                         configuration title records nil
                         :identifier identifier)))
                  (mutation-journal-append
                   configuration
                   (list :mutation
                         :kind :image-commit
                         :id identifier
                         :parent (image-commit-parent-identifier commit)
                         :title title
                         :mutations mutation-identifiers
                         :generation generation-identifier
                         :script (namestring
                                  (image-commit-script-pathname commit))
                         :result :committed))
                  commit))
            (error (condition)
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind :image-commit
                     :id identifier
                     :parent *active-image-commit-identifier*
                     :title title
                     :mutations mutation-identifiers
                     :generation generation-identifier
                     :result :failed
                     :condition (bounded-string condition :limit 2000)))
              (error condition)))))))

(-> image-commit-write-generation-script
    (configuration pathname string (option image-commit))
    pathname)
(defun image-commit-write-generation-script
    (configuration pathname generation-identifier commit)
  "Write GENERATION-IDENTIFIER's complete base-image reconstruction script."
  (image-commit-write-script
   pathname
   generation-identifier
   "Retained generation reconstruction"
   (if commit
       (image-commit-entries commit)
       (image-commit--legacy-entries configuration))))


;;;; -- Image Commit Tools --

(-> image-commit-render-pending (configuration) string)
(defun image-commit-render-pending (configuration)
  "Render the running image's uncommitted reconstructible mutations."
  (let ((records (image-commit-pending-records configuration)))
    (if records
        (with-output-to-string (stream)
          (dolist (record records)
            (let ((properties (rest record)))
              (format stream "~A  ~A  ~A~%~A~2%"
                      (getf properties :id)
                      (getf properties :kind)
                      (getf properties :target)
                      (getf properties :proposed)))))
        "The active image has no uncommitted reconstructible mutations.")))

(defmethod tool-execute ((tool self-diff-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return reconstructible mutations pending in the running active image."
  (declare (ignore tool arguments))
  (tool-success
   (image-commit-render-pending (tool-context-configuration context))))

(defmethod tool-execute ((tool self-commit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Check and privately commit the running image's pending mutations."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((configuration (tool-context-configuration context))
           (title (self-validate-commit-title
                   (tool-argument arguments "title" :required t)))
           (records (image-commit-pending-records configuration))
           (identifier (make-identifier))
           (mutation-identifiers
             (mapcar (lambda (record) (getf (rest record) :id)) records)))
      (unless records
        (error 'image-commit-error
               :message "The active image has no reconstructible mutations to commit."
               :tool-name "self.commit"
               :pathname (configuration-image-commit-root configuration)
               :stage ':validation))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :image-commit
             :id identifier
             :parent *active-image-commit-identifier*
             :title title
             :mutations mutation-identifiers
             :result :pending))
      (handler-case
          (progn
            (mutation-checker-check-active
             (tool-context-effective-mutation-checker context)
             configuration
             (image-commit-render-pending configuration))
            (let ((commit (image-commit-publish
                           configuration title records nil
                           :identifier identifier)))
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind :image-commit
                     :id identifier
                     :parent (image-commit-parent-identifier commit)
                     :title title
                     :mutations mutation-identifiers
                     :script (namestring
                              (image-commit-script-pathname commit))
                     :result :committed))
              (tool-success
               (format nil
                       "Committed ~D live mutation~:P as private image commit ~A.~%Replay script: ~A"
                       (length records)
                       identifier
                       (namestring (image-commit-script-pathname commit))))))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :image-commit
                 :id identifier
                 :parent *active-image-commit-identifier*
                 :title title
                 :mutations mutation-identifiers
                 :result :failed
                 :condition (bounded-string condition :limit 2000)))
          (error condition))))))
