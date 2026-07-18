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
   (history-commit
    :initarg :history-commit
    :initform nil
    :reader image-commit-history-commit
    :type (option string)
    :documentation "The private Git commit retaining this snapshot's artifacts.")
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

(-> image-history--commit-p (t) boolean)
(defun image-history--commit-p (value)
  "Return true when VALUE is a full hexadecimal Git object identifier."
  (and (stringp value)
       (member (length value) '(40 64))
       (every (lambda (character) (digit-char-p character 16)) value)
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
           (uiop:rename-file-overwriting-target temporary pathname)
           (sb-posix:chmod (namestring pathname) #o444))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))

(-> image-commit--write-string-atomically (pathname string) pathname)
(defun image-commit--write-string-atomically (pathname content)
  "Atomically write CONTENT to PATHNAME and make the result read-only."
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
             (write-string content stream)
             (finish-output stream))
           (uiop:rename-file-overwriting-target temporary pathname)
           (sb-posix:chmod (namestring pathname) #o444))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))


;;;; -- Private Git History --

(-> image-history--git-command (configuration list) string)
(defun image-history--git-command (configuration arguments)
  "Run Git ARGUMENTS in CONFIGURATION's private mutation-history repository."
  (uiop:run-program
   (append (list "git" "-C"
                 (namestring
                  (configuration-mutation-history-root configuration)))
           arguments)
   :output :string
   :error-output :output))

(-> image-history--ensure-repository (configuration) pathname)
(defun image-history--ensure-repository (configuration)
  "Create CONFIGURATION's private mutation-history Git repository when absent."
  (let ((root (configuration-mutation-history-root configuration)))
    (ensure-directories-exist root)
    (unless (uiop:directory-exists-p (merge-pathnames ".git/" root))
      (uiop:run-program
       (list "git" "init" "--quiet" (namestring root))
       :output :string
       :error-output :output))
    root))

(-> image-history--artifact-directory (configuration string) pathname)
(defun image-history--artifact-directory (configuration identifier)
  "Return IDENTIFIER's working-tree directory in private mutation history."
  (merge-pathnames (format nil "commits/~A/" identifier)
                   (configuration-mutation-history-root configuration)))

(-> image-history--artifact-specification (string string string) string)
(defun image-history--artifact-specification
    (history-commit identifier artifact-name)
  "Return Git's object specification for ARTIFACT-NAME in one snapshot."
  (format nil "~A:commits/~A/~A"
          history-commit
          identifier
          artifact-name))

(-> image-history-commit
    (configuration &key (:identifier string)
                        (:title string)
                        (:manifest-pathname pathname)
                        (:script-pathname pathname))
    string)
(defun image-history-commit
    (configuration &key identifier title manifest-pathname script-pathname)
  "Commit IDENTIFIER's replay artifacts to private Git history under TITLE."
  (let* ((root (configuration-mutation-history-root configuration))
         (directory (image-history--artifact-directory configuration
                                                        identifier))
         (relative-directory (format nil "commits/~A" identifier))
         (history-manifest (merge-pathnames "manifest.sexp" directory))
         (history-script (merge-pathnames "reconstruct.lisp" directory)))
    (handler-case
        (progn
          (image-history--ensure-repository configuration)
          (when (probe-file directory)
            (error 'image-commit-error
                   :message
                   (format nil
                           "Private mutation history already contains ~A."
                           identifier)
                   :tool-name "self.commit"
                   :pathname directory
                   :stage ':history))
          (ensure-directories-exist history-manifest)
          (uiop:copy-file manifest-pathname history-manifest)
          (uiop:copy-file script-pathname history-script)
          (sb-posix:chmod (namestring history-manifest) #o444)
          (sb-posix:chmod (namestring history-script) #o444)
          (image-history--git-command
           configuration
           (list "add" "--" relative-directory))
          (image-history--git-command
           configuration
           (list "-c" "user.name=Autolith"
                 "-c" "user.email=autolith@localhost"
                 "commit" "--quiet" "--no-gpg-sign" "--only"
                 "-m" title "--" relative-directory))
          (let ((commit
                  (string-trim
                   '(#\Space #\Tab #\Newline #\Return)
                   (image-history--git-command
                    configuration
                    '("rev-parse" "HEAD")))))
            (unless (image-history--commit-p commit)
              (error "Git returned an invalid mutation-history commit."))
            commit))
      (image-commit-error (condition)
        (error condition))
      (error (condition)
        (error 'image-commit-error
               :message (format nil "Could not retain mutation history: ~A"
                                condition)
               :tool-name "self.commit"
               :pathname root
               :stage ':history)))))

(-> image-history--restore-artifact
    (configuration &key (:history-commit string)
                        (:identifier string)
                        (:artifact-name string)
                        (:pathname pathname))
    pathname)
(defun image-history--restore-artifact
    (configuration &key history-commit identifier artifact-name pathname)
  "Restore one canonical replay artifact from a private Git commit."
  (handler-case
      (image-commit--write-string-atomically
       pathname
       (image-history--git-command
        configuration
        (list "show"
              (image-history--artifact-specification
               history-commit identifier artifact-name))))
    (error (condition)
      (error 'image-commit-error
             :message (format nil "Could not restore private image commit: ~A"
                              condition)
             :tool-name "self.commit"
             :pathname pathname
             :stage ':history))))

(-> image-history-restore (configuration string string) null)
(defun image-history-restore (configuration identifier history-commit)
  "Restore missing canonical artifacts for IDENTIFIER from HISTORY-COMMIT."
  (unless (image-history--commit-p history-commit)
    (error 'image-commit-error
           :message "The selected mutation-history Git identity is invalid."
           :tool-name "self.commit"
           :pathname (configuration-current-image-commit-path configuration)
           :stage ':selection))
  (let* ((directory (image-commit--directory configuration identifier))
         (manifest-pathname (merge-pathnames "manifest.sexp" directory))
         (script-pathname (merge-pathnames "reconstruct.lisp" directory)))
    (unless (probe-file manifest-pathname)
      (image-history--restore-artifact
       configuration
       :history-commit history-commit
       :identifier identifier
       :artifact-name "manifest.sexp"
       :pathname manifest-pathname))
    (unless (probe-file script-pathname)
      (image-history--restore-artifact
       configuration
       :history-commit history-commit
       :identifier identifier
       :artifact-name "reconstruct.lisp"
       :pathname script-pathname)))
  nil)

(-> image-commit--entry-p (t) boolean)
(defun image-commit--entry-p (entry)
  "Return true when ENTRY is one complete portable replay entry."
  (and (listp entry)
       (member (getf entry :kind) '(:definition :set :legacy) :test #'eq)
       (non-empty-string-p (getf entry :id))
       (non-empty-string-p (getf entry :target))
       (stringp (getf entry :source))
       (or (null (getf entry :package))
           (non-empty-string-p (getf entry :package)))
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

(-> image-commit-load
    (configuration string &key (:history-commit (option string)))
    image-commit)
(defun image-commit-load (configuration identifier &key history-commit)
  "Load and validate private image commit IDENTIFIER from CONFIGURATION."
  (let* ((directory (image-commit--directory configuration identifier))
         (manifest-pathname (merge-pathnames "manifest.sexp" directory)))
    (when history-commit
      (image-history-restore configuration identifier history-commit))
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
                     :history-commit history-commit
                     :entries entries
                     :consumed-mutation-identifiers consumed
                     :journal-position (getf properties :journal-position)
                     :created-at (getf properties :created-at)))))

(-> image-commit--pointer-state
    (configuration)
    (values (option string) (option string)))
(defun image-commit--pointer-state (configuration)
  "Return selected private image and history commit identifiers, if valid."
  (let ((pathname (configuration-current-image-commit-path configuration)))
    (if (probe-file pathname)
      (let ((form (read-portable-form pathname)))
        (let* ((properties (and (listp form) (rest form)))
               (version (and properties (getf properties :version)))
               (history-commit (and properties
                                    (getf properties :history-commit))))
          (unless (and (listp form)
                       (eq (first form) :current-image-commit)
                       (member version '(1 2))
                       (image-commit--identifier-p (getf properties :id))
                       (or (= version 1)
                           (image-history--commit-p history-commit)))
            (error 'image-commit-error
                   :message "The current private image-commit pointer is invalid."
                   :tool-name "self.commit"
                   :pathname pathname
                   :stage ':selection))
          (values (getf properties :id)
                  (and (= version 2) history-commit))))
        (values nil nil))))

(-> image-commit--pointer-identifier (configuration) (option string))
(defun image-commit--pointer-identifier (configuration)
  "Return the atomically selected private image commit identifier, if valid."
  (image-commit--pointer-state configuration))

(-> image-commit-current (configuration) (option image-commit))
(defun image-commit-current (configuration)
  "Return the private commit represented by the running image."
  (if *image-state-initialized-p*
      (and *active-image-commit-identifier*
           (image-commit-load
            configuration
            *active-image-commit-identifier*
            :history-commit *active-image-history-commit*))
      (multiple-value-bind (identifier history-commit)
          (image-commit--pointer-state configuration)
        (and identifier
             (image-commit-load configuration identifier
                                :history-commit history-commit)))))

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
       (or (null (getf (rest record) :package))
           (non-empty-string-p (getf (rest record) :package)))
       t))

(-> image-commit--discard-record-p (t string) boolean)
(defun image-commit--discard-record-p (record lineage)
  "Return true when RECORD completes a discard in LINEAGE."
  (and (listp record)
       (eq (first record) :mutation)
       (eq (getf (rest record) :kind) :discard)
       (eq (getf (rest record) :result) :discarded)
       (non-empty-string-p (getf (rest record) :id))
       (string= (or (getf (rest record) :lineage) "") lineage)
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
         (records-by-identifier (make-hash-table :test #'equal))
         (order nil))
    (dolist (record (mutation-journal-read-records configuration))
      (cond
        ((image-commit--record-p record *active-image-lineage-identifier*)
         (let ((identifier (getf (rest record) :id)))
           (unless (or (member identifier consumed :test #'string=)
                       (gethash identifier records-by-identifier))
             (setf (gethash identifier records-by-identifier) record)
             (setf order (nconc order (list identifier))))))
        ((image-commit--discard-record-p
          record
          *active-image-lineage-identifier*)
         (remhash (getf (rest record) :id) records-by-identifier))))
    (loop for identifier in order
          for record = (gethash identifier records-by-identifier)
          when record
            collect record)))

(-> image-commit-effective-pending-records (configuration) list)
(defun image-commit-effective-pending-records (configuration)
  "Return the newest pending mutation for each semantic kind and target."
  (let ((effective nil))
    (dolist (record (image-commit-pending-records configuration))
      (let ((properties (rest record)))
        (setf effective
              (remove-if
               (lambda (candidate)
                 (let ((candidate-properties (rest candidate)))
                   (and (eq (getf candidate-properties :kind)
                            (getf properties :kind))
                        (string= (getf candidate-properties :target)
                                 (getf properties :target)))))
               effective))
        (setf effective (nconc effective (list record)))))
    effective))

(-> image-commit--matching-entry (configuration list) (option list))
(defun image-commit--matching-entry (configuration record)
  "Return the committed replay entry matching pending RECORD, when present."
  (let ((properties (rest record)))
    (find-if
     (lambda (entry)
       (and (eq (getf entry :kind) (getf properties :kind))
            (string= (getf entry :target) (getf properties :target))))
     (image-commit-base-entries configuration))))

(-> image-commit--same-target-p (list list) boolean)
(defun image-commit--same-target-p (left right)
  "Return true when LEFT and RIGHT mutate the same semantic target."
  (let ((left-properties (rest left))
        (right-properties (rest right)))
    (and (eq (getf left-properties :kind)
             (getf right-properties :kind))
         (string= (getf left-properties :target)
                  (getf right-properties :target))
         t)))

(-> image-commit--pending-baseline (configuration list list) (option string))
(defun image-commit--pending-baseline (configuration record pending)
  "Return RECORD's committed or first-journaled baseline source."
  (let ((entry (image-commit--matching-entry configuration record)))
    (or (and entry (getf entry :source))
        (let ((first-record
                (find-if
                 (lambda (candidate)
                   (image-commit--same-target-p candidate record))
                 pending)))
          (and first-record (getf (rest first-record) :previous))))))

(-> image-commit--proposal-equal-p (list string string) boolean)
(defun image-commit--proposal-equal-p (record baseline proposed)
  "Return true when BASELINE and PROPOSED contain the same readable form."
  (handler-case
      (let ((package
              (self-resolve-package (getf (rest record) :package))))
        (and (equal (self-read-form baseline
                                    :read-eval nil
                                    :package package)
                    (self-read-form proposed
                                    :read-eval nil
                                    :package package))
             t))
    (error ()
      (and (string= baseline proposed) t))))

(-> image-commit-effective-diff-records (configuration) list)
(defun image-commit-effective-diff-records (configuration)
  "Return one pending record per target whose proposal differs from its base."
  (let ((pending (image-commit-pending-records configuration)))
    (remove-if
     (lambda (record)
       (let ((baseline
               (image-commit--pending-baseline configuration record pending)))
         (and baseline
              (image-commit--proposal-equal-p
               record
               baseline
               (getf (rest record) :proposed)))))
     (image-commit-effective-pending-records configuration))))

(-> image-commit--record->entry (list) list)
(defun image-commit--record->entry (record)
  "Convert one installed mutation journal RECORD to a replay entry."
  (let ((properties (rest record)))
    (list :kind (getf properties :kind)
          :id (getf properties :id)
          :target (getf properties :target)
          :package (or (getf properties :package) "AUTOLITH")
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
     (format stream
             "(self-replay-definition ~S ~S)~%"
             (or (getf entry :package) "AUTOLITH")
             (getf entry :source)))
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
           (uiop:rename-file-overwriting-target temporary pathname)
           (sb-posix:chmod (namestring pathname) #o444))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))


;;;; -- Clean Replay Probe --

(define-constant +image-commit-replay-probe-argument+
  "--autolith-internal-image-commit-replay-probe"
  :test #'string=
  :documentation "The private command argument requesting a clean replay probe.")

(define-constant +image-commit-replay-probe-version+ 1
  :documentation "The clean replay probe protocol version.")

(-> image-commit-replay-probe-output (string) string)
(defun image-commit-replay-probe-output (identifier)
  "Return the canonical success marker for private commit IDENTIFIER."
  (format nil "(:AUTOLITH-IMAGE-COMMIT-REPLAY :VERSION ~D :ID ~S)"
          +image-commit-replay-probe-version+
          identifier))

(-> image-commit-replay-probe-main (string string) null)
(defun image-commit-replay-probe-main (script-name identifier)
  "Load SCRIPT-NAME in a clean source process and print its probe identity."
  (unless (image-commit--identifier-p identifier)
    (error 'image-commit-error
           :message "The clean replay probe received an invalid commit identity."
           :tool-name "self.commit"
           :pathname nil
           :stage ':replay-probe))
  (let ((script (pathname script-name)))
    (unless (probe-file script)
      (error 'image-commit-error
             :message "The clean replay probe script does not exist."
             :tool-name "self.commit"
             :pathname script
             :stage ':replay-probe))
    (let ((*package* (find-package '#:autolith)))
      (load script)))
  (write-string (image-commit-replay-probe-output identifier)
                *standard-output*)
  (terpri *standard-output*)
  (finish-output *standard-output*)
  nil)

(-> image-commit-replay-probe (configuration pathname string) null)
(defun image-commit-replay-probe (configuration script identifier)
  "Require SCRIPT to load successfully in a clean pinned Autolith process."
  (let* ((source-root (configuration-source-root configuration))
         (entry (merge-pathnames "bin/autolith-active" source-root))
         (configured-command (uiop:getenv "AUTOLITH_SBCL"))
         (sbcl-command (if (non-empty-string-p configured-command)
                           configured-command
                           "sbcl"))
         (expected (image-commit-replay-probe-output identifier))
         (output
           (handler-case
               (uiop:run-program
                (list sbcl-command
                      "--noinform"
                      "--script"
                      (namestring entry)
                      +image-commit-replay-probe-argument+
                      (namestring script)
                      identifier)
                :input nil
                :output :string
                :error-output :output)
             (error (condition)
               (error 'image-commit-error
                      :message
                      (format nil "The clean private replay probe failed: ~A"
                              condition)
                      :tool-name "self.commit"
                      :pathname script
                      :stage ':replay-probe)))))
    (unless (search expected output :test #'char=)
      (error 'image-commit-error
             :message "The clean private replay probe returned no success marker."
             :tool-name "self.commit"
             :pathname script
             :stage ':replay-probe)))
  nil)

(defvar *image-commit-replay-probe-function* #'image-commit-replay-probe
  "The clean-process replay boundary used before selecting a private commit.")

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
          (funcall *image-commit-replay-probe-function*
                   configuration
                   script-pathname
                   identifier)
          (let* ((history-commit
                   (image-history-commit
                    configuration
                    :identifier identifier
                    :title title
                    :manifest-pathname manifest-pathname
                    :script-pathname script-pathname))
                 (commit
                   (image-commit-load configuration identifier
                                      :history-commit history-commit)))
            (image-commit--write-form-atomically
             (configuration-current-image-commit-path configuration)
             (list :current-image-commit
                   :version 2
                   :id identifier
                   :manifest (namestring manifest-pathname)
                   :history-commit history-commit))
            (setf *active-image-commit-identifier* identifier
                  *active-image-history-commit* history-commit)
            (dolist (mutation-record mutation-records)
              (remhash (getf (rest mutation-record) :id)
                       *exploratory-undo-actions*))
            commit))
      (image-commit-error (condition)
        (when (probe-file directory)
          (uiop:delete-directory-tree directory
                                      :validate t
                                      :if-does-not-exist :ignore))
        (error condition))
      (error (condition)
        (when (probe-file directory)
          (uiop:delete-directory-tree directory
                                      :validate t
                                      :if-does-not-exist :ignore))
        (error 'image-commit-error
               :message (format nil "Could not publish private image commit: ~A"
                                condition)
               :tool-name "self.commit"
               :pathname directory
               :stage ':publish)))))

(-> image-commit-contains-mutation-p (configuration string) boolean)
(defun image-commit-contains-mutation-p (configuration identifier)
  "Return true when the selected private commit contains mutation IDENTIFIER."
  (multiple-value-bind (selected history-commit)
      (image-commit--pointer-state configuration)
    (and selected
         (member identifier
                 (image-commit-consumed-mutation-identifiers
                  (image-commit-load
                   configuration selected :history-commit history-commit))
                 :test #'string=)
         t)))

(-> image-state-load (configuration) list)
(defun image-state-load (configuration)
  "Load normal startup mutation state and begin a fresh journal lineage."
  (clrhash *exploratory-undo-actions*)
  (multiple-value-bind (identifier history-commit)
      (image-commit--pointer-state configuration)
    (let ((failures nil))
      (setf *active-image-commit-identifier* identifier
            *active-image-history-commit* history-commit
            *active-image-lineage-identifier* (make-identifier)
            *image-state-initialized-p* t)
      (if identifier
          (let* ((commit (image-commit-load
                          configuration identifier
                          :history-commit history-commit))
                 (pathname (image-commit-script-pathname commit)))
            (handler-case
                (let ((*package* (find-package '#:autolith)))
                  (load pathname))
              (error (condition)
                (push (cons pathname (format nil "~A" condition)) failures))))
          (return-from image-state-load (overlay-load-all configuration)))
      (nreverse failures))))

(-> image-state-reconnect () null)
(defun image-state-reconnect ()
  "Preserve the checkpointed commit while beginning a new branch lineage."
  (clrhash *exploratory-undo-actions*)
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
                         :history-commit
                         (image-commit-history-commit commit)
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
  "Render the running image's effective uncommitted mutation state."
  (let* ((records (image-commit-pending-records configuration))
         (effective (image-commit-effective-diff-records configuration)))
    (cond
      (effective
        (with-output-to-string (stream)
          (format stream "Installed mutations: ~D~%Effective changes: ~D~2%"
                  (length records) (length effective))
          (dolist (record effective)
            (let ((properties (rest record)))
              (format stream "~A  ~A  ~A~%~A~2%"
                      (getf properties :id)
                      (getf properties :kind)
                      (getf properties :target)
                      (getf properties :proposed))))))
      (records
       (format nil "Installed mutations: ~D~%Effective changes: 0~%The pending state produces no effective change from the selected private image."
               (length records)))
      (t
       "The active image has no uncommitted reconstructible mutations."))))

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
           (effective (image-commit-effective-diff-records configuration))
           (identifier (make-identifier))
           (mutation-identifiers
             (mapcar (lambda (record) (getf (rest record) :id)) records)))
      (unless records
        (error 'image-commit-error
               :message "The active image has no reconstructible mutations to commit."
               :tool-name "self.commit"
               :pathname (configuration-image-commit-root configuration)
               :stage ':validation))
      (unless effective
        (error 'image-commit-error
               :message
               "The pending mutations produce no effective change; discard them instead of creating an empty private commit."
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
                     :history-commit
                     (image-commit-history-commit commit)
                     :result :committed))
              (tool-success
               (format nil
                       "Committed ~D live mutation~:P as private image commit ~A.~%Private Git commit: ~A~%Replay script: ~A"
                       (length records)
                       identifier
                       (image-commit-history-commit commit)
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
