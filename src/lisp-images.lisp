(in-package #:autolith)

;;;; -- Saved Lisp Worker Images --

(define-constant +pristine-lisp-image-identifier+ "pristine"
  :test #'string=
  :documentation "The immutable virtual base used for fresh Lisp workers.")

(define-constant +lisp-image-manifest-version+ 1
  :documentation "The current saved Lisp worker-image manifest version.")

(define-constant +minimum-lisp-image-core-size+ 1048576
  :documentation "The smallest plausible saved SBCL worker core in bytes.")

(defclass lisp-image ()
  ((identifier
    :initarg :identifier
    :reader lisp-image-identifier
    :type non-empty-string
    :documentation "The immutable worker-image identifier.")
   (directory
    :initarg :directory
    :reader lisp-image-directory
    :type pathname
    :documentation "The private directory containing this image's artifacts.")
   (core-pathname
    :initarg :core-pathname
    :reader lisp-image-core-pathname
    :type pathname
    :documentation "The saved SBCL Lisp core pathname.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader lisp-image-manifest-pathname
    :type pathname
    :documentation "The portable immutable manifest pathname.")
   (parent-identifier
    :initarg :parent-identifier
    :reader lisp-image-parent-identifier
    :type non-empty-string
    :documentation "The pristine or saved image from which this image descended.")
   (note
    :initarg :note
    :reader lisp-image-note
    :type non-empty-string
    :documentation "The durable explanation of modifications and intended use.")
   (sbcl-version
    :initarg :sbcl-version
    :reader lisp-image-sbcl-version
    :type non-empty-string
    :documentation "The exact SBCL version that saved the Lisp heap.")
   (operating-system
    :initarg :operating-system
    :reader lisp-image-operating-system
    :type non-empty-string
    :documentation "The operating system that saved the Lisp heap.")
   (operating-system-version
    :initarg :operating-system-version
    :reader lisp-image-operating-system-version
    :type non-empty-string
    :documentation "The operating-system build that saved the Lisp heap.")
   (architecture
    :initarg :architecture
    :reader lisp-image-architecture
    :type non-empty-string
    :documentation "The machine architecture that saved the Lisp heap.")
   (source-commit
    :initarg :source-commit
    :reader lisp-image-source-commit
    :type (option string)
    :documentation "The Autolith source revision loaded when the image was saved.")
   (created-at
    :initarg :created-at
    :reader lisp-image-created-at
    :type timestamp
    :documentation "The image creation time as Common Lisp universal time."))
  (:documentation "An immutable named SBCL worker heap with durable provenance."))

(-> lisp-image-identifier-p (t) boolean)
(defun lisp-image-identifier-p (value)
  "Return true when VALUE is safe as one saved worker-image path component."
  (and (non-empty-string-p value)
       (<= (length value) 80)
       (not (string= value +pristine-lisp-image-identifier+))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(-> lisp-image--validate-identifier (string) string)
(defun lisp-image--validate-identifier (identifier)
  "Return valid saved image IDENTIFIER or signal LISP-IMAGE-ERROR."
  (unless (lisp-image-identifier-p identifier)
    (error 'lisp-image-error
           :message
           (format nil
                   "Invalid Lisp image name ~S. Use 1 to 80 letters, digits, hyphens, or underscores; pristine is reserved."
                   identifier)
           :tool-name "lisp.images"
           :pathname nil
           :stage ':name))
  identifier)

(-> lisp-image--directory (configuration string) pathname)
(defun lisp-image--directory (configuration identifier)
  "Return IDENTIFIER's private saved worker-image directory."
  (merge-pathnames
   (format nil "~A/" (lisp-image--validate-identifier identifier))
   (configuration-lisp-image-root configuration)))

(-> lisp-image--plausible-core-p (pathname) boolean)
(defun lisp-image--plausible-core-p (pathname)
  "Return true when PATHNAME names a plausibly sized regular SBCL core."
  (and (probe-file pathname)
       (handler-case
           (with-open-file (stream pathname
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
             (> (file-length stream) +minimum-lisp-image-core-size+))
         (error ()
           nil))
       t))

(-> lisp-image--manifest-form
    (string string string pathname (option string) integer)
    list)
(defun lisp-image--manifest-form
    (identifier parent-identifier note core-pathname source-commit created-at)
  "Return the complete portable manifest for one saved worker image."
  (list :lisp-image
        :version +lisp-image-manifest-version+
        :id identifier
        :parent parent-identifier
        :note note
        :core (namestring core-pathname)
        :sbcl-version (lisp-implementation-version)
        :operating-system (software-type)
        :operating-system-version (software-version)
        :architecture (machine-type)
        :source-commit source-commit
        :created-at created-at))

(-> lisp-image--write-manifest (pathname list) pathname)
(defun lisp-image--write-manifest (pathname form)
  "Atomically publish immutable worker-image manifest FORM at PATHNAME."
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
                                   :if-exists :error
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
        (delete-file temporary))))
  pathname)

(-> lisp-image-publish-manifest
    (configuration string string string pathname &key (:source-commit (option string)))
    lisp-image)
(defun lisp-image-publish-manifest
    (configuration identifier parent-identifier note core-pathname
     &key source-commit)
  "Validate CORE-PATHNAME and publish IDENTIFIER's immutable manifest."
  (setf identifier (lisp-image--validate-identifier identifier))
  (unless (and (or (string= parent-identifier
                            +pristine-lisp-image-identifier+)
                   (lisp-image-identifier-p parent-identifier))
               (non-empty-string-p note)
               (<= (length note) 4000))
    (error 'lisp-image-error
           :message "A Lisp image needs a valid parent and a note of at most 4000 characters."
           :tool-name "lisp.save-image"
           :pathname core-pathname
           :stage ':manifest))
  (let* ((directory (lisp-image--directory configuration identifier))
         (expected-core (merge-pathnames "worker.core" directory))
         (manifest (merge-pathnames "manifest.sexp" directory)))
    (unless (and (equal (pathname core-pathname) expected-core)
                 (lisp-image--plausible-core-p expected-core))
      (error 'lisp-image-error
             :message "The saved Lisp image core is absent, misplaced, or implausibly small."
             :tool-name "lisp.save-image"
             :pathname core-pathname
             :stage ':core))
    (when (probe-file manifest)
      (error 'lisp-image-error
             :message (format nil "Lisp image ~A already exists." identifier)
             :tool-name "lisp.save-image"
             :pathname manifest
             :stage ':publish))
    (sb-posix:chmod (namestring expected-core) #o444)
    (lisp-image--write-manifest
     manifest
     (lisp-image--manifest-form identifier
                                parent-identifier
                                note
                                expected-core
                                source-commit
                                (get-universal-time)))
    (lisp-image-load configuration identifier)))

(-> lisp-image--source-commit (configuration) (option string))
(defun lisp-image--source-commit (configuration)
  "Return CONFIGURATION's current tracked source revision, when available."
  (handler-case
      (let ((output
              (uiop:run-program
               (list "git"
                     "-C"
                     (namestring (configuration-source-root configuration))
                     "rev-parse"
                     "HEAD")
               :output :string
               :error-output :output)))
        (let ((commit
                (string-trim '(#\Space #\Tab #\Newline #\Return) output)))
          (and (non-empty-string-p commit) commit)))
    (error ()
      nil)))

(-> lisp-image-staging-directory (configuration string) pathname)
(defun lisp-image-staging-directory (configuration identifier)
  "Return a fresh unpublished directory for saved image IDENTIFIER."
  (lisp-image--validate-identifier identifier)
  (merge-pathnames
   (format nil ".~A.~A/" identifier (make-identifier))
   (configuration-lisp-image-root configuration)))

(-> lisp-image-publish-saved-core
    (configuration
     &key (:identifier string)
          (:parent-identifier string)
          (:note string)
          (:staging-directory pathname))
    lisp-image)
(defun lisp-image-publish-saved-core
    (configuration &key identifier parent-identifier note staging-directory)
  "Atomically publish a validated core from STAGING-DIRECTORY."
  (setf identifier (lisp-image--validate-identifier identifier))
  (let* ((root (configuration-lisp-image-root configuration))
         (directory (lisp-image--directory configuration identifier))
         (staging-directory (uiop:ensure-directory-pathname staging-directory))
         (staging-core (merge-pathnames "worker.core" staging-directory))
         (staging-manifest (merge-pathnames "manifest.sexp" staging-directory))
         (published-core (merge-pathnames "worker.core" directory)))
    (unless (and (uiop:subpathp staging-directory root)
                 (not (equal staging-directory directory))
                 (lisp-image--plausible-core-p staging-core))
      (error 'lisp-image-error
             :message "The unpublished Lisp core is absent or outside its staging root."
             :tool-name "lisp.save-image"
             :pathname staging-core
             :stage ':core))
    (when (probe-file directory)
      (error 'lisp-image-error
             :message (format nil "Lisp image ~A already exists." identifier)
             :tool-name "lisp.save-image"
             :pathname directory
             :stage ':publish))
    (unless (and (or (string= parent-identifier
                              +pristine-lisp-image-identifier+)
                     (lisp-image-identifier-p parent-identifier))
                 (non-empty-string-p note)
                 (<= (length note) 4000))
      (error 'lisp-image-error
             :message "A Lisp image needs a valid parent and a note of at most 4000 characters."
             :tool-name "lisp.save-image"
             :pathname staging-manifest
             :stage ':manifest))
    (sb-posix:chmod (namestring staging-core) #o444)
    (lisp-image--write-manifest
     staging-manifest
     (lisp-image--manifest-form identifier
                                parent-identifier
                                note
                                published-core
                                (lisp-image--source-commit configuration)
                                (get-universal-time)))
    (handler-case
        (rename-file staging-directory directory)
      (error (condition)
        (error 'lisp-image-error
               :message (format nil "Could not publish Lisp image ~A: ~A"
                                identifier
                                condition)
               :tool-name "lisp.save-image"
               :pathname directory
               :stage ':publish)))
    (lisp-image-load configuration identifier)))

(-> lisp-image-load (configuration string) lisp-image)
(defun lisp-image-load (configuration identifier)
  "Load and validate saved Lisp worker image IDENTIFIER."
  (let* ((directory (lisp-image--directory configuration identifier))
         (manifest (merge-pathnames "manifest.sexp" directory)))
    (unless (probe-file manifest)
      (error 'lisp-image-error
             :message (format nil "Lisp image ~A has no manifest." identifier)
             :tool-name "lisp.images"
             :pathname manifest
             :stage ':manifest))
    (let* ((form (read-portable-form manifest))
           (properties (and (listp form) (rest form)))
           (core-value (and properties (getf properties :core)))
           (core (and (non-empty-string-p core-value) (pathname core-value))))
      (unless (and (listp form)
                   (eq (first form) :lisp-image)
                   (= (or (getf properties :version) 0)
                      +lisp-image-manifest-version+)
                   (string= (or (getf properties :id) "") identifier)
                   (let ((parent (getf properties :parent)))
                     (and (non-empty-string-p parent)
                          (or (string= parent +pristine-lisp-image-identifier+)
                              (lisp-image-identifier-p parent))))
                   (non-empty-string-p (getf properties :note))
                   (<= (length (getf properties :note)) 4000)
                   core
                   (uiop:subpathp core directory)
                   (lisp-image--plausible-core-p core)
                   (non-empty-string-p (getf properties :sbcl-version))
                   (non-empty-string-p (getf properties :operating-system))
                   (non-empty-string-p
                    (getf properties :operating-system-version))
                   (non-empty-string-p (getf properties :architecture))
                   (or (null (getf properties :source-commit))
                       (non-empty-string-p (getf properties :source-commit)))
                   (typep (getf properties :created-at) 'timestamp))
        (error 'lisp-image-error
               :message (format nil "Invalid Lisp image manifest at ~A." manifest)
               :tool-name "lisp.images"
               :pathname manifest
               :stage ':manifest))
      (make-instance 'lisp-image
                     :identifier identifier
                     :directory directory
                     :core-pathname core
                     :manifest-pathname manifest
                     :parent-identifier (getf properties :parent)
                     :note (getf properties :note)
                     :sbcl-version (getf properties :sbcl-version)
                     :operating-system (getf properties :operating-system)
                     :operating-system-version
                     (getf properties :operating-system-version)
                     :architecture (getf properties :architecture)
                     :source-commit (getf properties :source-commit)
                     :created-at (getf properties :created-at)))))

(-> lisp-image-compatible-p (lisp-image) boolean)
(defun lisp-image-compatible-p (image)
  "Return true when IMAGE can boot under this exact SBCL host."
  (and (string= (lisp-image-sbcl-version image)
                (lisp-implementation-version))
       (string= (lisp-image-operating-system image) (software-type))
       (string= (lisp-image-operating-system-version image)
                (software-version))
       (string= (lisp-image-architecture image) (machine-type))
       t))

(-> lisp-image-scan (configuration) (values list list))
(defun lisp-image-scan (configuration)
  "Return valid saved images and (PATHNAME . REPORT) failures."
  (let ((root (configuration-lisp-image-root configuration))
        (images nil)
        (failures nil))
    (when (uiop:directory-exists-p root)
      (dolist (directory (sort (uiop:subdirectories root)
                               #'string<
                               :key #'namestring))
        (let ((identifier
                (first (last (pathname-directory
                              (uiop:ensure-directory-pathname directory))))))
          (handler-case
              (push (lisp-image-load configuration (string identifier)) images)
            (error (condition)
              (push (cons directory (princ-to-string condition)) failures))))))
    (values (sort images #'string< :key #'lisp-image-identifier)
            (nreverse failures))))

(-> lisp-image-render-inventory (configuration) string)
(defun lisp-image-render-inventory (configuration)
  "Return a concise model-visible inventory of pristine and saved images."
  (multiple-value-bind (images failures)
      (lisp-image-scan configuration)
    (with-output-to-string (stream)
      (format stream "~A  compatible  immutable base~%"
              +pristine-lisp-image-identifier+)
      (dolist (image images)
        (format stream "~A  ~A  parent ~A~%  note: ~A~%"
                (lisp-image-identifier image)
                (if (lisp-image-compatible-p image)
                    "compatible"
                    "incompatible")
                (lisp-image-parent-identifier image)
                (lisp-image-note image)))
      (dolist (failure failures)
        (format stream "invalid  ~A~%  error: ~A~%"
                (namestring (first failure))
                (rest failure))))))

(-> lisp-image-prompt-notes (configuration) string)
(defun lisp-image-prompt-notes (configuration)
  "Return bounded saved-image notes for model context on every request."
  (bounded-string (lisp-image-render-inventory configuration) :limit 12000))
