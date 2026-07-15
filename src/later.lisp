(in-package #:autolith)

;;;; -- Persistent Deferred Inputs --

(define-constant +later-version+ 1
  :documentation "The readable deferred-input state format version.")

(defclass later-entry ()
  ((identifier
    :initarg :identifier
    :reader later-entry-identifier
    :type non-empty-string
    :documentation "The stable identifier of this deferred input.")
   (input
    :initarg :input
    :reader later-entry-input
    :type non-empty-string
    :documentation "The ordinary user input dispatched when this entry is due.")
   (directory
    :initarg :directory
    :reader later-entry-directory
    :type non-empty-string
    :documentation "The canonical workspace in which this input was scheduled.")
   (due-at
    :initarg :due-at
    :reader later-entry-due-at
    :type timestamp
    :documentation "The universal time at which this input becomes runnable.")
   (created-at
    :initarg :created-at
    :reader later-entry-created-at
    :type timestamp
    :documentation "The universal time at which this input was scheduled.")
   (window
    :initarg :window
    :reader later-entry-window
    :type non-empty-string
    :documentation "The rate-limit window or estimate governing DUE-AT."))
  (:documentation "One durable input waiting for a rate-limit reset."))

(defclass later-state ()
  ((entries
    :initarg :entries
    :initform nil
    :accessor later-state-entries
    :type list
    :documentation "Deferred entries ordered by due time and creation time."))
  (:documentation "Validated deferred inputs restored across Autolith processes."))

(-> later--property-present-p (list keyword) boolean)
(defun later--property-present-p (properties property)
  "Return true when PROPERTIES contains PROPERTY as a plist key."
  (not (null (loop for tail on properties by #'cddr
                   thereis (eq (first tail) property)))))

(-> later--entry-form-p (t) boolean)
(defun later--entry-form-p (form)
  "Return true when FORM is one complete portable deferred entry."
  (handler-case
      (and (consp form)
           (eq (first form) ':entry)
           (let ((properties (rest form)))
             (and (= (length properties) 12)
                  (every (lambda (property)
                           (later--property-present-p properties property))
                         '(:id :input :directory :due-at :created-at :window))
                  (non-empty-string-p (getf properties :id))
                  (non-empty-string-p (getf properties :input))
                  (non-empty-string-p (getf properties :directory))
                  (typep (getf properties :due-at) 'timestamp)
                  (typep (getf properties :created-at) 'timestamp)
                  (non-empty-string-p (getf properties :window)))))
    (error ()
      nil)))

(-> later--form-p (t) boolean)
(defun later--form-p (form)
  "Return true when FORM is one supported deferred-input state."
  (handler-case
      (and (listp form)
           (= (length form) 5)
           (eq (first form) ':later)
           (eq (second form) ':version)
           (= (third form) +later-version+)
           (eq (fourth form) ':entries)
           (listp (fifth form))
           (every #'later--entry-form-p (fifth form))
           (= (length (remove-duplicates
                       (mapcar (lambda (entry)
                                 (getf (rest entry) :id))
                               (fifth form))
                       :test #'string=))
              (length (fifth form))))
    (error ()
      nil)))

(-> later--entry-form->entry (list) later-entry)
(defun later--entry-form->entry (form)
  "Return the deferred entry represented by validated FORM."
  (let ((properties (rest form)))
    (make-instance 'later-entry
                   :identifier (copy-seq (getf properties :id))
                   :input (copy-seq (getf properties :input))
                   :directory (copy-seq (getf properties :directory))
                   :due-at (getf properties :due-at)
                   :created-at (getf properties :created-at)
                   :window (copy-seq (getf properties :window)))))

(-> later--entry< (later-entry later-entry) boolean)
(defun later--entry< (left right)
  "Return true when LEFT should run before RIGHT."
  (or (< (later-entry-due-at left) (later-entry-due-at right))
      (and (= (later-entry-due-at left) (later-entry-due-at right))
           (< (later-entry-created-at left) (later-entry-created-at right)))))

(-> later--sort-entries (list) list)
(defun later--sort-entries (entries)
  "Return a fresh due-time-ordered copy of ENTRIES."
  (stable-sort (copy-list entries) #'later--entry<))

(-> later--read (configuration) later-state)
(defun later--read (configuration)
  "Read CONFIGURATION's deferred inputs or return an empty state."
  (block nil
    (let ((pathname (configuration-later-path configuration)))
      (unless (probe-file pathname)
        (return (make-instance 'later-state)))
      (handler-case
          (with-open-file (stream pathname
                                  :direction :input
                                  :external-format :utf-8)
            (let* ((*read-eval* nil)
                   (end-marker (cons nil nil))
                   (form (read stream nil end-marker))
                   (extra (read stream nil end-marker)))
              (unless (and (not (eq form end-marker))
                           (eq extra end-marker)
                           (later--form-p form))
                (error 'later-error
                       :message (format nil
                                        "Deferred inputs at ~A are malformed or unsupported."
                                        pathname)
                       :pathname pathname
                       :operation ':read
                       :cause nil))
              (make-instance
               'later-state
               :entries
               (later--sort-entries
                (mapcar #'later--entry-form->entry (fifth form))))))
        (later-error (condition)
          (error condition))
        (error (cause)
          (error 'later-error
                 :message (format nil "Could not read deferred inputs at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> later-load (configuration) later-state)
(defun later-load (configuration)
  "Return deferred inputs, warning and using an empty queue after corruption."
  (handler-case
      (later--read configuration)
    (later-error (condition)
      (warn 'later-load-warning
            :pathname (later-error-pathname condition)
            :cause condition)
      (make-instance 'later-state))))

(-> later--entry->form (later-entry) list)
(defun later--entry->form (entry)
  "Return ENTRY as one portable readable form."
  (list :entry
        :id (later-entry-identifier entry)
        :input (later-entry-input entry)
        :directory (later-entry-directory entry)
        :due-at (later-entry-due-at entry)
        :created-at (later-entry-created-at entry)
        :window (later-entry-window entry)))

(-> later--state-form (later-state) list)
(defun later--state-form (state)
  "Return STATE as one portable readable form."
  (list :later
        :version +later-version+
        :entries (mapcar #'later--entry->form
                         (later-state-entries state))))

(-> later--write (configuration later-state) null)
(defun later--write (configuration state)
  "Atomically persist deferred input STATE with private file permissions."
  (let* ((pathname (configuration-later-path configuration))
         (temporary
           (make-pathname
            :name (format nil ".~A.~A"
                          (pathname-name pathname)
                          (make-identifier))
            :type "tmp"
            :defaults pathname)))
    (handler-case
        (unwind-protect
             (progn
               (ensure-directories-exist pathname)
               (with-open-file (stream temporary
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (let ((*print-circle* t)
                       (*print-readably* t)
                       (*print-pretty* t))
                   (prin1 (later--state-form state) stream)
                   (terpri stream)
                   (finish-output stream)))
               (sb-posix:chmod (namestring temporary) #o600)
               (uiop:rename-file-overwriting-target temporary pathname)
               (sb-posix:chmod (namestring pathname) #o600))
          (when (probe-file temporary)
            (delete-file temporary)))
      (error (cause)
        (error 'later-error
               :message (format nil "Could not persist deferred inputs at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> later-schedule
    (&key (:configuration configuration) (:state later-state)
          (:input string) (:directory pathname) (:due-at timestamp)
          (:window string) (:created-at timestamp))
    later-entry)
(defun later-schedule
    (&key configuration state input directory due-at window
          (created-at (get-universal-time)))
  "Persist INPUT in DIRECTORY for DUE-AT and return its new deferred entry."
  (unless (and (non-empty-string-p input)
               (non-empty-string-p window))
    (error 'later-error
           :message "A deferred input and rate-limit window are required."
           :pathname (configuration-later-path configuration)
           :operation ':validate
           :cause nil))
  (let ((existing-directory (uiop:directory-exists-p directory)))
    (unless existing-directory
      (error 'later-error
             :message (format nil "Deferred-input directory ~A does not exist."
                              directory)
             :pathname (configuration-later-path configuration)
             :operation ':validate
             :cause nil))
    (let* ((entry
             (make-instance 'later-entry
                            :identifier (make-identifier)
                            :input (copy-seq input)
                            :directory
                            (namestring
                             (uiop:ensure-directory-pathname
                              (truename existing-directory)))
                            :due-at due-at
                            :created-at created-at
                            :window (copy-seq window)))
           (entries
             (later--sort-entries
              (append (later-state-entries state) (list entry))))
           (replacement (make-instance 'later-state :entries entries)))
      (later--write configuration replacement)
      (setf (later-state-entries state) entries)
      entry)))

(-> later-cancel (configuration later-state string) boolean)
(defun later-cancel (configuration state identifier)
  "Remove IDENTIFIER from STATE durably and report whether it existed."
  (let* ((previous (later-state-entries state))
         (entries (remove identifier previous
                          :key #'later-entry-identifier
                          :test #'string=)))
    (if (= (length entries) (length previous))
        nil
        (progn
          (later--write configuration
                        (make-instance 'later-state :entries entries))
          (setf (later-state-entries state) entries)
          t))))
