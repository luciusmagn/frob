(in-package #:autolith)

;;;; -- Global Preferences --

(define-constant +preferences-version+ 3
  :documentation "The readable global preferences file format version.")

(defclass preference-state ()
  ((model
    :initarg :model
    :initform nil
    :reader preference-state-model
    :type (option non-empty-string)
    :documentation "The last interactively selected provider model, if any.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :reader preference-state-reasoning-effort
    :type (option non-empty-string)
    :documentation "The last interactively selected reasoning effort, if any.")
   (reasoning-traces-p
    :initarg :reasoning-traces-p
    :initform nil
    :reader preference-state-reasoning-traces-p
    :type boolean
    :documentation "Whether provider reasoning summaries are requested and shown.")
   (compact-view-p
    :initarg :compact-view-p
    :initform t
    :reader preference-state-compact-view-p
    :type boolean
    :documentation "Whether successful routine tool results are hidden."))
  (:documentation "Validated global choices restored across Autolith processes."))

(-> preferences--form-p (t) boolean)
(defun preferences--form-p (form)
  "Return true when FORM is one complete supported preferences record."
  (handler-case
      (and (consp form)
           (eq (first form) :preferences)
           (let* ((properties (rest form))
                  (version (getf properties :version -1)))
             (and (evenp (length properties))
                  (readable-state-property-present-p
                   properties :reasoning-traces-p)
                  (typep (getf properties :reasoning-traces-p) 'boolean)
                  (case version
                    (1
                     t)
                    ((2 3)
                     (let ((model (getf properties :model))
                           (effort (getf properties :reasoning-effort)))
                       (and
                        (readable-state-property-present-p properties :model)
                        (readable-state-property-present-p
                         properties :reasoning-effort)
                        (or (null model) (non-empty-string-p model))
                        (or (null effort)
                            (not
                             (null
                              (member effort
                                      +supported-reasoning-efforts+
                                      :test #'string=))))
                        (or (= version 2)
                            (and
                             (readable-state-property-present-p
                              properties :compact-view-p)
                             (typep (getf properties :compact-view-p)
                                    'boolean))))))
                    (otherwise
                     nil)))))
    (error ()
      nil)))

(-> preferences--form->state (list) preference-state)
(defun preferences--form->state (form)
  "Return the validated preference state represented by FORM."
  (let ((properties (rest form)))
    (make-instance 'preference-state
                   :model (getf properties :model)
                   :reasoning-effort (getf properties :reasoning-effort)
                   :reasoning-traces-p
                   (getf properties :reasoning-traces-p)
                   :compact-view-p (getf properties :compact-view-p t))))

(-> preferences--read (configuration) preference-state)
(defun preferences--read (configuration)
  "Read CONFIGURATION's preference state or return empty defaults."
  (block nil
    (let ((pathname (configuration-preferences-path configuration)))
      (unless (probe-file pathname)
        (return (make-instance 'preference-state)))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless (and sole-form-p (preferences--form-p form))
              (error 'preferences-error
                     :message (format nil
                                      "Preferences at ~A are malformed or unsupported."
                                      pathname)
                     :pathname pathname
                     :operation ':read
                     :cause nil))
            (preferences--form->state form))
        (preferences-error (condition)
          (error condition))
        (error (cause)
          (error 'preferences-error
                 :message (format nil "Could not read preferences at ~A: ~A"
                                  pathname
                                  cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> preferences-load (configuration) preference-state)
(defun preferences-load (configuration)
  "Return validated preferences, warning and using defaults after corruption."
  (handler-case
      (preferences--read configuration)
    (preferences-error (condition)
      (warn 'preferences-load-warning
            :pathname (preferences-error-pathname condition)
            :cause condition)
      (make-instance 'preference-state))))

(-> preferences-reasoning-traces-p (configuration) boolean)
(defun preferences-reasoning-traces-p (configuration)
  "Return the persisted reasoning-summary setting, defaulting safely to false."
  (preference-state-reasoning-traces-p (preferences-load configuration)))

(-> preferences-compact-view-p (configuration) boolean)
(defun preferences-compact-view-p (configuration)
  "Return the persisted compact-view setting, defaulting safely to true."
  (preference-state-compact-view-p (preferences-load configuration)))

(-> preferences-apply-model-selection (configuration) configuration)
(defun preferences-apply-model-selection (configuration)
  "Apply saved model choices unless their corresponding environment variables exist."
  (let* ((preferences (preferences-load configuration))
         (saved-model (preference-state-model preferences))
         (saved-effort (preference-state-reasoning-effort preferences))
         (selected configuration))
    (when (and saved-model
               (not (non-empty-string-p (uiop:getenv "AUTOLITH_MODEL"))))
      (setf selected (configuration--clone selected :model saved-model)))
    (when (and saved-effort
               (not (non-empty-string-p
                     (uiop:getenv "AUTOLITH_REASONING_EFFORT"))))
      (setf selected
            (configuration-with-reasoning-effort selected saved-effort)))
    selected))

(-> preferences--write (configuration preference-state) null)
(defun preferences--write (configuration preferences)
  "Atomically write PREFERENCES under CONFIGURATION's private config root."
  (let ((pathname (configuration-preferences-path configuration)))
    (handler-case
        (snapshot-write
         pathname
         (list :preferences
               :version +preferences-version+
               :model (preference-state-model preferences)
               :reasoning-effort
               (preference-state-reasoning-effort preferences)
               :reasoning-traces-p
               (preference-state-reasoning-traces-p preferences)
               :compact-view-p
               (preference-state-compact-view-p preferences)))
      (error (cause)
        (error 'preferences-error
               :message (format nil "Could not persist preferences at ~A: ~A"
                                pathname
                                cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> preferences-set-model-selection (configuration) null)
(defun preferences-set-model-selection (configuration)
  "Persist CONFIGURATION's model and reasoning effort as global defaults."
  (let ((previous (preferences-load configuration)))
    (preferences--write
     configuration
     (make-instance
      'preference-state
      :model (configuration-model configuration)
      :reasoning-effort (configuration-reasoning-effort configuration)
      :reasoning-traces-p
      (preference-state-reasoning-traces-p previous)
      :compact-view-p
      (preference-state-compact-view-p previous))))
  nil)

(-> preferences-set-reasoning-traces (configuration boolean) null)
(defun preferences-set-reasoning-traces (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding saved model choices."
  (let ((previous (preferences-load configuration)))
    (preferences--write
     configuration
     (make-instance
      'preference-state
      :model (preference-state-model previous)
      :reasoning-effort (preference-state-reasoning-effort previous)
      :reasoning-traces-p enabled-p
      :compact-view-p (preference-state-compact-view-p previous))))
  nil)

(-> preferences-set-compact-view (configuration boolean) null)
(defun preferences-set-compact-view (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding other global choices."
  (let ((previous (preferences-load configuration)))
    (preferences--write
     configuration
     (make-instance
      'preference-state
      :model (preference-state-model previous)
      :reasoning-effort (preference-state-reasoning-effort previous)
      :reasoning-traces-p
      (preference-state-reasoning-traces-p previous)
      :compact-view-p enabled-p)))
  nil)
