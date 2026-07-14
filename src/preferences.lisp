(in-package #:autolith)

;;;; -- Global Preferences --

(define-constant +preferences-version+ 1
  :documentation "The readable global preferences file format version.")

(-> preferences--form-p (t) boolean)
(defun preferences--form-p (form)
  "Return true when FORM is one complete supported preferences record."
  (handler-case
      (and (consp form)
           (eq (first form) :preferences)
           (let ((properties (rest form)))
             (and (evenp (length properties))
                  (= (getf properties :version -1) +preferences-version+)
                  (loop for tail on properties by #'cddr
                        thereis (eq (first tail) :reasoning-traces-p))
                  (typep (getf properties :reasoning-traces-p) 'boolean))))
    (error ()
      nil)))

(-> preferences--read-reasoning-traces (configuration) boolean)
(defun preferences--read-reasoning-traces (configuration)
  "Read the persisted reasoning-summary preference for CONFIGURATION."
  (block nil
    (let ((pathname (configuration-preferences-path configuration)))
      (unless (probe-file pathname)
        (return nil))
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
                           (preferences--form-p form))
                (error 'preferences-error
                       :message (format nil
                                        "Preferences at ~A are malformed or unsupported."
                                        pathname)
                       :pathname pathname
                       :operation ':read
                       :cause nil))
              (getf (rest form) :reasoning-traces-p)))
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

(-> preferences-reasoning-traces-p (configuration) boolean)
(defun preferences-reasoning-traces-p (configuration)
  "Return the persisted reasoning-summary setting, defaulting safely to false."
  (handler-case
      (preferences--read-reasoning-traces configuration)
    (preferences-error (condition)
      (warn 'preferences-load-warning
            :pathname (preferences-error-pathname condition)
            :cause condition)
      nil)))

(-> preferences-set-reasoning-traces (configuration boolean) null)
(defun preferences-set-reasoning-traces (configuration enabled-p)
  "Atomically persist ENABLED-P as CONFIGURATION's reasoning-summary setting."
  (let* ((pathname (configuration-preferences-path configuration))
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
                   (prin1 (list :preferences
                                :version +preferences-version+
                                :reasoning-traces-p enabled-p)
                          stream)
                   (terpri stream)
                   (finish-output stream)))
               (sb-posix:chmod (namestring temporary) #o600)
               (uiop:rename-file-overwriting-target temporary pathname)
               (sb-posix:chmod (namestring pathname) #o600))
          (when (probe-file temporary)
            (delete-file temporary)))
      (error (cause)
        (error 'preferences-error
               :message (format nil "Could not persist preferences at ~A: ~A"
                                pathname
                                cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)
