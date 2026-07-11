(in-package #:frob)

;;;; -- Application Lifecycle --

(-> application-banner (application) list)
(defun application-banner (application)
  "Return APPLICATION's restrained styled banner and security notice."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (append
     (list (terminal-span :brand "█▀▀ █▀█ █▀█ █▄▄")
           (terminal-span :dim (format nil "  v~A~%" +frob-version+))
           (terminal-span :brand (format nil "█▀  █▀▄ █▄█ █▄█~%"))
           (terminal-span :plain (format nil "~%")))
     (application--field-spans "model"
                               (format nil "~A (effort ~A)"
                                       (configuration-model configuration)
                                       (configuration-reasoning-effort
                                        configuration)))
     (application--field-spans "conversation"
                               (conversation-identifier conversation))
     (application--field-spans "workspace"
                               (namestring
                                (configuration-working-directory
                                 configuration)))
     (list (terminal-span
            :notice
            (format nil "~%  Frob executes model-generated code with your ~
                         user privileges.~%  It is not a security sandbox."))))))

(-> application-handle-expected-error (application frob-error) null)
(defun application-handle-expected-error (application condition)
  "Present expected CONDITION without abandoning APPLICATION's active path."
  (terminal-ui-set-status (application-ui application) nil)
  (application-render-records application)
  (application-present
   application
   (application--transcript-entry
    application
    :style ':failure
    :header "✗ error"
    :body (if (typep condition 'credentials-unavailable)
              (format nil "~A~%Use /auth to authenticate Frob directly."
                      condition)
              (format nil "~A" condition))))
  nil)

(-> application-update-size (application) null)
(defun application-update-size (application)
  "Apply a pending terminal resize to APPLICATION's unfinished rows."
  (when *terminal-resize-pending-p*
    (setf *terminal-resize-pending-p* nil)
    (terminal-ui-resize (application-ui application)
                        (terminal-current-columns)))
  nil)

(-> application-run (application) null)
(defun application-run (application)
  "Run APPLICATION until explicit exit, always restoring terminal and worker state."
  (let ((ui (application-ui application))
        (worker (application-worker application)))
    (sb-sys:enable-interrupt
     sb-unix:sigwinch
     (lambda (signal code context)
       (declare (ignore signal code context))
       (setf *terminal-resize-pending-p* t)))
    (unwind-protect
         (with-terminal-ui (active-ui ui)
           (declare (ignore active-ui))
           (application-present application (application-banner application))
           (application-render-records application)
           (loop
             (application-update-size application)
             (let ((event (terminal-ui-read-event ui)))
               (multiple-value-bind (action payload)
                   (terminal-ui-process-event ui event)
                 (case action
                   (:submit
                    (let ((signal-backtrace nil))
                      (handler-bind
                          ((serious-condition
                             (lambda (condition)
                               (declare (ignore condition))
                               (setf signal-backtrace
                                     (application-safe-backtrace)))))
                        (handler-case
                            (when (eq (application-handle-input application payload)
                                      :quit)
                              (return))
                          ((or agent-loop-error
                               conversation-invariant-error
                               response-stream-error
                               active-image-corruption)
                           (condition)
                            (application-raise-fatal application
                                                     condition
                                                     signal-backtrace))
                          (frob-error (condition)
                            (application-handle-expected-error application condition))
                          (serious-condition (condition)
                            (application-raise-fatal application
                                                     condition
                                                     signal-backtrace))))))
                   ((:end-of-input :interrupt)
                    (return)))))))
      (sb-sys:enable-interrupt sb-unix:sigwinch :default)
      (when worker
        (lisp-worker-stop worker))))
  nil)


;;;; -- Command-Line Entry --

(-> main-usage () string)
(defun main-usage ()
  "Return the command-line usage text."
  "Usage: frob [--resume ID]
       frob --auth
       frob --version
       frob --recovery [--generation ID | --list]")

(-> main-authenticate (configuration) null)
(defun main-authenticate (configuration)
  "Run Frob-owned device authentication without starting the conversation UI."
  (configuration-ensure-directories configuration)
  (device-authentication-login
   (device-authentication-client-create)
   (credential-manager-create configuration)
   :stream *standard-output*
   :open-browser-p t)
  (format t "~&ChatGPT authentication was saved by Frob.~%")
  nil)

(-> main-dispatch (list) null)
(defun main-dispatch (arguments)
  "Dispatch validated Frob ARGUMENTS inside the active process."
  (cond
    ((member "--worker" arguments :test #'string=)
     (worker-main))
    ((member "--version" arguments :test #'string=)
     (format t "frob ~A~%" +frob-version+))
    ((or (member "--help" arguments :test #'string=)
         (member "-h" arguments :test #'string=))
     (format t "~A~%" (main-usage)))
    (t
     (let* ((configuration (configuration-create))
            (resume-position (position "--resume" arguments :test #'string=))
            (resume-id (and resume-position
                            (nth (1+ resume-position) arguments))))
       (when (and resume-position (not (non-empty-string-p resume-id)))
         (error 'configuration-error :message "--resume requires an identifier."))
       (cond
         ((member "--auth" arguments :test #'string=)
          (main-authenticate configuration))
         (t
          (setf *active-application*
                (if (typep *active-application* 'application)
                    (application-reconnect *active-application*
                                           :conversation-id resume-id)
                    (application-create configuration :conversation-id resume-id)))
          (when (and (member "--simulate-crash" arguments :test #'string=)
                     (not (non-empty-string-p (uiop:getenv "FROB_RECOVERED"))))
            (let ((capsule
                    (application-write-crash-capsule
                     *active-application*
                     (make-condition 'simple-error
                                     :format-control "Intentional recovery test."
                                     :format-arguments nil))))
              (format *error-output* "Intentional crash capsule: ~A~%" capsule)
              (uiop:quit 70)))
          (handler-case
              (application-run *active-application*)
            (fatal-control-path-error (condition)
              (format *error-output*
                      "Frob entered recovery after a fatal error. Capsule: ~A~%"
                      (fatal-control-path-error-capsule-pathname condition))
              (uiop:quit 70))))))))
  nil)

(-> main (list) null)
(defun main (arguments)
  "Run the Frob command described by ARGUMENTS with stable exit classification."
  (handler-case
      (main-dispatch arguments)
    (frob-error (condition)
      (format *error-output* "Frob could not start: ~A~%" condition)
      (uiop:quit 64)))
  nil)
