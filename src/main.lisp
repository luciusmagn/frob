(in-package #:autolith)

;;;; -- Application Lifecycle --

(-> application-banner (application) list)
(defun application-banner (application)
  "Return APPLICATION's restrained styled banner and security notice."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (append
     (list (terminal-span
            :brand
            ;; Generated with FIGlet's banner font and embedded for fast startup.
            (format nil
                    "~{~A~%~}"
                    '("   #    #     # ####### ####### #       ### ####### #     #"
                      "  # #   #     #    #    #     # #        #     #    #     #"
                      " #   #  #     #    #    #     # #        #     #    #     #"
                      "#     # #     #    #    #     # #        #     #    #######"
                      "####### #     #    #    #     # #        #     #    #     #"
                      "#     # #     #    #    #     # #        #     #    #     #"
                      "#     #  #####     #    ####### ####### ###    #    #     #")))
           (terminal-span
            :dim
            (format nil "AUTOLITH  v~A~%~%"
                    +autolith-version+)))
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
            (format nil "~%  Autolith executes model-generated code with your ~
                         user privileges.~%  It is not a security sandbox."))))))

(-> application-handle-expected-error (application autolith-error) null)
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
              (format nil "~A~%Use /auth to authenticate Autolith directly."
                      condition)
              (format nil "~A" condition))))
  nil)

(-> application-read-terminal-event (terminal-ui) t)
(defun application-read-terminal-event (ui)
  "Read one UI event, applying pending resizes before and after the blocking read."
  (terminal-ui-refresh-size ui #'application-pending-terminal-columns)
  (prog1 (terminal-ui-read-event ui)
    (terminal-ui-refresh-size ui #'application-pending-terminal-columns)))

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
           (dolist (failure (application-overlay-failures application))
             (application-present
              application
              (application--transcript-entry
               application
               :style ':failure
               :header "✗ overlay skipped"
               :body (format nil "~A~%~A"
                             (namestring (first failure))
                             (rest failure)))))
           (application-render-records application)
           ;; Entering the interactive debugger would hang the raw terminal,
           ;; so any debugger entry becomes the fatal recovery path instead.
           (let ((*debugger-hook*
                   (lambda (condition hook)
                     (declare (ignore hook))
                     (application-raise-fatal application
                                              condition
                                              (application-safe-backtrace)))))
             (loop
               (let ((event (application-read-terminal-event ui)))
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
                            (rollback-requested (condition)
                              (error condition))
                            ((or agent-loop-error
                                 conversation-invariant-error
                                 response-stream-error
                                 active-image-corruption)
                             (condition)
                              (application-raise-fatal application
                                                       condition
                                                       signal-backtrace))
                            (autolith-error (condition)
                              (application-handle-expected-error application condition))
                            (serious-condition (condition)
                              (application-raise-fatal application
                                                       condition
                                                       signal-backtrace))))))
                     ((:end-of-input :interrupt)
                      (return))))))))
      (sb-sys:enable-interrupt sb-unix:sigwinch :default)
      (when worker
        (lisp-worker-stop worker))))
  nil)


;;;; -- Command-Line Entry --

(defconstant +main-fatal-recovery-status+ 70
  "The process status asking the stable launcher to recover after a fatal error.")

(defconstant +main-rollback-recovery-status+ 75
  "The process status asking the stable launcher to start a selected rollback.")

(-> main-usage () string)
(defun main-usage ()
  "Return the command-line usage text."
  "Usage: autolith [--resume ID]
       autolith --auth
       autolith --version
       autolith --recovery [--generation ID | --list]")

(-> main-authenticate (configuration) null)
(defun main-authenticate (configuration)
  "Run Autolith-owned device authentication without starting the conversation UI."
  (configuration-ensure-directories configuration)
  (device-authentication-login
   (device-authentication-client-create)
   (credential-manager-create configuration)
   :stream *standard-output*
   :open-browser-p t)
  (format t "~&ChatGPT authentication was saved by Autolith.~%")
  nil)

(-> main-dispatch (list) null)
(defun main-dispatch (arguments)
  "Dispatch validated Autolith ARGUMENTS inside the active process."
  (cond
    ((member "--worker" arguments :test #'string=)
     (worker-main))
    ((member "--version" arguments :test #'string=)
     (format t "autolith ~A~%" +autolith-version+))
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
                     (not (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))))
            (let ((capsule
                    (application-write-crash-capsule
                     *active-application*
                     (make-condition 'simple-error
                                     :format-control "Intentional recovery test."
                                     :format-arguments nil))))
              (format *error-output* "Intentional crash capsule: ~A~%" capsule)
              (uiop:quit +main-fatal-recovery-status+)))
          (handler-case
              (application-run *active-application*)
            (rollback-requested (condition)
              (format *error-output*
                      "Autolith is rolling back to retained generation ~A.~%"
                      (rollback-requested-generation-id condition))
              (uiop:quit +main-rollback-recovery-status+))
            (fatal-control-path-error (condition)
              (format *error-output*
                      "Autolith entered recovery after a fatal error. Capsule: ~A~%"
                      (fatal-control-path-error-capsule-pathname condition))
              (uiop:quit +main-fatal-recovery-status+))))))))
  nil)

(-> main (list) null)
(defun main (arguments)
  "Run the Autolith command described by ARGUMENTS with stable exit classification."
  (handler-case
      (main-dispatch arguments)
    (autolith-error (condition)
      (format *error-output* "Autolith could not start: ~A~%" condition)
      (uiop:quit 64)))
  nil)
