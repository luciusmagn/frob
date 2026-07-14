(in-package #:autolith)

;;;; -- Application Lifecycle --

(define-constant +application-banner-logo-lines+
  '((:brand-gradient-1 . "  :::.      :::")
    (:brand-gradient-2 . "  ;;`;;     ;;;")
    (:brand-gradient-3 . " ,[[ '[[,   [[[")
    (:brand-gradient-4 . "c$$$cc$$$c  $$'")
    (:brand-gradient-5 . " 888   888,o88oo,.__")
    (:brand-gradient-6 . " YMM   \"\"` \"\"\"\"YUMMM"))
  :test #'equal
  :documentation
  "The AL mark generated with FIGlet's Cosmic font, paired with row styles.")

(define-constant +application-banner-gap+ "   "
  :test #'string=
  :documentation "Horizontal space between the startup mark and session data.")

(define-constant +application-banner-minimum-metadata-width+ 32
  :documentation "The minimum useful width for metadata beside the startup mark.")

(-> application--banner-logo-width () (integer 1))
(defun application--banner-logo-width ()
  "Return the widest row of the embedded startup mark in terminal cells."
  (loop for entry in +application-banner-logo-lines+
        maximize (text-cell-width (rest entry))))

(-> application--banner-columns (application) (integer 1))
(defun application--banner-columns (application)
  "Return APPLICATION's current terminal width or the restrained default."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (if ui
        (terminal-columns (terminal-ui-terminal ui))
        +terminal-default-columns+)))

(-> application--banner-metadata-field (string string) list)
(defun application--banner-metadata-field (label value)
  "Return one aligned metadata LABEL and VALUE row without a newline."
  (list (terminal-span :dim (format nil "~12A  " label))
        (terminal-span :plain value)))

(-> application--banner-metadata-rows (application (integer 1)) list)
(defun application--banner-metadata-rows (application maximum-width)
  "Return identity and runtime rows no wider than MAXIMUM-WIDTH."
  (let* ((configuration (application-configuration application))
         (title
           (list (terminal-span :strong "AUTOLITH")
                 (terminal-span :dim (format nil " v~A" +autolith-version+))))
         (model
           (application--banner-metadata-field
            "model"
            (format nil "~A (effort ~A)"
                    (configuration-model configuration)
                    (configuration-reasoning-effort configuration))))
         (workspace
           (application--banner-metadata-field
            "workspace"
            (namestring (configuration-working-directory configuration))))
         (detail-rows (list title model workspace))
         (divider-width
           (min maximum-width
                (loop for row in detail-rows
                      maximize (terminal--spans-width row)))))
    (list title
          (list (terminal-span
                 :dim
                 (make-string divider-width :initial-element #\─)))
          model
          workspace)))

(-> application--banner-terminate-row (list) list)
(defun application--banner-terminate-row (spans)
  "Return SPANS followed by one plain newline span."
  (append spans (list (terminal-span :plain (string #\Newline)))))

(-> application--banner-side-by-side-spans (list integer) list)
(defun application--banner-side-by-side-spans (metadata-rows columns)
  "Return the startup mark with METADATA-ROWS aligned beside it within COLUMNS."
  (let* ((logo-width (application--banner-logo-width))
         (metadata-width (- columns
                            logo-width
                            (text-cell-width +application-banner-gap+))))
    (loop for logo-entry in +application-banner-logo-lines+
          for index from 0
          for metadata-row = (nth index metadata-rows)
          append
          (let ((logo-text (rest logo-entry)))
            (application--banner-terminate-row
             (append
              (list (terminal-span
                     (first logo-entry)
                     (if metadata-row
                         (format nil "~VA" logo-width logo-text)
                         logo-text)))
              (when metadata-row
                (append
                 (list (terminal-span :plain +application-banner-gap+))
                 (terminal--clip-spans metadata-row metadata-width)))))))))

(-> application--banner-stacked-spans (list integer) list)
(defun application--banner-stacked-spans (metadata-rows columns)
  "Return the startup mark above clipped METADATA-ROWS within COLUMNS."
  (append
   (loop for logo-entry in +application-banner-logo-lines+
         append (application--banner-terminate-row
                 (list (terminal-span (first logo-entry)
                                      (rest logo-entry)))))
   (list (terminal-span :plain (string #\Newline)))
   (loop for metadata-row in metadata-rows
         append (application--banner-terminate-row
                 (terminal--clip-spans metadata-row columns)))))

(-> application-banner (application) list)
(defun application-banner (application)
  "Return APPLICATION's styled identity, session metadata, and security notice."
  (let* ((columns (application--banner-columns application))
         (metadata-width
           (- columns
              (application--banner-logo-width)
              (text-cell-width +application-banner-gap+)))
         (side-by-side-minimum
           (+ (application--banner-logo-width)
              (text-cell-width +application-banner-gap+)
              +application-banner-minimum-metadata-width+))
         (header
           (if (>= columns side-by-side-minimum)
               (application--banner-side-by-side-spans
                (application--banner-metadata-rows application metadata-width)
                columns)
               (application--banner-stacked-spans
                (application--banner-metadata-rows application columns)
                columns))))
    (append
     (list (terminal-span :plain (string #\Newline)))
     header
     (list
      (terminal-span
       :notice
       (format nil "~%Autolith executes model-generated code with your user ~
                    privileges.~%It is not a security sandbox."))))))

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
  (terminal-ui-refresh-size ui #'application-pending-terminal-size)
  (prog1 (terminal-ui-read-event ui)
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)))

(-> application--resume-command (application) string)
(defun application--resume-command (application)
  "Return the shell command that resumes APPLICATION's exact conversation."
  (format nil "autolith --resume ~A"
          (uiop:escape-shell-token
           (conversation-identifier
            (application-conversation application)))))

(-> application--present-resume-instruction (application) boolean)
(defun application--present-resume-instruction (application)
  "Present APPLICATION's exact resume command when its conversation is durable."
  (let ((conversation (application-conversation application)))
    (if (conversation-persisted-p conversation)
        (not
         (null
          (application-present
           application
           (list
            (terminal-span :dim "To resume this conversation, run:")
            (terminal-span :plain (string #\Newline))
            (terminal-span :code
                           (format nil "  ~A"
                                   (application--resume-command application)))))))
        nil)))

(-> application-run (application) null)
(defun application-run (application)
  "Run APPLICATION with responsive input, always restoring terminal and workers."
  (let ((ui (application-ui application))
        (worker (application-worker application))
        (input-controller nil))
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
               :header "✗ mutation replay skipped"
               :body (format nil "~A~%~A"
                             (namestring (first failure))
                             (rest failure)))))
           (application-render-records application)
           (setf input-controller
                 (application-input-controller-create application))
           ;; Entering the interactive debugger would hang the raw terminal,
           ;; so any debugger entry becomes the fatal recovery path instead.
           (let ((*checkpoint-thread-quiescer*
                   (lambda (function)
                     (application-input-controller-call-with-reader-paused
                      input-controller function)))
                 (*debugger-hook*
                   (lambda (condition hook)
                     (declare (ignore hook))
                     (application-raise-fatal application
                                              condition
                                              (application-safe-backtrace)))))
             (handler-case
                 (loop
                   for work =
                     (application-input-controller--next-work input-controller)
                   while work
                   do (unwind-protect
                          (application-input-controller--run-work
                           input-controller work)
                        (application-input-controller--finish-work
                         input-controller)))
               (application-turn-cancelled ()
                 nil)
               (application-input-failed (condition)
                 (application-raise-fatal
                  application
                  (application-input-failed-original-condition condition)
                  (application-input-failed-backtrace condition)))))
           (when (eq (application-input-controller-exit-reason input-controller)
                     ':interrupt)
             (application--present-resume-instruction application)))
      (sb-sys:enable-interrupt sb-unix:sigwinch :default)
      (when input-controller
        (application-input-controller-stop input-controller))
      (when worker
        (lisp-worker-manager-stop worker))))
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
