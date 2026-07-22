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

(define-constant +application-recovery-gradient-styles+
  '(:recovery-gradient-1 :recovery-gradient-2 :recovery-gradient-3
    :recovery-gradient-4 :recovery-gradient-5 :recovery-gradient-6)
  :test #'equal
  :documentation "The distinct row styles used after recovery starts Autolith.")

(define-constant +application-banner-gap+ "   "
  :test #'string=
  :documentation "Horizontal space between the startup mark and session data.")

(define-constant +application-banner-minimum-metadata-width+ 32
  :documentation "The minimum useful width for metadata beside the startup mark.")

(-> application--banner-logo-lines () list)
(defun application--banner-logo-lines ()
  "Return startup-mark rows colored for an ordinary or recovered process."
  (if (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))
      (mapcar (lambda (entry style)
                (cons style (rest entry)))
              +application-banner-logo-lines+
              +application-recovery-gradient-styles+)
      +application-banner-logo-lines+))

(-> application--banner-logo-width () (integer 1))
(defun application--banner-logo-width ()
  "Return the widest row of the embedded startup mark in terminal cells."
  (loop for entry in (application--banner-logo-lines)
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
         (mode
           (and (configuration-immutable-p configuration)
                (application--banner-metadata-field "mode" "immutable")))
         (detail-rows (append (list title model workspace)
                              (when mode (list mode))))
         (divider-width
           (min maximum-width
                (loop for row in detail-rows
                      maximize (terminal--spans-width row)))))
    (append
     (list title
           (list (terminal-span
                  :dim
                  (make-string divider-width :initial-element #\─)))
           model
           workspace)
     (when mode (list mode)))))

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
    (loop for logo-entry in (application--banner-logo-lines)
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
   (loop for logo-entry in (application--banner-logo-lines)
         append (application--banner-terminate-row
                 (list (terminal-span (first logo-entry)
                                      (rest logo-entry)))))
   (list (terminal-span :plain (string #\Newline)))
   (loop for metadata-row in metadata-rows
         append (application--banner-terminate-row
                 (terminal--clip-spans metadata-row columns)))))

(-> application--startup-command-entry () list)
(defun application--startup-command-entry ()
  "Return one command entry selected for the startup banner."
  (nth (random (length +application-commands+)
               (make-random-state t))
       +application-commands+))

(-> application--command-tip-spans (list) terminal-styled-text)
(defun application--command-tip-spans (entry)
  "Return a startup tip with ENTRY's command token styled as code."
  (list (terminal-span :plain (format nil "~2%"))
        (terminal-span :dim "Tip: ")
        (terminal-span :code (getf entry :name))
        (terminal-span :plain (format nil " ~A" (getf entry :tip)))))

(-> application-banner (application) list)
(defun application-banner (application)
  "Return APPLICATION's identity, session metadata, security notice, and tip."
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
                    privileges.~%Sandboxing is no substitute for human oversight")))
     (application--command-tip-spans
      (application--startup-command-entry)))))

(-> application--update-notice (application) (option list))
(defun application--update-notice (application)
  "Return the cached update notice appropriate to APPLICATION's installation."
  (let ((availability (application-update-availability application)))
    (when availability
      (let ((current-version +autolith-version+)
            (latest-version (subseq (update-availability-tag availability) 1)))
        (list
         (terminal-span
          ':notice
          (format nil "Update available: Autolith ~A -> ~A.~%"
                  current-version latest-version))
         (terminal-span
          ':dim
          (if (eq (update-availability-method availability) ':nix)
              "Installed through Nix. Update the flake or profile that provides Autolith."
              "Choose whether to install it before continuing.")))))))

(-> application--update-choice-items () list)
(defun application--update-choice-items ()
  "Return the explicit user choices for a packaged release update."
  '((:name "Not now" :argument nil
     :description "continue with the installed release")
    (:name "Update now" :argument nil
     :description "install the verified release and restart")
    (:name "Skip this version" :argument nil
     :description "hide this exact release until a newer one appears")))

(-> application--offer-startup-update (application) null)
(defun application--offer-startup-update (application)
  "Offer an attended update for APPLICATION's validated packaged release."
  (let ((availability (application-update-availability application)))
    (when (and availability
               (eq (update-availability-method availability) ':release))
      (let* ((tag (update-availability-tag availability))
             (choice
               (terminal-ui-select
                (application-ui application)
                :title (format nil "Autolith ~A is available" (subseq tag 1))
                :items (application--update-choice-items)
                :resize-callback #'application-pending-terminal-size)))
        (cond
          ((string= (or choice "") "Update now")
           (error 'update-requested
                  :message (format nil "Update to Autolith ~A." (subseq tag 1))
                  :tag tag))
          ((string= (or choice "") "Skip this version")
           (update-state-dismiss (application-configuration application) tag)
           (setf (application-update-availability application) nil)
           (application-present
            application
            (list (terminal-span ':dim
                                 (format nil "Skipped Autolith ~A." (subseq tag 1))))))))))
  nil)

(-> application-handle-expected-error (application autolith-error) null)
(defun application-handle-expected-error (application condition)
  "Present expected CONDITION without abandoning APPLICATION's active path."
  (application-set-activity application nil)
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

(-> application--initial-work-items
    ((option string) boolean)
    list)
(defun application--initial-work-items (initial-command resume-offer-p)
  "Return ordered controller work for command-line startup behavior."
  (cond
    (initial-command
     (list (list ':command initial-command)))
    (resume-offer-p
     (list (list ':project-adaptation-offer)))
    (t
     nil)))

(-> application-run
    (application &key (:initial-command (option string))
                      (:initial-input (option user-message-input))
                      (:resume-offer-p boolean))
    null)
(defun application-run
    (application &key initial-command initial-input resume-offer-p)
  "Run APPLICATION with responsive input, always restoring terminal and workers."
  (let ((ui (application-ui application))
        (worker (application-worker application))
        (input-controller nil)
        (tool-runtimes-closed-p nil)
        (worker-stopped-p nil))
    (labels ((close-runtime-resources ()
               "Close APPLICATION's external runtimes at most once."
               (unless tool-runtimes-closed-p
                 (unwind-protect
                      (ignore-errors
                        (tool-registry-close-runtime-state
                         (application-tool-registry application)))
                   (setf tool-runtimes-closed-p t)))
               (unless worker-stopped-p
                 (unwind-protect
                      (when worker
                        (lisp-worker-manager-stop worker))
                   (setf worker-stopped-p t)))
               nil)

             (finish-shutdown ()
               "Close runtimes while preserving CONTROLLER's Ctrl-C escape."
               (if input-controller
                   (progn
                     (application-input-controller-call-with-shutdown-escape
                      input-controller #'close-runtime-resources)
                     (setf input-controller nil))
                   (close-runtime-resources))))
      (sb-sys:enable-interrupt
       sb-unix:sigwinch
       (lambda (signal code context)
         (declare (ignore signal code context))
         (setf *terminal-resize-pending-p* t)))
      (unwind-protect
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (unwind-protect
                  (progn
                    (application-present application
                                         (application-banner application))
                    (let ((update-notice
                            (application--update-notice application)))
                      (when update-notice
                        (application-present application update-notice)))
                    (when (and (null initial-command) (null initial-input))
                      (application--offer-startup-update application))
                    (let ((provenance
                            (application-installation-provenance application)))
                      (when provenance
                        (setf (application-update-check-thread application)
                              (update-check-start
                               (application-configuration application)
                               provenance))))
                    (dolist (failure
                             (application-overlay-failures application))
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
                    (when initial-input
                      (terminal-ui-set-input ui initial-input))
                    (setf (application-project-adaptation-offer-p application)
                          (and resume-offer-p (not (null initial-command))))
                    (setf input-controller
                          (application-input-controller-create
                           application
                           :initial-work-items
                           (application--initial-work-items initial-command
                                                            resume-offer-p)))
                    ;; Entering the interactive debugger would hang the raw
                    ;; terminal, so debugger entry becomes fatal recovery.
                    (let ((*checkpoint-thread-quiescer*
                            (lambda (function)
                              (application-input-controller-call-with-reader-paused
                               input-controller
                               (lambda ()
                                 (application--quiesce-update-check application)
                                 (funcall function)))))
                          (*debugger-hook*
                            (lambda (condition hook)
                              (declare (ignore hook))
                              (application-raise-fatal
                               application
                               condition
                               (application-safe-backtrace)))))
                      (handler-case
                          (loop
                            for work =
                              (application-input-controller--next-work
                               input-controller)
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
                           (application-input-failed-original-condition
                            condition)
                           (application-input-failed-backtrace condition)))))
                    (when (eq
                           (application-input-controller-exit-reason
                            input-controller)
                           ':interrupt)
                      (application--present-resume-instruction application)))
               (finish-shutdown)))
        (sb-sys:enable-interrupt sb-unix:sigwinch :default)
        (finish-shutdown))))
  nil)

;;;; -- Command-Line Entry --

(defconstant +main-fatal-recovery-status+ 70
  "The process status asking the stable launcher to recover after a fatal error.")

(defconstant +main-rollback-recovery-status+ 75
  "The process status asking the stable launcher to start a selected rollback.")

(defconstant +main-update-request-status+ 76
  "The process status asking a packaged outer launcher to perform an update.")

(-> main-usage () string)
(defun main-usage ()
  "Return the command-line usage text."
  "Usage: autolith [--from-source] [--immutable]
       autolith [--from-source] [--immutable] [-i FILE | --image FILE]...
       autolith [--from-source] [--immutable] resume [ID]
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

(-> main--resume-selection (list) (values boolean (option string)))
(defun main--resume-selection (arguments)
  "Return whether ARGUMENTS request resume and their optional identifier."
  (let ((position
          (position "resume" arguments :test #'string=)))
    (if position
        (let ((candidate (nth (1+ position) arguments)))
          (values t
                  (and (non-empty-string-p candidate)
                       (not (uiop:string-prefix-p "-" candidate))
                       candidate)))
        (values nil nil))))

(-> main--image-values (list) list)
(defun main--image-values (arguments)
  "Return image value strings carried by repeatable -i and --image options."
  (let ((values nil)
        (remaining arguments))
    (loop while remaining
          for argument = (pop remaining)
          do (cond
               ((or (string= argument "-i")
                    (string= argument "--image"))
                (unless remaining
                  (error 'configuration-error
                         :message (format nil "~A requires an image pathname."
                                          argument)))
                (let ((value (pop remaining)))
                  (when (uiop:string-prefix-p "-" value)
                    (error 'configuration-error
                           :message (format nil
                                            "~A requires an image pathname."
                                            argument)))
                  (setf values
                        (nconc values
                               (uiop:split-string value :separator '(#\,))))))
               ((uiop:string-prefix-p "--image=" argument)
                (setf values
                      (nconc values
                             (uiop:split-string
                              (subseq argument (length "--image="))
                              :separator '(#\,)))))))
    (when (some (lambda (value) (not (non-empty-string-p value))) values)
      (error 'configuration-error
             :message "Every --image value must name a local image."))
    values))

(-> main--initial-image-input (list) (option user-message-input))
(defun main--initial-image-input (arguments)
  "Return a labelled initial composer draft for ARGUMENTS' local images."
  (let ((pathnames
          (mapcar #'image-input-validate-pathname
                  (main--image-values arguments))))
    (when pathnames
      (user-message-input-create
       :text (format nil "~{~A~^ ~}"
                     (loop for number from 1 to (length pathnames)
                           collect (terminal-ui--image-label number)))
       :image-pathnames pathnames))))

(-> main-dispatch (list) null)
(defun main-dispatch (arguments)
  "Dispatch validated Autolith ARGUMENTS inside the active process."
  (cond
    ((and (= (length arguments) 3)
          (string= (first arguments)
                   +image-commit-replay-probe-argument+))
     (image-commit-replay-probe-main (second arguments)
                                     (third arguments)))
    ((member "--worker" arguments :test #'string=)
     (worker-main))
    ((member "--version" arguments :test #'string=)
     (format t "autolith ~A~%" +autolith-version+))
    ((or (member "--help" arguments :test #'string=)
         (member "-h" arguments :test #'string=))
     (format t "~A~%" (main-usage)))
    (t
     (let* ((immutable-p (not (null (member "--immutable" arguments
                                            :test #'string=))))
            (configuration (configuration-create :immutable-p immutable-p))
            (resume-selection
              (multiple-value-list (main--resume-selection arguments)))
            (resume-requested-p (first resume-selection))
            (resume-id (second resume-selection)))
       (cond
         ((member "--auth" arguments :test #'string=)
          (main-authenticate configuration))
         (t
          (setf *active-application*
                (if (typep *active-application* 'application)
                    (application-reconnect *active-application*
                                           :conversation-id resume-id
                                           :immutable-p immutable-p)
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
              (application-run
               *active-application*
               :initial-command (and resume-requested-p
                                     (null resume-id)
                                     "/resume")
               :initial-input (main--initial-image-input arguments)
               :resume-offer-p resume-requested-p)
            (rollback-requested (condition)
              (format *error-output*
                      "Autolith is rolling back to retained generation ~A.~%"
                      (rollback-requested-generation-id condition))
              (uiop:quit +main-rollback-recovery-status+))
            (update-requested (condition)
              (format *error-output*
                      "Autolith will update to ~A after restoring the terminal.~%"
                      (subseq (update-requested-tag condition) 1))
              (uiop:quit +main-update-request-status+))
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
