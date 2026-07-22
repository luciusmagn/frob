(in-package #:autolith)

;;;; -- Responsive Terminal Input --

(defclass application-input-controller ()
  ((application
    :initarg :application
    :reader application-input-controller-application
    :type application
    :documentation "The application receiving terminal events and submitted work.")
   (lock
    :initform (make-lock "Autolith input controller")
    :reader application-input-controller-lock
    :type t
    :documentation "The lock protecting work, reader, and exit state.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith input controller")
    :reader application-input-controller-condition-variable
    :type t
    :documentation "The main and reader thread wakeup condition.")
   (work-items
    :initform nil
    :accessor application-input-controller-work-items
    :type list
    :documentation "FIFO message and command work submitted by the reader.")
   (steering-items
    :initform nil
    :accessor application-input-controller-steering-items
    :type list
    :documentation "FIFO user messages waiting for the active turn's next tool boundary.")
   (later-state
    :initarg :later-state
    :reader application-input-controller-later-state
    :type later-state
    :documentation "The durable deferred inputs owned by this controller.")
   (pending-later-entries
    :initarg :pending-later-entries
    :accessor application-input-controller-pending-later-entries
    :type list
    :documentation "Deferred entries not currently dispatched by this process.")
   (active-p
    :initform nil
    :accessor application-input-controller-active-p
    :type boolean
    :documentation "Whether the main thread is processing one work item.")
   (stopping-p
    :initform nil
    :accessor application-input-controller-stopping-p
    :type boolean
    :documentation "Whether no more terminal input or work may be accepted.")
   (exit-reason
    :initform nil
    :accessor application-input-controller-exit-reason
    :type (option keyword)
    :documentation "The user-facing reason input processing stopped.")
   (reader-thread
    :initform nil
    :accessor application-input-controller-reader-thread
    :type t
    :documentation "The restartable terminal reader thread.")
   (reader-paused-p
    :initform nil
    :accessor application-input-controller-reader-paused-p
    :type boolean
    :documentation "Whether the reader must remain stopped for main-thread input.")
   (pause-depth
    :initform 0
    :accessor application-input-controller-pause-depth
    :type (integer 0)
    :documentation "Nested main-thread requests keeping the reader stopped.")
   (main-thread
    :initarg :main-thread
    :reader application-input-controller-main-thread
    :type t
    :documentation "The model and command thread interrupted for immediate exit.")
   (failure
    :initform nil
    :accessor application-input-controller-failure
    :type (option serious-condition)
    :documentation "A fatal terminal-reader condition awaiting main-thread handling.")
   (failure-backtrace
    :initform nil
    :accessor application-input-controller-failure-backtrace
    :type (option string)
    :documentation "The reader backtrace captured with FAILURE."))
  (:documentation
   "Ephemeral terminal input and FIFO submission state for one application run."))

(-> application-input--text ((or string user-message-input)) string)
(defun application-input--text (input)
  "Return the editable text carried by INPUT."
  (etypecase input
    (string input)
    (user-message-input (user-message-input-text input))))

(-> application-input--copy
    ((or string user-message-input))
    (or string user-message-input))
(defun application-input--copy (input)
  "Return a detached copy of INPUT."
  (etypecase input
    (string (copy-seq input))
    (user-message-input (user-message-input-copy input))))

(-> application-input--preview ((or string user-message-input)) string)
(defun application-input--preview (input)
  "Return INPUT's text for pending-work presentation."
  (etypecase input
    (string input)
    (user-message-input (user-message-input-preview input))))

(-> application--message-input
    ((or string user-message-input))
    (option (or string user-message-input)))
(defun application--message-input (input)
  "Return INPUT's model message, or NIL when it is empty or a slash command."
  (let ((text (application-input--text input)))
    (cond
      ((and (not (non-empty-string-p text))
            (not (and (typep input 'user-message-input)
                      (user-message-input-image-pathnames input))))
       nil)
      ((uiop:string-prefix-p "//" text)
       (etypecase input
         (string (subseq text 1))
         (user-message-input
          (user-message-input-create
           :text (subseq text 1)
           :image-pathnames (user-message-input-image-pathnames input)))))
      ((uiop:string-prefix-p "/" text)
       nil)
      (t
       (application-input--copy input)))))

(-> application--quit-command-p (string) boolean)
(defun application--quit-command-p (input)
  "Return true when INPUT is the explicit quit or exit slash command."
  (let ((command
          (application-command-canonical-name
           (or (first (uiop:split-string
                       input
                       :separator '(#\Space #\Tab)))
               ""))))
    (string= command "/quit")))

(-> application--command-needs-terminal-owner-p (string) boolean)
(defun application--command-needs-terminal-owner-p (input)
  "Return true when command INPUT must read from or reconfigure the terminal."
  (let* ((parts (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string input :separator '(#\Space #\Tab))))
         (command
           (application-command-canonical-name (or (first parts) "")))
         (argument (second parts)))
    (or (not (null (member command '("/auth" "/model") :test #'string=)))
        (and (null argument)
             (not
              (null
               (member command
                       '("/resume" "/effort" "/rollback" "/permissions")
                       :test #'string=)))))))

(-> application-input-controller--publish-counts
    (application-input-controller)
    null)
(defun application-input-controller--publish-counts (controller)
  "Publish CONTROLLER's pending input previews through its serialized UI."
  (with-lock-held ((application-input-controller-lock controller))
    (terminal-ui-set-pending-inputs
     (application-ui (application-input-controller-application controller))
     (mapcar #'application-input--preview
             (application-input-controller-steering-items controller))
     (loop for work in (application-input-controller-work-items controller)
           for input = (second work)
           when (typep input '(or string user-message-input))
             collect (application-input--preview input))))
  nil)

(-> application-input-controller-turn-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller-turn-active-p (controller)
  "Return true when CONTROLLER's main thread is processing one work item."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-active-p controller)))))

(-> application-input-controller-busy-p
    (application-input-controller)
    boolean)
(defun application-input-controller-busy-p (controller)
  "Return true when CONTROLLER has active or pending application work."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (or (application-input-controller-active-p controller)
          (application-input-controller-work-items controller))))))

(-> application-input-controller--interrupt-main
    (application-input-controller condition)
    null)
(defun application-input-controller--interrupt-main (controller condition)
  "Signal CONDITION on CONTROLLER's main thread unless already running there."
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread thread (lambda () (error condition))))))
  nil)

(-> application-input-controller--record-failure
    (application-input-controller serious-condition (option string))
    null)
(defun application-input-controller--record-failure
    (controller condition backtrace)
  "Record reader CONDITION, discard pending work, and wake the main thread."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-failure controller)
        (setf (application-input-controller-failure controller) condition
              (application-input-controller-failure-backtrace controller) backtrace
              (application-input-controller-work-items controller) nil
              (application-input-controller-steering-items controller) nil
              (application-input-controller-stopping-p controller) t))
      (setf active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition
            'application-input-failed
            :original-condition condition
            :backtrace backtrace))
        (error ()
          nil))))
  nil)

(-> application-input-controller--enqueue
    (application-input-controller keyword (or string user-message-input))
    null)
(defun application-input-controller--enqueue (controller kind input)
  "Append one work item of KIND carrying INPUT to CONTROLLER."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (setf (application-input-controller-work-items controller)
            (nconc (application-input-controller-work-items controller)
                   (list (list kind (application-input--copy input)))))
      (condition-notify
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--enqueue-steering
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--enqueue-steering (controller input)
  "Queue INPUT for the active turn, or promote it before follow-ups if that turn ended."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (if (application-input-controller-active-p controller)
          (setf (application-input-controller-steering-items controller)
                (nconc (application-input-controller-steering-items controller)
                       (list (application-input--copy input))))
          (push (list ':message (application-input--copy input))
                (application-input-controller-work-items controller)))
      (condition-notify
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--take-steering
    (application-input-controller)
    list)
(defun application-input-controller--take-steering (controller)
  "Return and consume CONTROLLER's messages for the completed tool boundary."
  (let ((messages nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (setf messages (application-input-controller-steering-items controller)
              (application-input-controller-steering-items controller) nil)))
    (application-input-controller--publish-counts controller)
    messages))

(-> application-input-controller--request-exit
    (application-input-controller keyword)
    null)
(defun application-input-controller--request-exit (controller reason)
  "Stop CONTROLLER for REASON, discarding work and cancelling an active turn."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-exit-reason controller)
        (setf (application-input-controller-exit-reason controller) reason))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-steering-items controller) nil
            active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition 'application-turn-cancelled))
        (error ()
          nil))))
  nil)

(-> application-input-controller--hold-command
    (application-input-controller string)
    null)
(defun application-input-controller--hold-command (controller input)
  "Restore busy command INPUT and explain when it can be submitted."
  (let* ((application (application-input-controller-application controller))
         (ui (application-ui application)))
    (terminal-ui-set-input ui input)
    (application-present
     application
     (list
      (terminal-span
       ':hint
       "∙ command held until the current response finishes")
      (terminal-span ':plain (string #\Newline))
      (terminal-span
       ':dim
       "  Edit it now or press Enter again when idle."))))
  nil)

(-> application-input-controller--handle-submission
    (application-input-controller (or string user-message-input)
     &key (:steer-p boolean))
    null)
(defun application-input-controller--handle-submission
    (controller input &key steer-p)
  "Route submitted INPUT to model work, command work, or busy-command policy."
  (let ((message (application--message-input input))
        (text (application-input--text input)))
    (cond
      (message
       (if steer-p
           (application-input-controller--enqueue-steering controller message)
           (application-input-controller--enqueue controller ':message message)))
      ((not (non-empty-string-p text))
       nil)
      ((application-input-controller-busy-p controller)
       (if (application--quit-command-p text)
           (application-input-controller--request-exit controller ':quit)
           (application-input-controller--hold-command controller text)))
      (t
       (application-input-controller--enqueue controller ':command text))))
  nil)

(-> application-input-controller--handle-queue-submission
    (application-input-controller (or string user-message-input))
    null)
(defun application-input-controller--handle-queue-submission (controller input)
  "Queue INPUT as post-turn message or command work."
  (let ((message (application--message-input input))
        (text (application-input--text input)))
    (cond
      (message
       (application-input-controller--enqueue controller ':message message))
      ((non-empty-string-p text)
       (application-input-controller--enqueue controller ':command text))))
  nil)

(-> application-input-controller--recall-follow-up
    (application-input-controller)
    boolean)
(defun application-input-controller--recall-follow-up (controller)
  "Recall CONTROLLER's newest follow-up into the editor for revision."
  (let ((work nil)
        (steering-inputs nil)
        (queued-inputs nil))
    (with-lock-held ((application-input-controller-lock controller))
      (when (and (application-input-controller-active-p controller)
                 (application-input-controller-work-items controller))
        (setf work (first (last
                           (application-input-controller-work-items controller)))
              (application-input-controller-work-items controller)
              (butlast (application-input-controller-work-items controller))
              steering-inputs
              (copy-list
               (application-input-controller-steering-items controller))
              queued-inputs
              (loop for queued-work
                      in (application-input-controller-work-items controller)
                    for input = (second queued-work)
                    when (typep input '(or string user-message-input))
                      collect (application-input--preview input)))))
    (when work
      (terminal-ui-recall-follow-up
       (application-ui (application-input-controller-application controller))
       (second work)
       :steering-inputs steering-inputs
       :queued-inputs queued-inputs))
    (not (null work))))

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (let ((ui (application-ui
             (application-input-controller-application controller)))
        (turn-active-p
          (application-input-controller-turn-active-p controller)))
    (multiple-value-bind (action payload)
        (terminal-ui-process-event
         ui event :queue-completion-p turn-active-p)
      (case action
        (:submit
         (application-input-controller--handle-submission
          controller payload :steer-p turn-active-p))
        (:queue
         (application-input-controller--handle-queue-submission
          controller payload))
        (:edit-queue
         (application-input-controller--recall-follow-up controller))
        (:end-of-input
         (application-input-controller--request-exit controller ':end-of-input))
        (:interrupt
         (application-input-controller--request-exit controller ':interrupt)))))
  nil)

(-> application-input-controller--input-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--input-ready-p (controller)
  "Apply pending resizes and report whether CONTROLLER's terminal has input."
  (let* ((ui (application-ui
              (application-input-controller-application controller)))
         (terminal (terminal-ui-terminal ui)))
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)
    (terminal-ui-refresh-status ui)
    (if (terminal-input-ready-p terminal)
        t
        (progn
          (with-lock-held ((application-input-controller-lock controller))
            (unless (or (application-input-controller-stopping-p controller)
                        (application-input-controller-reader-paused-p controller))
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout 0.02)))
          nil))))

(-> application-input-controller--reader-loop
    (application-input-controller)
    null)
(defun application-input-controller--reader-loop (controller)
  "Read and process terminal events until pause, exit, or reader failure."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (loop
            (when
                (with-lock-held ((application-input-controller-lock controller))
                  (or (application-input-controller-stopping-p controller)
                      (application-input-controller-reader-paused-p controller)))
              (return))
            (when (application-input-controller--input-ready-p controller)
              (application-input-controller--process-event
               controller
               (application-read-terminal-event
                (application-ui
                 (application-input-controller-application controller))))))
        (serious-condition (condition)
          (application-input-controller--record-failure
           controller condition signal-backtrace)))))
  nil)

(-> application-input-controller--start-reader
    (application-input-controller)
    null)
(defun application-input-controller--start-reader (controller)
  "Start CONTROLLER's reader unless it is paused, stopping, or already live."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (or (application-input-controller-stopping-p controller)
                (application-input-controller-reader-paused-p controller)
                (let ((thread
                        (application-input-controller-reader-thread controller)))
                  (and thread (thread-alive-p thread))))
      (setf (application-input-controller-reader-thread controller)
            (make-thread
             (lambda ()
               (application-input-controller--reader-loop controller))
             :name "Autolith terminal input"))))
  nil)

(-> application-input-controller--pause-reader
    (application-input-controller)
    null)
(defun application-input-controller--pause-reader (controller)
  "Stop and join CONTROLLER's reader without ending the application."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-reader-paused-p controller) t
            thread (application-input-controller-reader-thread controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil)))))
  nil)

(-> application-input-controller-call-with-reader-paused
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-reader-paused
    (controller function)
  "Call FUNCTION while CONTROLLER has no live terminal reader."
  (let ((outermost-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf outermost-p
            (zerop (application-input-controller-pause-depth controller)))
      (incf (application-input-controller-pause-depth controller)))
    (when outermost-p
      (application-input-controller--pause-reader controller))
    (unwind-protect
         (funcall function)
      (let ((restart-p nil))
        (with-lock-held ((application-input-controller-lock controller))
          (decf (application-input-controller-pause-depth controller))
          (when (zerop (application-input-controller-pause-depth controller))
            (setf (application-input-controller-reader-paused-p controller) nil
                  restart-p
                  (not (application-input-controller-stopping-p controller)))))
        (when restart-p
          (application-input-controller--start-reader controller))))))

(-> application--command-authorization-items (string pathname) list)
(defun application--command-authorization-items (command directory)
  "Return the modal choices for COMMAND in DIRECTORY."
  (declare (ignore command))
  (list
   (list :name "once"
         :argument nil
         :description "allow once inside the workspace sandbox")
   (list :name "always"
         :argument nil
         :description
         (format nil "always allow this exact command in ~A"
                 (application--abbreviated-directory (namestring directory))))
   (list :name "sandbox"
         :argument nil
         :description "allow sandboxed commands for this session")
   (list :name "full"
         :argument nil
         :description "let it ride with full user privileges for this session")
   (list :name "deny"
         :argument nil
         :description "do not run the command")))

(-> application--ask-command-permission
    (application string pathname)
    keyword)
(defun application--ask-command-permission (application command directory)
  "Ask interactively how COMMAND may run in DIRECTORY, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (terminal-ui-select
                  ui
                  :title
                  (format nil "run ~A"
                          (text-cell-prefix
                           (sanitize-text command :single-line-p t)
                           56))
                  :items (application--command-authorization-items
                          command directory)
                  :resize-callback #'application-pending-terminal-size)))))
        (cond
          ((string= (or choice "") "once")
           ':sandboxed)
          ((string= (or choice "") "always")
           (permissions-allow
            :configuration (application-configuration application)
            :state         (application-permission-state application)
            :command       command
            :directory     directory)
           ':sandboxed)
          ((string= (or choice "") "sandbox")
           (setf (application-permission-mode application) ':sandboxed)
           ':sandboxed)
          ((string= (or choice "") "full")
           (setf (application-permission-mode application) ':full-access)
           ':full-access)
          (t
           ':deny))))))

(-> application-authorize-command (application string pathname) keyword)
(defun application-authorize-command (application command directory)
  "Return the session, saved, or interactively selected permission for COMMAND."
  (with-lock-held ((application-command-authorization-lock application))
    (case (application-permission-mode application)
      (:full-access
       ':full-access)
      (:sandboxed
       ':sandboxed)
      (:ask
       (if (permissions-allowed-p
            (application-permission-state application)
            command
            directory)
           ':sandboxed
           (application--ask-command-permission
            application command directory))))))

(-> application-input-controller-schedule-later
    (application-input-controller string &key (:due-at timestamp) (:window string))
    later-entry)
(defun application-input-controller-schedule-later
    (controller input &key due-at window)
  "Persist INPUT for DUE-AT and wake CONTROLLER's deferred scheduler."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (entry
           (later-schedule
            :configuration configuration
            :state (application-input-controller-later-state controller)
            :input input
            :directory (configuration-working-directory configuration)
            :due-at due-at
            :window window)))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-pending-later-entries controller)
            (later--sort-entries
             (append
              (application-input-controller-pending-later-entries controller)
              (list entry))))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    entry))

(-> application-input-controller-cancel-later
    (application-input-controller string)
    boolean)
(defun application-input-controller-cancel-later (controller identifier)
  "Cancel deferred IDENTIFIER durably and remove it from CONTROLLER."
  (let* ((application (application-input-controller-application controller))
         (cancelled-p
           (later-cancel
            (application-configuration application)
            (application-input-controller-later-state controller)
            identifier)))
    (when cancelled-p
      (with-lock-held ((application-input-controller-lock controller))
        (setf (application-input-controller-pending-later-entries controller)
              (remove identifier
                      (application-input-controller-pending-later-entries
                       controller)
                      :key #'later-entry-identifier
                      :test #'string=))
        (condition-notify
         (application-input-controller-condition-variable controller))))
    cancelled-p))

(-> application-input-controller--promote-due-later
    (application-input-controller timestamp)
    null)
(defun application-input-controller--promote-due-later (controller now)
  "Move CONTROLLER's entries due at NOW onto its ordinary work queue."
  (loop for entry = (first
                     (application-input-controller-pending-later-entries
                      controller))
        while (and entry (<= (later-entry-due-at entry) now))
        do (pop (application-input-controller-pending-later-entries controller))
           (setf (application-input-controller-work-items controller)
                 (nconc (application-input-controller-work-items controller)
                        (list (list ':later entry)))))
  nil)

(-> application-input-controller--later-wait-seconds
    (application-input-controller timestamp)
    (option real))
(defun application-input-controller--later-wait-seconds (controller now)
  "Return seconds until CONTROLLER's next deferred entry, if one exists."
  (let ((entry (first
                (application-input-controller-pending-later-entries controller))))
    (and entry (max 0.01 (- (later-entry-due-at entry) now)))))

(-> application-input-controller--complete-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--complete-later (controller entry)
  "Remove successfully dispatched ENTRY from durable deferred state."
  (later-cancel
   (application-configuration
    (application-input-controller-application controller))
   (application-input-controller-later-state controller)
   (later-entry-identifier entry))
  nil)

(-> application-input-controller--retry-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--retry-later (controller entry)
  "Reschedule failed ENTRY from current rate data or a five-minute fallback."
  (let* ((application (application-input-controller-application controller))
         (configuration (application-configuration application))
         (provider (application-provider application))
         (now (get-universal-time)))
    (multiple-value-bind (reset-at window)
        (later-reset-deadline (and provider (provider-rate-limits provider))
                              :now now)
      (let ((replacement
              (later-reschedule
               :configuration configuration
               :state (application-input-controller-later-state controller)
               :entry entry
               :due-at (if (and reset-at (> reset-at now))
                           reset-at
                           (+ now 300))
               :window (if (and window reset-at (> reset-at now))
                           window
                           "5 minute retry"))))
        (with-lock-held ((application-input-controller-lock controller))
          (setf (application-input-controller-pending-later-entries controller)
                (later--sort-entries
                 (append
                  (application-input-controller-pending-later-entries controller)
                  (list replacement))))
          (condition-notify
           (application-input-controller-condition-variable controller)))
        (application-present
         application
         (format nil "Deferred input ~A was rescheduled after ~A."
                 (later-entry-identifier replacement)
                 (later-entry-window replacement))))))
  nil)

(-> application-input-controller-create
    (application)
    application-input-controller)
(defun application-input-controller-create (application)
  "Create CONTROLLER for APPLICATION and start its terminal reader."
  (let* ((configuration
           (and (slot-boundp application 'configuration)
                (application-configuration application)))
         (later-state
           (if (typep configuration 'configuration)
               (later-load configuration)
               (make-instance 'later-state)))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :later-state later-state
                          :pending-later-entries
                          (copy-list (later-state-entries later-state))
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (application-input-controller--start-reader controller)
    controller))

(-> application-input-controller--next-work
    (application-input-controller)
    (option list))
(defun application-input-controller--next-work (controller)
  "Wait for and return CONTROLLER's next work item, or NIL after exit."
  (let ((work nil))
    (with-lock-held ((application-input-controller-lock controller))
      (loop
        do (application-input-controller--promote-due-later
            controller (get-universal-time))
        while (and
               (null (application-input-controller-work-items controller))
               (null (application-input-controller-failure controller))
               (not (application-input-controller-stopping-p controller)))
        do (let ((wait-seconds
                   (application-input-controller--later-wait-seconds
                    controller (get-universal-time))))
             (if wait-seconds
                 (condition-wait
                  (application-input-controller-condition-variable controller)
                  (application-input-controller-lock controller)
                  :timeout wait-seconds)
                 (condition-wait
                  (application-input-controller-condition-variable controller)
                  (application-input-controller-lock controller)))))
      (when (application-input-controller-failure controller)
        (error
         'application-input-failed
         :original-condition (application-input-controller-failure controller)
         :backtrace (application-input-controller-failure-backtrace controller)))
      (unless (application-input-controller-stopping-p controller)
        (setf work (pop (application-input-controller-work-items controller))
              (application-input-controller-active-p controller) t)))
    (application-input-controller--publish-counts controller)
    work))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Finish current work and promote unconsumed steering before queued follow-ups."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (let ((steering-items
              (application-input-controller-steering-items controller)))
        (when steering-items
          (setf (application-input-controller-work-items controller)
                (append (mapcar (lambda (input)
                                  (list ':message input))
                                steering-items)
                        (application-input-controller-work-items controller)))))
      (setf (application-input-controller-steering-items controller) nil))
    (setf (application-input-controller-active-p controller) nil)
    (condition-notify
     (application-input-controller-condition-variable controller)))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Stop CONTROLLER, discard pending work, and join its terminal reader."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-reader-paused-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-steering-items controller) nil
            (application-input-controller-pending-later-entries controller) nil
            (application-input-controller-active-p controller) nil
            thread (application-input-controller-reader-thread controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (terminal-ui-set-pending-inputs
     (application-ui (application-input-controller-application controller))
     nil
     nil)
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil))))
    (let ((application (application-input-controller-application controller)))
      (when (eq controller (application-input-controller application))
        (setf (application-input-controller application) nil))))
  nil)

(-> application--run-message-input
    (application (or string user-message-input)
     &key (:steering-function (option function)))
    keyword)
(defun application--run-message-input
    (application input &key steering-function)
  "Run model INPUT with established expected, cancellation, and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (progn
            (application-run-message
             application input :steering-function steering-function)
            ':continue)
        (application-turn-cancelled (condition)
          (error condition))
        (application-input-failed (condition)
          (error condition))
        (rollback-requested (condition)
          (error condition))
        ((or agent-loop-error
             conversation-invariant-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          ':failed)
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application--run-command-input (application string) keyword)
(defun application--run-command-input (application input)
  "Run command INPUT with established expected and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (application-handle-input application input)
        (rollback-requested (condition)
          (error condition))
        ((or agent-loop-error
             conversation-invariant-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          ':failed)
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application-input-controller--run-later
    (application-input-controller later-entry)
    null)
(defun application-input-controller--run-later (controller entry)
  "Dispatch due deferred ENTRY and durably complete or retry it."
  (block nil
    (let* ((application (application-input-controller-application controller))
           (input (later-entry-input entry)))
      (application-present
       application
       (format nil "Running deferred input ~A after its ~A reset.~%  ~A"
               (later-entry-identifier entry)
               (later-entry-window entry)
               (text-cell-prefix
                (sanitize-text input :single-line-p t)
                72)))
      (handler-case
          (application-set-working-directory
           application (later-entry-directory entry))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          (handler-case
              (application-input-controller--complete-later controller entry)
            (later-error (persistence-condition)
              (application-handle-expected-error application
                                                 persistence-condition)))
          (return nil)))
      (let* ((message (application--message-input input))
             (result
              (if message
                  (application--run-message-input application message)
                  (application--run-command-input application input))))
        (handler-case
            (if (eq result ':failed)
                (application-input-controller--retry-later controller entry)
                (application-input-controller--complete-later controller entry))
          (later-error (condition)
            (application-handle-expected-error application condition)))
        (when (eq result ':quit)
          (application-input-controller--request-exit controller ':quit)))))
  nil)

(-> application-input-controller--run-work
    (application-input-controller list)
    null)
(defun application-input-controller--run-work (controller work)
  "Run one submitted WORK item on the application main thread."
  (let ((application (application-input-controller-application controller)))
    (case (first work)
      (:message
       (application--run-message-input
        application
        (second work)
        :steering-function
        (lambda ()
          (application-input-controller--take-steering controller))))
      (:command
       (let* ((input (second work))
              (result
                (if (application--command-needs-terminal-owner-p input)
                    (application-input-controller-call-with-reader-paused
                     controller
                     (lambda ()
                       (application--run-command-input application input)))
                    (application--run-command-input application input))))
         (when (eq result ':quit)
           (application-input-controller--request-exit controller ':quit))))
      (:later
       (application-input-controller--run-later controller (second work)))))
  nil)
