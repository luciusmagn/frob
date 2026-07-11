(in-package #:frob)

;;;; -- Terminal Constants --

(define-constant +terminal-default-columns+ 80
  :documentation "The fallback terminal width when no positive width is supplied.")

(define-constant +terminal-history-limit+ 100
  :documentation "The maximum number of submitted inputs retained by a line editor.")

(define-constant +terminal-escape-character+ (code-char 27)
  :test #'char=
  :documentation "The ASCII escape character used by trusted terminal controls.")

(define-constant +terminal-replacement-character+ (code-char #xfffd)
  :test #'char=
  :documentation "The visible replacement for untrusted terminal control characters.")

(define-constant +terminal-newline-marker+ (code-char #x21b5)
  :test #'char=
  :documentation "The single-row marker used to display an embedded newline.")

(define-constant +terminal-left-clipping-marker+ (code-char #x2039)
  :test #'char=
  :documentation "The marker showing that editor text is clipped on the left.")

(define-constant +terminal-bracketed-paste-enable+
  (format nil "~C[?2004h" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that enables bracketed paste mode.")

(define-constant +terminal-bracketed-paste-disable+
  (format nil "~C[?2004l" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that disables bracketed paste mode.")

(define-constant +terminal-bracketed-paste-start+
  (format nil "~C[200~~" +terminal-escape-character+)
  :test #'string=
  :documentation "The input marker beginning one bracketed paste payload.")

(define-constant +terminal-bracketed-paste-end+
  (format nil "~C[201~~" +terminal-escape-character+)
  :test #'string=
  :documentation "The input marker ending one bracketed paste payload.")

(define-constant +terminal-erase-line+
  (format nil "~C[2K" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that erases only the current terminal line.")


;;;; -- Terminal and Editor Objects --

(defclass terminal ()
  ((columns
    :initarg :columns
    :initform +terminal-default-columns+
    :accessor terminal-columns
    :type (integer 1)
    :documentation "The current terminal width in character cells.")
   (interactive-p
    :initarg :interactive-p
    :initform nil
    :accessor terminal-interactive-p
    :type boolean
    :documentation "Whether this terminal currently accepts noncanonical input.")
   (started-p
    :initform nil
    :accessor terminal-started-p
    :type boolean
    :documentation "Whether this terminal has entered its active lifecycle."))
  (:documentation "A replaceable primary-screen terminal transport."))

(defclass stream-terminal (terminal)
  ((input-stream
    :initarg :input-stream
    :reader stream-terminal-input-stream
    :type stream
    :documentation "The character stream carrying terminal input.")
   (output-stream
    :initarg :output-stream
    :reader stream-terminal-output-stream
    :type stream
    :documentation "The character stream receiving terminal output.")
   (input-file-descriptor
    :initarg :input-file-descriptor
    :reader stream-terminal-input-file-descriptor
    :type integer
    :documentation "The POSIX descriptor whose termios state is controlled.")
   (saved-terminal-mode
    :initform nil
    :accessor stream-terminal-saved-terminal-mode
    :type t
    :documentation "The exact termios value restored when the terminal stops."))
  (:documentation "An SBCL stream terminal backed by POSIX file descriptor zero."))

(defclass line-editor ()
  ((text
    :initform ""
    :reader line-editor-text
    :type string
    :documentation "The complete logical input, including pasted newlines.")
   (cursor
    :initform 0
    :reader line-editor-cursor
    :type (integer 0)
    :documentation "The character index at which the next insertion occurs.")
   (history
    :initform nil
    :reader line-editor-history
    :type list
    :documentation "Submitted inputs ordered from newest to oldest.")
   (history-index
    :initform -1
    :type integer
    :documentation "The recalled history index, or negative one while editing a draft.")
   (history-draft
    :initform nil
    :type (option string)
    :documentation "The input restored after leaving history navigation.")
   (history-limit
    :initarg :history-limit
    :reader line-editor-history-limit
    :type (integer 1)
    :documentation "The maximum number of retained history entries."))
  (:documentation "A small single-row editor with logical multiline paste support."))

(defclass terminal-ui ()
  ((terminal
    :initarg :terminal
    :reader terminal-ui-terminal
    :type terminal
    :documentation "The primary-screen terminal transport.")
   (editor
    :initarg :editor
    :reader terminal-ui-editor
    :type line-editor
    :documentation "The editable user input shown on the live prompt row.")
   (prompt
    :initarg :prompt
    :reader terminal-ui-prompt
    :type string
    :documentation "The untrusted-text-safe prompt prefix.")
   (status
    :initform nil
    :accessor terminal-ui-status
    :type (option string)
    :documentation "The optional unfinished activity shown above the prompt.")
   (finalized-identifiers
    :initform (make-hash-table :test #'equal)
    :reader terminal-ui-finalized-identifiers
    :type hash-table
    :documentation "Identifiers whose finalized transcript text was already emitted.")
   (live-rendered-p
    :initform nil
    :accessor terminal-ui-live-rendered-p
    :type boolean
    :documentation "Whether live rows currently occupy the terminal bottom.")
   (rendered-status-p
    :initform nil
    :accessor terminal-ui-rendered-status-p
    :type boolean
    :documentation "Whether the current live region includes a status row.")
   (started-p
    :initform nil
    :accessor terminal-ui-started-p
    :type boolean
    :documentation "Whether the UI lifecycle has started."))
  (:documentation
   "A scrollback-preserving UI with immutable transcript output and at most two live rows."))


;;;; -- Terminal Conditions --

(define-condition terminal-error (frob-error)
  ((operation
    :initarg :operation
    :reader terminal-error-operation
    :type keyword
    :documentation "The terminal operation that could not complete.")
   (cause
    :initarg :cause
    :reader terminal-error-cause
    :type (option condition)
    :documentation "The underlying implementation condition, when available."))
  (:documentation "A terminal mode, input, or output operation failed."))


;;;; -- Terminal Protocol --

(defgeneric terminal-start (terminal)
  (:documentation "Start TERMINAL without entering an alternate screen."))

(defgeneric terminal-stop (terminal)
  (:documentation "Restore TERMINAL input state and finish its lifecycle."))

(defgeneric terminal-read-event (terminal)
  (:documentation "Read and return one semantic input event from TERMINAL."))

(defgeneric terminal--write (terminal text)
  (:documentation "Write trusted renderer TEXT through the terminal transport."))

(defgeneric terminal-flush (terminal)
  (:documentation "Make all pending TERMINAL output visible."))


;;;; -- Editor Protocol --

(defgeneric line-editor-handle-event (editor event)
  (:documentation
   "Apply EVENT to EDITOR and return an action keyword plus optional submitted text."))

(defgeneric line-editor-render (editor prompt columns)
  (:documentation
   "Render EDITOR and PROMPT within COLUMNS, returning the row and cursor column."))


;;;; -- Text Safety and Width --

(-> terminal--normalize-newlines (string) string)
(defun terminal--normalize-newlines (text)
  "Return TEXT with carriage-return line endings normalized to newline characters."
  (with-output-to-string (stream)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (write-char #\Newline stream)
                (when (and (< (1+ index) (length text))
                           (char= (char text (1+ index)) #\Newline))
                  (incf index)))
               (t
                (write-char character stream)))
             (incf index))))

(-> terminal-sanitize-text
    (string &key (:single-line-p boolean))
    string)
(defun terminal-sanitize-text (text &key (single-line-p nil))
  "Return TEXT without executable ESC, C0, DEL, or C1 terminal controls.

Newlines remain newlines unless SINGLE-LINE-P is true. Tabs become spaces so
the renderer can calculate cursor positions deterministically."
  (with-output-to-string (stream)
    (loop for character across (terminal--normalize-newlines text)
          for code = (char-code character)
          do (cond
               ((char= character #\Newline)
                (write-char (if single-line-p
                                +terminal-newline-marker+
                                #\Newline)
                            stream))
               ((char= character #\Tab)
                (write-string "    " stream))
               ((or (< code 32)
                    (= code 127)
                    (<= 128 code 159))
                (write-char +terminal-replacement-character+ stream))
               (t
                (write-char character stream))))))

(-> terminal--character-width (character) (integer 0 2))
(defun terminal--character-width (character)
  "Return the terminal-cell width of CHARACTER for restrained prompt layout."
  (cond
    ((plusp (sb-unicode:combining-class character))
     0)
    ((member (sb-unicode:east-asian-width character) '(:w :f))
     2)
    (t
     1)))

(-> terminal--text-width (string &key (:start integer) (:end (option integer)))
    (integer 0))
(defun terminal--text-width (text &key (start 0) end)
  "Return the terminal-cell width of TEXT between START and END."
  (loop for index from start below (or end (length text))
        sum (terminal--character-width (char text index))))

(-> terminal--prefix-within-width (string integer) string)
(defun terminal--prefix-within-width (text maximum-width)
  "Return the longest prefix of TEXT whose cell width does not exceed MAXIMUM-WIDTH."
  (let ((width 0)
        (end 0))
    (loop while (< end (length text))
          for character-width = (terminal--character-width (char text end))
          while (<= (+ width character-width) maximum-width)
          do (incf width character-width)
             (incf end))
    (subseq text 0 end)))

(-> terminal--editor-window
    (string integer integer)
    (values string integer))
(defun terminal--editor-window (text cursor maximum-width)
  "Return a visible slice of TEXT and CURSOR's cell offset within that slice."
  (let ((start 0)
        (available (max 1 maximum-width)))
    (loop while (and (< start cursor)
                     (>= (terminal--text-width text :start start :end cursor)
                         available))
          do (incf start))
    (let* ((clipped-p (plusp start))
           (marker-width (if clipped-p 1 0))
           (content-width (max 0 (- available marker-width)))
           (end start)
           (used 0))
      (loop while (< end (length text))
            for character-width = (terminal--character-width (char text end))
            while (<= (+ used character-width) content-width)
            do (incf used character-width)
               (incf end))
      (let ((visible (if clipped-p
                         (concatenate 'string
                                      (string +terminal-left-clipping-marker+)
                                      (subseq text start end))
                         (subseq text start end)))
            (cursor-column
              (+ marker-width
                 (terminal--text-width text :start start :end cursor))))
        (values visible cursor-column)))))


;;;; -- Line Editor Mechanics --

(-> line-editor--set-state (line-editor string integer) line-editor)
(defun line-editor--set-state (editor text cursor)
  "Replace EDITOR's TEXT and CURSOR while preserving its history."
  (setf (slot-value editor 'text) text
        (slot-value editor 'cursor) (min (max 0 cursor) (length text)))
  editor)

(-> line-editor--leave-history (line-editor) null)
(defun line-editor--leave-history (editor)
  "Leave EDITOR's history navigation after a direct edit."
  (setf (slot-value editor 'history-index) -1
        (slot-value editor 'history-draft) nil)
  nil)

(-> line-editor--insert (line-editor string) null)
(defun line-editor--insert (editor inserted-text)
  "Insert sanitized INSERTED-TEXT at EDITOR's cursor."
  (line-editor--leave-history editor)
  (let* ((safe-text (terminal-sanitize-text inserted-text))
         (text (line-editor-text editor))
         (cursor (line-editor-cursor editor)))
    (line-editor--set-state
     editor
     (concatenate 'string
                  (subseq text 0 cursor)
                  safe-text
                  (subseq text cursor))
     (+ cursor (length safe-text))))
  nil)

(-> line-editor--delete-backward (line-editor) null)
(defun line-editor--delete-backward (editor)
  "Delete the character immediately before EDITOR's cursor."
  (line-editor--leave-history editor)
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (when (plusp cursor)
      (line-editor--set-state
       editor
       (concatenate 'string
                    (subseq text 0 (1- cursor))
                    (subseq text cursor))
       (1- cursor))))
  nil)

(-> line-editor--delete-forward (line-editor) null)
(defun line-editor--delete-forward (editor)
  "Delete the character at EDITOR's cursor."
  (line-editor--leave-history editor)
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (when (< cursor (length text))
      (line-editor--set-state
       editor
       (concatenate 'string
                    (subseq text 0 cursor)
                    (subseq text (1+ cursor)))
       cursor)))
  nil)

(-> line-editor--history-step (line-editor integer) null)
(defun line-editor--history-step (editor direction)
  "Move EDITOR one history entry in DIRECTION, where positive means older."
  (let* ((history (line-editor-history editor))
         (index (slot-value editor 'history-index)))
    (cond
      ((null history)
       nil)
      ((plusp direction)
       (when (< index (1- (length history)))
         (when (minusp index)
           (setf (slot-value editor 'history-draft)
                 (line-editor-text editor)))
         (incf (slot-value editor 'history-index))
         (let ((entry (nth (slot-value editor 'history-index) history)))
           (line-editor--set-state editor entry (length entry)))))
      ((not (minusp index))
       (decf (slot-value editor 'history-index))
       (if (minusp (slot-value editor 'history-index))
           (let ((draft (or (slot-value editor 'history-draft) "")))
             (setf (slot-value editor 'history-draft) nil)
             (line-editor--set-state editor draft (length draft)))
           (let ((entry (nth (slot-value editor 'history-index) history)))
             (line-editor--set-state editor entry (length entry)))))))
  nil)


;;;; -- Line Editor Methods --

(-> line-editor-set-text (line-editor string) line-editor)
(-> line-editor-clear (line-editor) line-editor)
(-> line-editor-add-history (line-editor string) line-editor)

(defmethod line-editor-handle-event ((editor line-editor) event)
  "Apply one semantic input EVENT to EDITOR."
  (labels ((changed ()
             (values :changed nil)))
    (cond
      ((and (consp event) (eq (first event) :insert))
       (line-editor--insert editor (second event))
       (changed))
      ((and (consp event) (eq (first event) :paste))
       (line-editor--insert editor (second event))
       (changed))
      ((and (consp event) (eq (first event) :line))
       (line-editor-set-text editor (second event))
       (line-editor-handle-event editor :submit))
      ((eq event :left)
       (when (plusp (line-editor-cursor editor))
         (decf (slot-value editor 'cursor)))
       (changed))
      ((eq event :right)
       (when (< (line-editor-cursor editor) (length (line-editor-text editor)))
         (incf (slot-value editor 'cursor)))
       (changed))
      ((eq event :home)
       (setf (slot-value editor 'cursor) 0)
       (changed))
      ((eq event :end)
       (setf (slot-value editor 'cursor) (length (line-editor-text editor)))
       (changed))
      ((eq event :backspace)
       (line-editor--delete-backward editor)
       (changed))
      ((eq event :delete)
       (line-editor--delete-forward editor)
       (changed))
      ((eq event :history-previous)
       (line-editor--history-step editor 1)
       (changed))
      ((eq event :history-next)
       (line-editor--history-step editor -1)
       (changed))
      ((eq event :interrupt)
       (if (plusp (length (line-editor-text editor)))
           (progn
             (line-editor-clear editor)
             (values :cleared nil))
           (values :interrupt nil)))
      ((eq event :end-of-input)
       (if (plusp (length (line-editor-text editor)))
           (progn
             (line-editor--delete-forward editor)
             (changed))
           (values :end-of-input nil)))
      ((eq event :submit)
       (let ((submitted (line-editor-text editor)))
         (when (non-empty-string-p submitted)
           (line-editor-add-history editor submitted))
         (line-editor-clear editor)
         (values :submit submitted)))
      ((eq event :escape)
       (values :escape nil))
      (t
       (values :ignored nil)))))

(defmethod line-editor-render
    ((editor line-editor) (prompt string) (columns integer))
  "Render EDITOR as one horizontally clipped row."
  (let* ((safe-prompt (terminal-sanitize-text prompt :single-line-p t))
         (prompt-limit (max 0 (1- columns)))
         (visible-prompt (terminal--prefix-within-width safe-prompt prompt-limit))
         (prompt-width (terminal--text-width visible-prompt))
         (content-width (max 1 (- columns prompt-width 1)))
         (safe-text (terminal-sanitize-text (line-editor-text editor)
                                            :single-line-p t)))
    (multiple-value-bind (visible-text cursor-offset)
        (terminal--editor-window safe-text
                                 (line-editor-cursor editor)
                                 content-width)
      (values (concatenate 'string visible-prompt visible-text)
              (min (1- columns) (+ prompt-width cursor-offset))))))

;; Generic FTYPEs remain broad so later adapters can add method specializations.
(-> line-editor-handle-event (t t) *)
(-> line-editor-render (t t t) *)


;;;; -- Stream Terminal Input --

(-> terminal-decode-sequence (string) t)
(defun terminal-decode-sequence (sequence)
  "Return the semantic event represented by one complete escape SEQUENCE."
  (cond
    ((string= sequence (format nil "~C[A" +terminal-escape-character+))
     :history-previous)
    ((string= sequence (format nil "~C[B" +terminal-escape-character+))
     :history-next)
    ((string= sequence (format nil "~C[C" +terminal-escape-character+))
     :right)
    ((string= sequence (format nil "~C[D" +terminal-escape-character+))
     :left)
    ((or (string= sequence (format nil "~C[H" +terminal-escape-character+))
         (string= sequence (format nil "~C[1~~" +terminal-escape-character+))
         (string= sequence (format nil "~COH" +terminal-escape-character+)))
     :home)
    ((or (string= sequence (format nil "~C[F" +terminal-escape-character+))
         (string= sequence (format nil "~C[4~~" +terminal-escape-character+))
         (string= sequence (format nil "~COF" +terminal-escape-character+)))
     :end)
    ((string= sequence (format nil "~C[3~~" +terminal-escape-character+))
     :delete)
    ((string= sequence +terminal-bracketed-paste-start+)
     :paste-start)
    (t
     :ignored)))

(-> terminal--character-ready-p (stream-terminal real) boolean)
(defun terminal--character-ready-p (terminal timeout)
  "Return true when TERMINAL input is ready within TIMEOUT seconds."
  (or (listen (stream-terminal-input-stream terminal))
      (and (terminal-interactive-p terminal)
           (sb-sys:wait-until-fd-usable
            (stream-terminal-input-file-descriptor terminal)
            :input
            timeout
            nil))))

(-> terminal--csi-final-character-p (character) boolean)
(defun terminal--csi-final-character-p (character)
  "Return true when CHARACTER terminates a control-sequence introducer."
  (<= #x40 (char-code character) #x7e))

(-> terminal--read-escape-sequence (stream-terminal) t)
(defun terminal--read-escape-sequence (terminal)
  "Read and decode the escape sequence following an initial ESC from TERMINAL."
  (block nil
    (unless (terminal--character-ready-p terminal 0.025)
      (return :escape))
    (let* ((stream (stream-terminal-input-stream terminal))
           (second-character (read-char stream nil nil)))
      (unless second-character
        (return :escape))
      (let ((buffer (make-array 8
                                :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
        (vector-push-extend +terminal-escape-character+ buffer)
        (vector-push-extend second-character buffer)
        (cond
          ((or (char= second-character #\[)
               (char= second-character #\O))
           (loop repeat 16
                 do (unless (terminal--character-ready-p terminal 0.025)
                      (return :ignored))
                    (let ((character (read-char stream nil nil)))
                      (unless character
                        (return :ignored))
                      (vector-push-extend character buffer)
                      (when (terminal--csi-final-character-p character)
                        (return (terminal-decode-sequence
                                 (coerce buffer 'string)))))
                 finally (return :ignored)))
          (t
           (return :ignored)))))))

(-> terminal--read-bracketed-paste (stream-terminal) string)
(defun terminal--read-bracketed-paste (terminal)
  "Read one complete bracketed paste payload from TERMINAL."
  (let* ((stream (stream-terminal-input-stream terminal))
         (marker +terminal-bracketed-paste-end+)
         (buffer (make-array 128
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0)))
    (loop
      for character = (read-char stream nil nil)
      do (unless character
           (error 'terminal-error
                  :message "Terminal input ended inside a bracketed paste."
                  :operation ':read-input
                  :cause nil))
         (vector-push-extend character buffer)
         (when (and (>= (length buffer) (length marker))
                    (loop for marker-index below (length marker)
                          for buffer-index from (- (length buffer) (length marker))
                          always (char= (char buffer buffer-index)
                                        (char marker marker-index))))
           (setf (fill-pointer buffer) (- (length buffer) (length marker)))
           (return (coerce buffer 'string))))))

(-> terminal--character-event (character) t)
(defun terminal--character-event (character)
  "Return the semantic input event represented by CHARACTER."
  (case (char-code character)
    (1 :home)
    (3 :interrupt)
    (4 :end-of-input)
    (5 :end)
    ((8 127) :backspace)
    (9 (list :insert "    "))
    ((10 13) :submit)
    (t
     (if (>= (char-code character) 32)
         (list :insert (string character))
         :ignored))))


;;;; -- Terminal Methods --

(defmethod terminal--write ((terminal stream-terminal) (text string))
  "Write trusted TEXT to TERMINAL's output stream."
  (write-string text (stream-terminal-output-stream terminal))
  nil)

(defmethod terminal-flush ((terminal stream-terminal))
  "Flush TERMINAL's output stream."
  (finish-output (stream-terminal-output-stream terminal))
  nil)

(-> terminal--terminal-mode-or-nil (stream-terminal) t)
(defun terminal--terminal-mode-or-nil (terminal)
  "Return TERMINAL's termios value, or NIL when its input is not a TTY."
  (handler-case
      (sb-posix:tcgetattr (stream-terminal-input-file-descriptor terminal))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enotty)
          nil
          (error 'terminal-error
                 :message "Could not inspect terminal input mode."
                 :operation ':start
                 :cause condition)))))

(-> terminal--configure-input-mode (sb-posix:termios) sb-posix:termios)
(defun terminal--configure-input-mode (mode)
  "Configure MODE for noncanonical, no-echo, application-managed input."
  (setf (sb-posix:termios-lflag mode)
        (logandc2 (sb-posix:termios-lflag mode)
                  (logior sb-posix:icanon
                          sb-posix:echo
                          sb-posix:isig
                          sb-posix:iexten))
        (sb-posix:termios-iflag mode)
        (logandc2 (sb-posix:termios-iflag mode) sb-posix:ixon))
  (let ((control-characters (sb-posix:termios-cc mode)))
    (setf (aref control-characters sb-posix:vmin) 1
          (aref control-characters sb-posix:vtime) 0))
  mode)

(defmethod terminal-start ((terminal stream-terminal))
  "Start TERMINAL in noncanonical mode, or select its non-TTY fallback."
  (when (terminal-started-p terminal)
    (return-from terminal-start terminal))
  (let ((saved-mode (terminal--terminal-mode-or-nil terminal)))
    (if (null saved-mode)
        (setf (terminal-interactive-p terminal) nil
              (terminal-started-p terminal) t)
        (handler-case
            (let ((active-mode
                    (terminal--configure-input-mode
                     (sb-posix:tcgetattr
                      (stream-terminal-input-file-descriptor terminal)))))
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               active-mode)
              (setf (stream-terminal-saved-terminal-mode terminal) saved-mode
                    (terminal-interactive-p terminal) t
                    (terminal-started-p terminal) t)
              (terminal--write terminal +terminal-bracketed-paste-enable+)
              (terminal-flush terminal))
          (error (condition)
            (ignore-errors
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               saved-mode))
            (setf (stream-terminal-saved-terminal-mode terminal) nil
                  (terminal-interactive-p terminal) nil
                  (terminal-started-p terminal) nil)
            (error 'terminal-error
                   :message "Could not enter terminal input mode."
                   :operation ':start
                   :cause condition)))))
  terminal)

(defmethod terminal-stop ((terminal stream-terminal))
  "Stop TERMINAL and restore the exact termios value captured at startup."
  (unless (terminal-started-p terminal)
    (return-from terminal-stop terminal))
  (let ((failure nil)
        (saved-mode (stream-terminal-saved-terminal-mode terminal)))
    (when (terminal-interactive-p terminal)
      (handler-case
          (progn
            (terminal--write terminal +terminal-bracketed-paste-disable+)
            (terminal-flush terminal))
        (error (condition)
          (setf failure condition)))
      (when saved-mode
        (handler-case
            (sb-posix:tcsetattr
             (stream-terminal-input-file-descriptor terminal)
             sb-posix:tcsanow
             saved-mode)
          (error (condition)
            (unless failure
              (setf failure condition))))))
    (setf (stream-terminal-saved-terminal-mode terminal) nil
          (terminal-interactive-p terminal) nil
          (terminal-started-p terminal) nil)
    (when failure
      (error 'terminal-error
             :message "Could not completely restore terminal state."
             :operation ':stop
             :cause failure)))
  terminal)

(defmethod terminal-read-event ((terminal stream-terminal))
  "Read one key, escape sequence, paste, or fallback line from TERMINAL."
  (if (terminal-interactive-p terminal)
      (let ((character
              (read-char (stream-terminal-input-stream terminal) nil nil)))
        (cond
          ((null character)
           :end-of-input)
          ((char= character +terminal-escape-character+)
           (let ((event (terminal--read-escape-sequence terminal)))
             (if (eq event :paste-start)
                 (list :paste (terminal--read-bracketed-paste terminal))
                 event)))
          (t
           (terminal--character-event character))))
      (let ((line (read-line (stream-terminal-input-stream terminal) nil nil)))
        (if line
            (list :line line)
            :end-of-input))))

;; Generic functions require broad FTYPEs so downstream terminal adapters can
;; add methods without SBCL replacing a class-restricted proclamation.
(-> terminal-start (t) *)
(-> terminal-stop (t) *)
(-> terminal-read-event (t) *)
(-> terminal--write (t t) *)
(-> terminal-flush (t) *)


;;;; -- Public Construction and Editor Operations --

(-> stream-terminal-create
    (&key
     (:input-stream stream)
     (:output-stream stream)
     (:input-file-descriptor integer)
     (:columns integer))
    stream-terminal)
(defun stream-terminal-create
    (&key
       (input-stream *standard-input*)
       (output-stream *standard-output*)
       (input-file-descriptor 0)
       (columns +terminal-default-columns+))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :input-stream input-stream
                 :output-stream output-stream
                 :input-file-descriptor input-file-descriptor
                 :columns (if (plusp columns)
                              columns
                              +terminal-default-columns+)))

(-> line-editor-create (&key (:history-limit integer)) line-editor)
(defun line-editor-create (&key (history-limit +terminal-history-limit+))
  "Create an empty line editor retaining at most HISTORY-LIMIT submissions."
  (unless (plusp history-limit)
    (error 'terminal-error
           :message "The terminal history limit must be positive."
           :operation ':create-editor
           :cause nil))
  (make-instance 'line-editor :history-limit history-limit))

(defun line-editor-set-text (editor text)
  "Replace EDITOR input with sanitized TEXT and move its cursor to the end."
  (let ((safe-text (terminal-sanitize-text text)))
    (line-editor--leave-history editor)
    (line-editor--set-state editor safe-text (length safe-text))))

(defun line-editor-clear (editor)
  "Clear EDITOR input and leave history navigation."
  (line-editor--leave-history editor)
  (line-editor--set-state editor "" 0))

(defun line-editor-add-history (editor text)
  "Add non-empty TEXT to EDITOR history unless it duplicates the newest entry."
  (when (and (non-empty-string-p text)
             (not (and (line-editor-history editor)
                       (string= text (first (line-editor-history editor))))))
    (push text (slot-value editor 'history))
    (when (> (length (line-editor-history editor))
             (line-editor-history-limit editor))
      (setf (slot-value editor 'history)
            (subseq (line-editor-history editor)
                    0
                    (line-editor-history-limit editor)))))
  editor)

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string))
    terminal-ui)
(defun terminal-ui-create (&key terminal editor (prompt "> "))
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (make-instance 'terminal-ui
                 :terminal terminal
                 :editor (or editor (line-editor-create))
                 :prompt prompt))


;;;; -- Live Region Mechanics --

(-> terminal--cursor-up (terminal integer) null)
(defun terminal--cursor-up (terminal rows)
  "Move TERMINAL upward by ROWS within the bounded live region."
  (when (plusp rows)
    (terminal--write terminal
                     (format nil "~C[~:DA"
                             +terminal-escape-character+
                             rows)))
  nil)

(-> terminal--cursor-right (terminal integer) null)
(defun terminal--cursor-right (terminal columns)
  "Move TERMINAL right by COLUMNS within the current prompt row."
  (when (plusp columns)
    (terminal--write terminal
                     (format nil "~C[~:DC"
                             +terminal-escape-character+
                             columns)))
  nil)

(-> terminal-ui--clear-live (terminal-ui) null)
(defun terminal-ui--clear-live (ui)
  "Erase only UI's currently rendered status and prompt rows."
  (when (and (terminal-interactive-p (terminal-ui-terminal ui))
             (terminal-ui-live-rendered-p ui))
    (let ((terminal (terminal-ui-terminal ui)))
      (terminal--write terminal (string #\Return))
      (terminal--write terminal +terminal-erase-line+)
      (when (terminal-ui-rendered-status-p ui)
        (terminal--cursor-up terminal 1)
        (terminal--write terminal (string #\Return))
        (terminal--write terminal +terminal-erase-line+)))
    (setf (terminal-ui-live-rendered-p ui) nil
          (terminal-ui-rendered-status-p ui) nil))
  nil)

(-> terminal--write-newline (terminal) null)
(defun terminal--write-newline (terminal)
  "Write a line break that returns to column zero on interactive terminals."
  (terminal--write terminal
                   (if (terminal-interactive-p terminal)
                       (format nil "~C~C" #\Return #\Newline)
                       (string #\Newline)))
  nil)

(-> terminal--write-safe-text (terminal string) null)
(defun terminal--write-safe-text (terminal text)
  "Write sanitized TEXT while making its line endings terminal-safe."
  (let ((line-start 0))
    (loop for newline = (position #\Newline text :start line-start)
          while newline
          do (terminal--write terminal (subseq text line-start newline))
             (terminal--write-newline terminal)
             (setf line-start (1+ newline))
          finally (terminal--write terminal (subseq text line-start))))
  nil)

(-> terminal-ui--paint-live (terminal-ui) null)
(defun terminal-ui--paint-live (ui)
  "Render UI's optional status and one-row editor without touching scrollback."
  (let ((terminal (terminal-ui-terminal ui)))
    (when (terminal-interactive-p terminal)
      (let ((status (terminal-ui-status ui)))
        (when status
          (terminal--write terminal
                           (terminal--prefix-within-width
                            (terminal-sanitize-text status :single-line-p t)
                            (terminal-columns terminal)))
          (terminal--write-newline terminal)))
      (multiple-value-bind (row cursor-column)
          (line-editor-render (terminal-ui-editor ui)
                              (terminal-ui-prompt ui)
                              (terminal-columns terminal))
        (terminal--write terminal (string #\Return))
        (terminal--write terminal +terminal-erase-line+)
        (terminal--write terminal row)
        (terminal--write terminal (string #\Return))
        (terminal--cursor-right terminal cursor-column))
      (setf (terminal-ui-live-rendered-p ui) t
            (terminal-ui-rendered-status-p ui)
            (not (null (terminal-ui-status ui))))
      (terminal-flush terminal)))
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Clear and repaint only UI's bounded live region."
  (terminal-ui--clear-live ui)
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--write-finalized (terminal-ui string) null)
(defun terminal-ui--write-finalized (ui text)
  "Write sanitized finalized TEXT once at the live region's former position."
  (let* ((terminal (terminal-ui-terminal ui))
         (safe-text (terminal-sanitize-text text)))
    (terminal--write-safe-text terminal safe-text)
    (unless (and (plusp (length safe-text))
                 (char= (char safe-text (1- (length safe-text))) #\Newline))
      (terminal--write-newline terminal))
    (terminal-flush terminal))
  nil)


;;;; -- Public UI Lifecycle and Events --

(-> terminal-ui-start (terminal-ui) terminal-ui)
(defun terminal-ui-start (ui)
  "Start UI on the primary screen and render its bounded live region."
  (unless (terminal-ui-started-p ui)
    (terminal-start (terminal-ui-terminal ui))
    (setf (terminal-ui-started-p ui) t)
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-stop (terminal-ui) terminal-ui)
(defun terminal-ui-stop (ui)
  "Erase UI's unfinished rows and restore its terminal even after partial startup."
  (unwind-protect
       (when (terminal-ui-started-p ui)
         (terminal-ui--clear-live ui)
         (terminal-flush (terminal-ui-terminal ui)))
    (setf (terminal-ui-started-p ui) nil)
    (terminal-stop (terminal-ui-terminal ui)))
  ui)

(defmacro with-terminal-ui ((variable ui-form) &body body)
  "Bind VARIABLE to UI-FORM, run BODY, and always restore its terminal state."
  `(let ((,variable ,ui-form))
     (unwind-protect
          (progn
            (terminal-ui-start ,variable)
            (locally
              ,@body))
       (terminal-ui-stop ,variable))))

(-> terminal-ui-append-finalized (terminal-ui t string) boolean)
(defun terminal-ui-append-finalized (ui identifier text)
  "Append finalized transcript TEXT once for IDENTIFIER and return true when emitted."
  (block nil
    (when (gethash identifier (terminal-ui-finalized-identifiers ui))
      (return nil))
    (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
    (terminal-ui--clear-live ui)
    (terminal-ui--write-finalized ui text)
    (terminal-ui--paint-live ui)
    t))

(-> terminal-ui-set-status (terminal-ui (option string)) terminal-ui)
(defun terminal-ui-set-status (ui status)
  "Replace UI's unfinished one-row STATUS and repaint only the live region."
  (let ((safe-status (and status
                          (terminal-sanitize-text status :single-line-p t))))
    (unless (equal safe-status (terminal-ui-status ui))
      (terminal-ui--clear-live ui)
      (setf (terminal-ui-status ui) safe-status)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-resize (terminal-ui integer) terminal-ui)
(defun terminal-ui-resize (ui columns)
  "Set UI terminal width to positive COLUMNS and repaint only unfinished rows."
  (let ((new-columns (max 1 columns)))
    (unless (= new-columns (terminal-columns (terminal-ui-terminal ui)))
      (terminal-ui--clear-live ui)
      (setf (terminal-columns (terminal-ui-terminal ui)) new-columns)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-read-event (terminal-ui) t)
(defun terminal-ui-read-event (ui)
  "Read one semantic input event for UI without emitting fallback prompt controls."
  (terminal-read-event (terminal-ui-terminal ui)))

(-> terminal-ui-process-event
    (terminal-ui t)
    (values keyword (option string)))
(defun terminal-ui-process-event (ui event)
  "Apply EVENT to UI's editor, repaint live rows, and return its action and payload."
  (multiple-value-bind (action payload)
      (line-editor-handle-event (terminal-ui-editor ui) event)
    (when (member action '(:changed :cleared :submit))
      (terminal-ui--repaint-live ui))
    (values action payload)))
