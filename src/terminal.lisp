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
   (styled-p
    :initarg :styled-p
    :initform nil
    :accessor terminal-styled-p
    :type boolean
    :documentation "Whether trusted output may include color and emphasis controls.")
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

(-> terminal--wrap-line (string integer) list)
(defun terminal--wrap-line (line maximum-width)
  "Return newline-free LINE broken at spaces into rows of at most MAXIMUM-WIDTH cells."
  (let ((width (max 1 maximum-width))
        (segments nil)
        (start 0))
    (loop while (< start (length line))
          do (let ((used 0)
                   (end start)
                   (break-position nil))
               (loop while (< end (length line))
                     for character = (char line end)
                     for character-width = (terminal--character-width character)
                     while (<= (+ used character-width) width)
                     do (incf used character-width)
                        (incf end)
                        (when (char= character #\Space)
                          (setf break-position end)))
               (cond
                 ((= end (length line))
                  (push (subseq line start end) segments)
                  (setf start end))
                 ((char= (char line end) #\Space)
                  (push (string-right-trim " " (subseq line start end)) segments)
                  (setf start end))
                 ((and break-position (> break-position start))
                  (push (string-right-trim " " (subseq line start break-position))
                        segments)
                  (setf start break-position))
                 (t
                  (let ((forced-end (max (1+ start) end)))
                    (push (subseq line start forced-end) segments)
                    (setf start forced-end))))
               (when (plusp start)
                 (loop while (and (< start (length line))
                                  (char= (char line start) #\Space))
                       do (incf start)))))
    (if segments
        (nreverse segments)
        (list ""))))

(-> terminal--wrap-text (string integer) list)
(defun terminal--wrap-text (text maximum-width)
  "Return sanitized TEXT as a list of rows at most MAXIMUM-WIDTH cells wide."
  (loop for line in (or (uiop:split-string text :separator '(#\Newline))
                        (list ""))
        append (terminal--wrap-line line maximum-width)))

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
