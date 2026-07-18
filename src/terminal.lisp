(in-package #:autolith)

;;;; -- Terminal Constants --

(define-constant +terminal-default-columns+ 80
  :documentation "The fallback terminal width when no positive width is supplied.")

(define-constant +terminal-default-rows+ 24
  :documentation "The fallback terminal height when no positive height is supplied.")

(define-constant +terminal-history-limit+ 100
  :documentation "The maximum number of submitted inputs retained by a line editor.")

(define-constant +terminal-ui-visible-completions+ 6
  :documentation "The maximum number of candidate rows painted at once.")

(define-constant +terminal-escape-character+ (code-char 27)
  :test #'char=
  :documentation "The ASCII escape character used by trusted terminal controls.")

(define-constant +terminal-bracketed-paste-enable+
  (format nil "~C[?2004h" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that enables bracketed paste mode.")

(define-constant +terminal-bracketed-paste-disable+
  (format nil "~C[?2004l" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that disables bracketed paste mode.")


;;;; -- Terminal Objects --

(defclass terminal ()
  ((rows
    :initarg :rows
    :initform +terminal-default-rows+
    :accessor terminal-rows
    :type (integer 1)
    :documentation "The current terminal height in character cells.")
   (columns
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

(defclass terminal-ui ()
  ((lock
    :initform (make-recursive-lock "Autolith terminal UI")
    :reader terminal-ui-lock
    :type t
    :documentation "The recursive lock serializing editor state and terminal writes.")
   (terminal
    :initarg :terminal
    :reader terminal-ui-terminal
    :type terminal
    :documentation "The primary-screen terminal transport.")
   (editor
    :initarg :editor
    :reader terminal-ui-editor
    :type line-editor
    :documentation "The Unicode-aware multiline user input editor.")
   (live-region
    :initarg :live-region
    :reader terminal-ui-live-region
    :type live-region
    :documentation "Clinedi region anchoring unfinished content below scrollback.")
   (prompt
    :initarg :prompt
    :reader terminal-ui-prompt
    :type string
    :documentation "The untrusted-text-safe prompt prefix.")
   (placeholder
    :initarg :placeholder
    :initform ""
    :reader terminal-ui-placeholder
    :type string
    :documentation "The dim hint shown on the prompt row while input is empty.")
   (completions
    :initarg :completions
    :initform nil
    :reader terminal-ui-completions
    :type list
    :documentation "Completion entries offered while typing an interactive command.")
   (completion-selector
    :initarg :completion-selector
    :reader terminal-ui-completion-selector
    :type selector
    :documentation "Clinedi navigation state for matching command completions.")
   (completion-active-p
    :initform nil
    :accessor terminal-ui-completion-active-p
    :type boolean
    :documentation "Whether arrows and Tab are choosing among completion candidates.")
   (completion-prefix
    :initform nil
    :accessor terminal-ui-completion-prefix
    :type (option string)
    :documentation "Input restored when active completion selection is cancelled.")
   (selector
    :initform nil
    :accessor terminal-ui-selector
    :type (option selector)
    :documentation "Clinedi navigation state for the active modal picker.")
   (selector-title
    :initform nil
    :accessor terminal-ui-selector-title
    :type (option string)
    :documentation "The application-owned title for the active modal picker.")
   (status
    :initform nil
    :accessor terminal-ui-status
    :type (option string)
    :documentation "The optional unfinished activity shown above the prompt.")
   (status-started-at
    :initform nil
    :accessor terminal-ui-status-started-at
    :type (option real)
    :documentation "The monotonic time at which the current activity phase began.")
   (status-progress-at
    :initform nil
    :accessor terminal-ui-status-progress-at
    :type (option real)
    :documentation "The monotonic time of the newest progress within the activity phase.")
   (status-rendered-signature
    :initform nil
    :accessor terminal-ui-status-rendered-signature
    :type list
    :documentation "The elapsed status values used by the newest live-region paint.")
   (clock-function
    :initarg :clock-function
    :initform (lambda ()
                (/ (get-internal-real-time)
                   (coerce internal-time-units-per-second 'double-float)))
    :reader terminal-ui-clock-function
    :type function
    :documentation "The injected monotonic clock function returning seconds.")
   (preview-rows
    :initform nil
    :accessor terminal-ui-preview-rows
    :type list
    :documentation "Transient styled rows shown in the live region, never scrollback.")
   (queued-input-previews
    :initform nil
    :accessor terminal-ui-queued-input-previews
    :type list
    :documentation "Sanitized queued follow-up text shown in the live region.")
   (steering-input-previews
    :initform nil
    :accessor terminal-ui-steering-input-previews
    :type list
    :documentation "Sanitized steering text shown in the live region.")
   (stream-tail
    :initform nil
    :accessor terminal-ui-stream-tail
    :type (or null string list)
    :documentation "The unfinished streamed row, as text or styled spans, continuing the transcript block above.")
   (finalized-identifiers
    :initform (make-hash-table :test #'equal)
    :reader terminal-ui-finalized-identifiers
    :type hash-table
    :documentation "Identifiers whose finalized transcript text was already emitted.")
   (started-p
    :initform nil
    :accessor terminal-ui-started-p
    :type boolean
    :documentation "Whether the UI lifecycle has started."))
  (:documentation
   "A scrollback-preserving UI with immutable transcript output and a bounded live region."))


;;;; -- Terminal Conditions --

(define-condition terminal-error (autolith-error)
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

(-> terminal-input-ready-p (terminal) boolean)
(defgeneric terminal-input-ready-p (terminal)
  (:documentation "Return true when TERMINAL can read an event without blocking."))

(defmethod terminal-input-ready-p ((terminal terminal))
  "Assume application-provided TERMINAL transports have an event ready."
  (declare (ignore terminal))
  t)

(defgeneric terminal--write (terminal text)
  (:documentation "Write trusted renderer TEXT through the terminal transport."))

(defgeneric terminal-flush (terminal)
  (:documentation "Make all pending TERMINAL output visible."))
