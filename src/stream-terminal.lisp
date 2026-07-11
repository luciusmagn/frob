(in-package #:frob)

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
              (terminal-styled-p terminal) nil
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
                    (terminal-styled-p terminal) (terminal-environment-styling-p)
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
          (terminal-styled-p terminal) nil
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


;;;; -- Public Construction --

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
