(in-package #:frob)

;;;; -- Semantic Terminal Styles --

(deftype terminal-style ()
  "A semantic terminal style resolved to color and emphasis by the renderer."
  '(member :plain :brand :user :tool :success :failure :notice :dim :selected))

(define-constant +terminal-style-table+
  '((:brand    . "1;35")
    (:user     . "1;36")
    (:tool     . "1;33")
    (:success  . "32")
    (:failure  . "1;31")
    (:notice   . "33")
    (:dim      . "2")
    (:selected . "7"))
  :test #'equal
  :documentation "Select-graphic-rendition parameters for each semantic style.")

(define-constant +terminal-style-reset+
  (format nil "~C[0m" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that restores default terminal rendition.")

(-> terminal-style-sequence (terminal-style) (option string))
(defun terminal-style-sequence (style)
  "Return STYLE's trusted rendition control, or NIL when STYLE is plain."
  (let ((parameters (rest (assoc style +terminal-style-table+))))
    (and parameters
         (format nil "~C[~Am" +terminal-escape-character+ parameters))))

(-> terminal-environment-styling-p () boolean)
(defun terminal-environment-styling-p ()
  "Return true when the process environment permits color and emphasis output."
  (and (not (non-empty-string-p (uiop:getenv "NO_COLOR")))
       (not (string= (or (uiop:getenv "TERM") "") "dumb"))))


;;;; -- Styled Spans --

(-> terminal-span-p (t) boolean)
(defun terminal-span-p (value)
  "Return true when VALUE pairs a known terminal style with untrusted text."
  (and (consp value)
       (typep (first value) 'terminal-style)
       (stringp (rest value))))

(-> terminal-span (terminal-style string) cons)
(defun terminal-span (style text)
  "Return one styled span pairing STYLE with untrusted TEXT."
  (cons style text))

(-> terminal-span-style (cons) terminal-style)
(defun terminal-span-style (span)
  "Return SPAN's semantic style."
  (first span))

(-> terminal-span-text (cons) string)
(defun terminal-span-text (span)
  "Return SPAN's untrusted text."
  (rest span))

(-> terminal-styled-text-p (t) boolean)
(defun terminal-styled-text-p (value)
  "Return true when VALUE is a proper list of styled spans."
  (loop for tail = value then (rest tail)
        while tail
        always (and (consp tail)
                    (terminal-span-p (first tail)))))

(deftype terminal-styled-text ()
  "A proper list of styled spans rendered in order."
  '(satisfies terminal-styled-text-p))


;;;; -- Single-Row Span Layout --

(-> terminal--spans-width (list) (integer 0))
(defun terminal--spans-width (spans)
  "Return the total single-row cell width of sanitized SPANS."
  (loop for span in spans
        sum (terminal--text-width
             (terminal-sanitize-text (terminal-span-text span)
                                     :single-line-p t))))

(-> terminal--clip-spans (list integer) list)
(defun terminal--clip-spans (spans maximum-width)
  "Return single-row SPANS sanitized and clipped to at most MAXIMUM-WIDTH cells."
  (let ((remaining (max 0 maximum-width))
        (clipped nil))
    (dolist (span spans (nreverse clipped))
      (when (plusp remaining)
        (let* ((text (terminal-sanitize-text (terminal-span-text span)
                                             :single-line-p t))
               (visible (terminal--prefix-within-width text remaining)))
          (when (plusp (length visible))
            (decf remaining (terminal--text-width visible))
            (push (terminal-span (terminal-span-style span) visible)
                  clipped)))))))
