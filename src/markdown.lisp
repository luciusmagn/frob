(in-package #:frob)

;;;; -- Markdown Renderer Object --

(defclass markdown-renderer ()
  ((width
    :initarg :width
    :reader markdown-renderer-width
    :type (integer 24)
    :documentation "The total cell budget for one rendered row including indents.")
   (code-open-p
    :initform nil
    :accessor markdown-renderer-code-open-p
    :type boolean
    :documentation "Whether rendering is inside a fenced code block.")
   (code-line-number
   :initform 1
    :accessor markdown-renderer-code-line-number
    :type (integer 1)
    :documentation "The gutter number given to the next fenced code line."))
  (:documentation
   "A line-oriented renderer turning a restrained markdown subset into styled rows."))

(-> markdown-renderer-create (&key (:width integer)) markdown-renderer)
(defun markdown-renderer-create (&key (width 80))
  "Create a markdown renderer wrapping rendered rows within WIDTH cells."
  (make-instance 'markdown-renderer :width (max 24 width)))

(-> markdown--copy-renderer (markdown-renderer) markdown-renderer)
(defun markdown--copy-renderer (renderer)
  "Return an independent copy of RENDERER for speculative presentation."
  (let ((copy (make-instance 'markdown-renderer
                             :width (markdown-renderer-width renderer))))
    (setf (markdown-renderer-code-open-p copy)
          (markdown-renderer-code-open-p renderer)
          (markdown-renderer-code-line-number copy)
          (markdown-renderer-code-line-number renderer))
    copy))


;;;; -- Inline Emphasis Spans --

(-> markdown--inline-close-position (string integer string) (option integer))
(defun markdown--inline-close-position (text start marker)
  "Return the first valid closing MARKER position in TEXT at or after START."
  (loop for position = (search marker text :start2 start)
          then (search marker text :start2 (1+ position))
        while position
        when (and (plusp position)
                  (not (char= (char text (1- position)) #\Space)))
          return position))

(-> markdown--inline-openable-p (string integer string) boolean)
(defun markdown--inline-openable-p (text index marker)
  "Return true when MARKER at INDEX opens an emphasis span that also closes."
  (let ((after (+ index (length marker))))
    (and (< after (length text))
         (not (char= (char text after) #\Space))
         (not (null (markdown--inline-close-position text after marker))))))

(-> markdown--parse-inline (string) (values string vector))
(defun markdown--parse-inline (text)
  "Return TEXT without inline markers plus per-character styles.

Recognizes `code`, **strong**, and *emphasis* spans whose delimiters pair
within TEXT. Unpaired delimiters stay literal."
  (let ((rendered (make-array (length text)
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0))
        (styles (make-array (length text) :adjustable t :fill-pointer 0))
        (mode ':plain)
        (index 0))
    (labels ((emit ()
               "Copy the current source character with the active style."
               (vector-push-extend (char text index) rendered)
               (vector-push-extend mode styles)
               (incf index))

             (marker-p (marker)
               "Return true when MARKER occurs at the current index."
               (let ((end (+ index (length marker))))
                 (and (<= end (length text))
                      (string= text marker :start1 index :end1 end))))

             (closable-p ()
               "Return true when a closing delimiter is valid at this index."
               (and (plusp index)
                    (not (char= (char text (1- index)) #\Space)))))
      (loop while (< index (length text))
            do (cond
                 ((eq mode ':code)
                  (if (char= (char text index) #\`)
                      (progn
                        (setf mode ':plain)
                        (incf index))
                      (emit)))
                 ((and (char= (char text index) #\`)
                       (position #\` text :start (1+ index)))
                  (setf mode ':code)
                  (incf index))
                 ((eq mode ':strong)
                  (if (and (marker-p "**") (closable-p))
                      (progn
                        (setf mode ':plain)
                        (incf index 2))
                      (emit)))
                 ((eq mode ':emphasis)
                  (if (and (char= (char text index) #\*) (closable-p))
                      (progn
                        (setf mode ':plain)
                        (incf index))
                      (emit)))
                 ((and (marker-p "**")
                       (markdown--inline-openable-p text index "**"))
                  (setf mode ':strong)
                  (incf index 2))
                 ((and (char= (char text index) #\*)
                       (not (marker-p "**"))
                       (markdown--inline-openable-p text index "*"))
                  (setf mode ':emphasis)
                  (incf index))
                 (t
                  (emit)))))
    (values (coerce rendered 'string) styles)))

(-> markdown--style-runs
    (string vector &key (:start integer) (:end integer))
    list)
(defun markdown--style-runs (rendered styles &key (start 0) (end (length rendered)))
  "Return spans for RENDERED between START and END grouped by equal style."
  (let ((spans nil)
        (run-start start))
    (loop for position from start below end
          when (and (> position run-start)
                    (not (eq (aref styles position)
                             (aref styles (1- position)))))
            do (push (terminal-span (aref styles (1- position))
                                    (subseq rendered run-start position))
                     spans)
               (setf run-start position))
    (when (< run-start end)
      (push (terminal-span (aref styles (1- end))
                           (subseq rendered run-start end))
            spans))
    (nreverse spans)))


;;;; -- Line Classification --

(-> markdown--fence-line-p (string) (values boolean string))
(defun markdown--fence-line-p (line)
  "Return whether LINE is a code fence, plus its trimmed language tag."
  (if (uiop:string-prefix-p "```" line)
      (values t (string-trim " `" (subseq line 3)))
      (values nil "")))

(-> markdown--leading-spaces (string) integer)
(defun markdown--leading-spaces (line)
  "Return the number of leading space characters in LINE."
  (or (position #\Space line :test-not #'char=)
      (length line)))

(-> markdown--bullet-content (string) (values (option string) integer))
(defun markdown--bullet-content (line)
  "Return LINE's bullet item content and leading indent, or NIL and zero."
  (let ((lead (markdown--leading-spaces line)))
    (if (and (< (1+ lead) (length line))
             (find (char line lead) "-*+")
             (char= (char line (1+ lead)) #\Space))
        (values (subseq line (+ lead 2)) lead)
        (values nil 0))))

(-> markdown--numbered-content (string) (values (option string) integer string))
(defun markdown--numbered-content (line)
  "Return LINE's numbered item content, leading indent, and marker text."
  (block nil
    (let* ((lead (markdown--leading-spaces line))
           (digits-end (loop for position from lead below (length line)
                             while (digit-char-p (char line position))
                             finally (return position))))
      (unless (and (> digits-end lead)
                   (<= (- digits-end lead) 3)
                   (< (1+ digits-end) (length line))
                   (find (char line digits-end) ".)")
                   (char= (char line (1+ digits-end)) #\Space))
        (return (values nil 0 "")))
      (values (subseq line (+ digits-end 2))
              lead
              (subseq line lead (1+ digits-end))))))

(-> markdown--line-layout (string) (values string list list))
(defun markdown--line-layout (line)
  "Return LINE's inline content plus its first-row and continuation prefixes."
  (multiple-value-bind (bullet-content bullet-lead)
      (markdown--bullet-content line)
    (multiple-value-bind (numbered-content numbered-lead numbered-marker)
        (markdown--numbered-content line)
      (cond
        (bullet-content
         (let ((lead (make-string (+ 2 bullet-lead) :initial-element #\Space)))
           (values bullet-content
                   (list (terminal-span ':plain lead)
                         (terminal-span ':brand "• "))
                   (list (terminal-span ':plain
                                        (concatenate 'string lead "  "))))))
        (numbered-content
         (let ((lead (make-string (+ 2 numbered-lead) :initial-element #\Space))
               (marker (concatenate 'string numbered-marker " ")))
           (values numbered-content
                   (list (terminal-span ':plain lead)
                         (terminal-span ':brand marker))
                   (list (terminal-span
                          ':plain
                          (concatenate 'string
                                       lead
                                       (make-string (length marker)
                                                    :initial-element #\Space)))))))
        (t
         (values line
                 (list (terminal-span ':plain "  "))
                 (list (terminal-span ':plain "  "))))))))


;;;; -- Row Assembly --

(-> markdown--code-prefixes (markdown-renderer) (values list list))
(defun markdown--code-prefixes (renderer)
  "Return RENDERER's numbered first-row and unnumbered continuation gutters."
  (values (list (terminal-span
                 ':dim
                 (format nil "  ~3D │ "
                         (markdown-renderer-code-line-number renderer))))
          (list (terminal-span ':dim "      │ "))))

(-> markdown--wrapped-rows
    (markdown-renderer string
     &key (:first-prefix list) (:continuation-prefix list))
    list)
(defun markdown--wrapped-rows
    (renderer content &key first-prefix continuation-prefix)
  "Return CONTENT as wrapped, prefixed, styled transcript rows."
  (multiple-value-bind (rendered styles)
      (markdown--parse-inline content)
    (let* ((prefix-width (max (terminal--spans-width first-prefix)
                              (terminal--spans-width continuation-prefix)))
           (content-width (max 8 (- (markdown-renderer-width renderer)
                                    prefix-width)))
           (rows nil)
           (cursor 0))
      (loop for row-text in (terminal--wrap-text rendered content-width)
            for first-row-p = t then nil
            do (let ((start (if (zerop (length row-text))
                                cursor
                                (search row-text rendered :start2 cursor))))
                 (push (append (if first-row-p
                                   first-prefix
                                   continuation-prefix)
                               (markdown--style-runs rendered
                                                     styles
                                                     :start start
                                                     :end (+ start
                                                             (length row-text))))
                       rows)
                 (setf cursor (+ start (length row-text)))))
      (nreverse rows))))

(-> markdown--code-rows (markdown-renderer string) list)
(defun markdown--code-rows (renderer line)
  "Return fenced code LINE as gutter-numbered wrapped rows."
  (multiple-value-bind (first-prefix continuation-prefix)
      (markdown--code-prefixes renderer)
    (let* ((content-width (max 8 (- (markdown-renderer-width renderer) 8)))
           (rows nil)
           (cursor 0))
      (loop for row-text in (terminal--wrap-text line content-width)
            for first-row-p = t then nil
            do (let ((start (if (zerop (length row-text))
                                cursor
                                (search row-text line :start2 cursor))))
                 (push (append (if first-row-p
                                   first-prefix
                                   continuation-prefix)
                               (when (plusp (length row-text))
                                 (list (terminal-span ':plain row-text))))
                       rows)
                 (setf cursor (+ start (length row-text)))))
      (incf (markdown-renderer-code-line-number renderer))
      (nreverse rows))))


;;;; -- Public Rendering Operations --

(-> markdown-render-line (markdown-renderer string) list)
(defun markdown-render-line (renderer line)
  "Return sanitized logical LINE as styled transcript rows, updating RENDERER."
  (multiple-value-bind (fence-p language)
      (markdown--fence-line-p line)
    (cond
      ((and (markdown-renderer-code-open-p renderer) fence-p)
       (setf (markdown-renderer-code-open-p renderer) nil)
       (list (list (terminal-span ':dim "  ```"))))
      ((markdown-renderer-code-open-p renderer)
       (markdown--code-rows renderer line))
      (fence-p
       (setf (markdown-renderer-code-open-p renderer) t
             (markdown-renderer-code-line-number renderer) 1)
       (list (list (terminal-span ':dim (format nil "  ```~A" language)))))
      ((zerop (length (string-trim " " line)))
       (list nil))
      (t
       (multiple-value-bind (content first-prefix continuation-prefix)
           (markdown--line-layout line)
         (markdown--wrapped-rows renderer
                                 content
                                 :first-prefix first-prefix
                                 :continuation-prefix continuation-prefix))))))

(-> markdown-render-partial
    (markdown-renderer string)
    (values list list string))
(defun markdown-render-partial (renderer partial)
  "Return no committed rows, PARTIAL's live tail, and all retained source.

An unfinished logical line remains speculative even after it visually wraps.
Later chunks may close inline delimiters and change every wrapped row, so only
newline-terminated lines may become immutable transcript output."
  (let* ((preview-renderer (markdown--copy-renderer renderer))
         (preview-rows (markdown-render-line preview-renderer partial)))
    (values nil (first (last preview-rows)) partial)))
