(in-package #:autolith)

;;;; -- Tool Transcript Presentation --

(define-constant +application-tool-call-lines+ 8
  :documentation "The maximum tool input lines shown in the terminal transcript.")

(define-constant +application-tool-output-lines+ 12
  :documentation "The maximum tool output lines shown in the terminal transcript.")

(define-constant +application-tool-inspection-lines+ 24
  :documentation "The maximum introspection lines shown in the terminal transcript.")

(define-constant +application-tool-diff-hunks+ 3
  :documentation "The maximum replacement locations shown for one fs.edit call.")

(-> application--display-lines (string) list)
(defun application--display-lines (text)
  "Return sanitized logical lines from TEXT without trailing blank rows."
  (let ((trimmed (string-right-trim '(#\Newline #\Return) text)))
    (when (plusp (length trimmed))
      (mapcar (lambda (line)
                (sanitize-text (string-right-trim '(#\Return) line)
                               :single-line-p t))
              (uiop:split-string trimmed :separator '(#\Newline))))))

(-> application--preview-rows
    (string terminal-style integer &key (:gutter (option string)))
    list)
(defun application--preview-rows (text style limit &key gutter)
  "Return up to LIMIT styled rows from TEXT, followed by an omission row."
  (let ((lines (application--display-lines text)))
    (when lines
      (let* ((visible-count (min limit (length lines)))
             (omitted (- (length lines) visible-count)))
        (append
         (loop for line in (subseq lines 0 visible-count)
               collect (append (when gutter
                                 (list (terminal-span ':dim gutter)))
                               (list (terminal-span style line))))
         (when (plusp omitted)
           (list (list (terminal-span
                        ':dim
                        (format nil "… +~D more line~:P" omitted))))))))))

(-> application--tool-row-spans (application list) list)
(defun application--tool-row-spans (application row)
  "Return one indented, sanitized, width-bounded tool transcript ROW."
  (let* ((columns (terminal-columns
                   (terminal-ui-terminal (application-ui application))))
         (maximum-width (max 1 (1- columns)))
         (indented (append (list (terminal-span ':plain "  ")) row)))
    (if (<= (terminal--spans-width indented) maximum-width)
        indented
        (append (terminal--clip-spans indented (max 0 (1- maximum-width)))
                (list (terminal-span ':dim "…"))))))

(-> application--tool-entry
    (application &key (:style terminal-style) (:header string)
                 (:detail (option string)) (:rows list))
    list)
(defun application--tool-entry
    (application &key (style ':plain) (header "") detail rows)
  "Return a transcript header followed by concise styled tool ROWS."
  (append
   (application--transcript-entry application
                                  :style style
                                  :header header
                                  :detail detail)
   (loop for row in rows
         append (list (terminal-span ':plain (string #\Newline)))
         when row
           append (application--tool-row-spans application row))))

(-> application--tool-field-rows (application list) list)
(defun application--tool-field-rows (application fields)
  "Return aligned, wrapped rows for tool detail FIELDS.

Each field is a plist containing :LABEL, :VALUE, and an optional :STYLE."
  (let* ((safe-fields
           (loop for field in fields
                 collect (list :label
                               (sanitize-text (or (getf field :label) "")
                                              :single-line-p t)
                               :value
                               (sanitize-text (or (getf field :value) "")
                                              :single-line-p t)
                               :style (or (getf field :style) ':plain))))
         (available-width
           (max 0
                (- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))
                   3)))
         (column-widths
           (layout-column-widths
            (loop for field in safe-fields
                  collect (list (getf field :label) (getf field :value)))
            available-width
            :gap-width 2
            :minimum-widths '(1 4)))
         (label-width (or (first column-widths) 0))
         (value-width (or (second column-widths) 0)))
    (loop for field in safe-fields
          append
          (let ((value-rows
                  (if (plusp value-width)
                      (or (wrap-text (getf field :value) value-width)
                          (list ""))
                      (list ""))))
            (loop for value-row in value-rows
                  for first-p = t then nil
                  collect
                  (append
                   (list (terminal-span
                          ':dim
                          (layout-fit-text
                           (if first-p (getf field :label) "")
                           label-width)))
                   (when (plusp value-width)
                     (list (terminal-span ':plain "  ")
                           (terminal-span (getf field :style)
                                          value-row)))))))))

(-> application--tool-section-row (string) list)
(defun application--tool-section-row (label)
  "Return one dim tool transcript section heading row."
  (list (terminal-span ':dim label)))

(-> application--function-call-arguments (json-object) (option json-object))
(defun application--function-call-arguments (call)
  "Decode CALL's argument object, returning NIL when it is malformed."
  (let ((source (json-get call "arguments")))
    (when (non-empty-string-p source)
      (handler-case
          (let ((arguments (json-decode source)))
            (and (json-object-p arguments) arguments))
        (error ()
          nil)))))

(-> application--presentation-value (t) string)
(defun application--presentation-value (value)
  "Return a concise readable presentation of one JSON argument VALUE."
  (cond
    ((stringp value)
     value)
    ((vectorp value)
     (format nil "~{~A~^, ~}"
             (map 'list #'application--presentation-value value)))
    ((json-object-p value)
     (json-encode value))
    ((eq value false)
     "false")
    (t
     (bounded-string value :limit 500))))

(-> application--generic-argument-rows
    (application (option json-object))
    list)
(defun application--generic-argument-rows (application arguments)
  "Return readable field rows for generic tool ARGUMENTS."
  (when arguments
    (let ((fields nil)
          (sections nil))
      (loop for name in (sort (loop for key being the hash-keys of arguments
                                    collect key)
                              #'string<)
            for value = (json-get arguments name)
            for text = (application--presentation-value value)
            do (if (find #\Newline text)
                   (setf sections
                         (append sections
                                 (list (application--tool-section-row name))
                                 (application--preview-rows
                                  text ':code +application-tool-call-lines+
                                  :gutter "│ ")))
                   (setf fields
                         (append fields
                                 (list (list :label name
                                             :value text
                                             :style ':code))))))
      (append (application--tool-field-rows application fields)
              sections))))

(-> application--restart-call-rows ((option json-object)) list)
(defun application--restart-call-rows (arguments)
  "Return a separate restart selection area from tool ARGUMENTS."
  (let ((restart (and arguments (json-get arguments "restart")))
        (value (and arguments (json-get arguments "restart-value"))))
    (when (non-empty-string-p restart)
      (append
       (list nil
             (application--tool-section-row "restart")
             (list (terminal-span ':notice (format nil "│ ~A" restart))))
       (when (non-empty-string-p value)
         (append (list (application--tool-section-row "restart value"))
                 (application--preview-rows
                  value ':code +application-tool-call-lines+
                  :gutter "│ ")))))))

(-> application--generic-tool-call-entry (application json-object) list)
(defun application--generic-tool-call-entry (application call)
  "Return a readable fallback entry for a function CALL."
  (application--tool-entry
   application
   :style ':tool
   :header (format nil "▸ ~A" (function-call-canonical-name call))
   :rows (application--generic-argument-rows
          application
          (application--function-call-arguments call))))

(defmethod application-tool-call-entry
    ((tool tool) (application application) (call hash-table))
  "Present CALL using the generic readable argument layout."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))

(defmethod application-tool-call-entry
    ((tool null) (application application) (call hash-table))
  "Present an unregistered CALL using the generic readable argument layout."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))


;;; Tool call specializations

(-> application--lisp-call-entry (application json-object string) list)
(defun application--lisp-call-entry (application call argument-name)
  "Return a Lisp source preview for CALL's ARGUMENT-NAME."
  (let* ((arguments (application--function-call-arguments call))
         (source (or (and arguments (json-get arguments argument-name)) "")))
    (application--tool-entry
     application
     :style ':tool
     :header (format nil "▸ ~A" (function-call-canonical-name call))
     :rows (append
            (application--preview-rows
             (application--presentation-value source)
             ':code
             +application-tool-call-lines+
             :gutter "│ ")
            (application--restart-call-rows arguments)))))

(-> application--simple-call-entry (application json-object string) list)
(defun application--simple-call-entry (application call argument-name)
  "Return CALL with one concise ARGUMENT-NAME row."
  (let* ((arguments (application--function-call-arguments call))
         (value (and arguments (json-get arguments argument-name))))
    (application--tool-entry
     application
     :style ':tool
     :header (format nil "▸ ~A" (function-call-canonical-name call))
     :rows (when value
             (list (list (terminal-span
                          ':code
                          (application--presentation-value value))))))))

(defmethod application-tool-call-entry
    ((tool fs-read-tool) (application application) (call hash-table))
  "Present an fs.read request without exposing the file contents."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (path (or (and arguments (json-get arguments "path")) ""))
         (start (let ((value (and arguments
                                  (json-get arguments "start-line"))))
                  (if (integerp value) (max 1 value) 1)))
         (count (let ((value (and arguments
                                  (json-get arguments "line-count"))))
                  (if (integerp value)
                      (max 1 value)
                      +fs-read-default-line-count+))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ fs.read"
     :rows (list (list (terminal-span
                        ':code
                        (format nil "~A  lines ~D-~D"
                                path start (+ start count -1))))))))

(defmethod application-tool-call-entry
    ((tool fs-list-tool) (application application) (call hash-table))
  "Present an fs.list path without raw JSON."
  (declare (ignore tool))
  (application--simple-call-entry application call "path"))

(defmethod application-tool-call-entry
    ((tool fs-write-tool) (application application) (call hash-table))
  "Present an fs.write destination and content size without its full content."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (path (or (and arguments (json-get arguments "path")) ""))
         (content (and arguments (json-get arguments "content"))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ fs.write"
     :rows (append
            (list (list (terminal-span ':code path)))
            (application--tool-field-rows
             application
             (list (list :label "content"
                         :value (if (stringp content)
                                    (format nil "~:D character~:P"
                                            (length content))
                                    "unknown size"))))))))

(-> application--edit-common-prefix-length (vector vector) integer)
(defun application--edit-common-prefix-length (old-lines new-lines)
  "Return the number of equal leading lines in OLD-LINES and NEW-LINES."
  (loop for index below (min (length old-lines) (length new-lines))
        while (string= (aref old-lines index) (aref new-lines index))
        count t))

(-> application--edit-common-suffix-length (vector vector integer) integer)
(defun application--edit-common-suffix-length
    (old-lines new-lines prefix-length)
  "Return equal trailing lines after PREFIX-LENGTH without overlap."
  (let ((maximum (min (- (length old-lines) prefix-length)
                      (- (length new-lines) prefix-length))))
    (loop for offset from 1 to maximum
          while (string= (aref old-lines (- (length old-lines) offset))
                         (aref new-lines (- (length new-lines) offset)))
          count t)))

(-> application--edit-line-number-width
    (vector vector &key (:old-start-line (option integer))
                        (:new-start-line (option integer)))
    integer)
(defun application--edit-line-number-width
    (old-lines new-lines &key old-start-line new-start-line)
  "Return the display width needed for OLD-LINES and NEW-LINES numbers."
  (let ((largest
          (max (or (and old-start-line
                        (+ old-start-line (max 0 (1- (length old-lines)))))
                   0)
               (or (and new-start-line
                        (+ new-start-line (max 0 (1- (length new-lines)))))
                   0))))
    (max 1 (length (princ-to-string largest)))))

(-> application--edit-line-number-cell ((option integer) integer) string)
(defun application--edit-line-number-cell (line-number width)
  "Return LINE-NUMBER right aligned to WIDTH, or an empty cell."
  (if line-number
      (format nil "~V@A" width line-number)
      (make-string width :initial-element #\Space)))


(-> application--syntax-lines (string string) (option vector))
(defun application--syntax-lines (text path)
  "Return syntax-highlighted display lines for TEXT at PATH, or NIL."
  (let ((lines (application--display-lines text)))
    (when lines
      (let* ((source (format nil "~{~A~^~%~}" lines))
             (highlighted (syntax--highlight-lines source :pathname path)))
        (and highlighted
             (= (length highlighted) (length lines))
             highlighted)))))

(-> application--edit-line-row
    (keyword string &key (:width integer)
                         (:line-number (option integer))
                         (:content-spans (option list)))
    list)
(defun application--edit-line-row
    (kind text &key (width 1) line-number content-spans)
  "Return one numbered context, removed, or added diff row."
  (let ((style (ecase kind
                 (:context ':dim)
                 (:removed ':failure)
                 (:added ':success)))
        (marker (ecase kind
                  (:context " ")
                  (:removed "-")
                  (:added "+"))))
    (cons
     (terminal-span
      style
      (format nil "~A ~A │ "
              marker
              (application--edit-line-number-cell line-number width)))
     (or content-spans
         (list (terminal-span style text))))))

(-> application--edit-change-rows
    (vector keyword &key (:start-line (option integer)) (:width integer)
                         (:highlighted-lines (option vector)))
    list)
(defun application--edit-change-rows
    (lines kind &key start-line (width 1) highlighted-lines)
  "Return bounded numbered changed LINES of KIND."
  (let* ((visible-count (min +application-tool-call-lines+ (length lines)))
         (omitted (- (length lines) visible-count))
         (noun (ecase kind
                 (:removed "removed")
                 (:added "added"))))
    (append
     (loop for index below visible-count
           for line-number = (and start-line (+ start-line index))
           collect (application--edit-line-row
                    kind
                    (aref lines index)
                    :width width
                    :line-number line-number
                    :content-spans (and highlighted-lines
                                        (aref highlighted-lines index))))
     (when (plusp omitted)
       (let ((line-number (and start-line (+ start-line visible-count))))
         (list
          (application--edit-line-row
           kind
           (format nil "… +~D more ~A line~:P" omitted noun)
           :width width
           :line-number line-number)))))))

(-> application--edit-diff-rows
    (string string &key (:old-start-line (option integer))
                        (:new-start-line (option integer))
                        (:path (option string)))
    list)
(defun application--edit-diff-rows
    (old-text new-text &key old-start-line new-start-line path)
  "Return a bounded line-numbered diff between OLD-TEXT and NEW-TEXT."
  (let* ((old-lines (coerce (or (application--display-lines old-text) nil)
                            'vector))
         (new-lines (coerce (or (application--display-lines new-text) nil)
                            'vector))
         (old-highlighted (and path (application--syntax-lines old-text path)))
         (new-highlighted (and path (application--syntax-lines new-text path)))
         (prefix-length
           (application--edit-common-prefix-length old-lines new-lines))
         (suffix-length
           (application--edit-common-suffix-length old-lines
                                                   new-lines
                                                   prefix-length))
         (width (application--edit-line-number-width
                 old-lines
                 new-lines
                 :old-start-line old-start-line
                 :new-start-line new-start-line)))
    (if (and (= prefix-length (length old-lines))
             (= prefix-length (length new-lines)))
        (list (list (terminal-span ':dim "no textual change")))
        (let ((removed (subseq old-lines
                               prefix-length
                               (- (length old-lines) suffix-length)))
              (added (subseq new-lines
                             prefix-length
                             (- (length new-lines) suffix-length))))
          (append
           (when (plusp prefix-length)
             (list
              (application--edit-line-row
               ':context
               (aref old-lines (1- prefix-length))
               :width width
               :line-number (or (and new-start-line
                                     (+ new-start-line prefix-length -1))
                                (and old-start-line
                                     (+ old-start-line prefix-length -1)))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted (1- prefix-length)))
                   (and old-highlighted
                        (aref old-highlighted (1- prefix-length)))))))
           (application--edit-change-rows
            removed
            ':removed
            :start-line (and old-start-line
                             (+ old-start-line prefix-length))
            :width width
            :highlighted-lines
            (and old-highlighted
                 (subseq old-highlighted
                         prefix-length
                         (- (length old-lines) suffix-length))))
           (application--edit-change-rows
            added
            ':added
            :start-line (and new-start-line
                             (+ new-start-line prefix-length))
            :width width
            :highlighted-lines
            (and new-highlighted
                 (subseq new-highlighted
                         prefix-length
                         (- (length new-lines) suffix-length))))
           (when (plusp suffix-length)
             (list
              (application--edit-line-row
               ':context
               (aref old-lines (- (length old-lines) suffix-length))
               :width width
               :line-number
               (or (and new-start-line
                        (+ new-start-line
                           (- (length new-lines) suffix-length)))
                   (and old-start-line
                        (+ old-start-line
                           (- (length old-lines) suffix-length))))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted
                              (- (length new-lines) suffix-length)))
                   (and old-highlighted
                        (aref old-highlighted
                              (- (length old-lines) suffix-length))))))))))))

(-> application--edit-file-hunks
    (application string string &key (:new-text string) (:replace-all boolean))
    list)
(defun application--edit-file-hunks
    (application path old-text &key (new-text "") replace-all)
  "Return old and resulting start lines for OLD-TEXT occurrences in PATH."
  (block nil
    (unless (and (non-empty-string-p old-text)
                 (slot-boundp application 'configuration))
      (return nil))
    (let ((configuration (application-configuration application)))
      (unless (typep configuration 'configuration)
        (return nil))
      (handler-case
          (let* ((pathname (merge-pathnames
                            (pathname path)
                            (configuration-working-directory configuration)))
                 (content (and (probe-file pathname)
                               (not (uiop:directory-exists-p pathname))
                               (uiop:read-file-string pathname)))
                 (newline-delta (- (count #\Newline new-text)
                                   (count #\Newline old-text))))
            (unless content
              (return nil))
            (loop with search-start = 0
                  with cumulative-delta = 0
                  for position = (search old-text content :start2 search-start)
                  while position
                  for old-start-line = (1+ (count #\Newline content
                                                  :end position))
                  for new-start-line = (+ old-start-line cumulative-delta)
                  collect (cons old-start-line new-start-line)
                  do (incf cumulative-delta newline-delta)
                     (setf search-start (+ position (length old-text)))
                  unless replace-all
                    do (loop-finish)))
        (error ()
          nil)))))

(-> application--edit-hunk-rows
    (application string string &key (:new-text string) (:replace-all boolean))
    list)
(defun application--edit-hunk-rows
    (application path old-text &key (new-text "") replace-all)
  "Return bounded numbered diff hunks for an fs.edit call."
  (let ((hunks (application--edit-file-hunks
                application
                path
                old-text
                :new-text new-text
                :replace-all replace-all)))
    (if (null hunks)
        (application--edit-diff-rows old-text new-text :path path)
        (let* ((visible-count (min +application-tool-diff-hunks+
                                   (length hunks)))
               (omitted (- (length hunks) visible-count)))
          (append
           (loop for (old-start-line . new-start-line)
                   in (subseq hunks 0 visible-count)
                 for first-p = t then nil
                 append (append
                         (unless first-p (list nil))
                         (application--edit-diff-rows
                          old-text
                          new-text
                          :old-start-line old-start-line
                          :new-start-line new-start-line
                          :path path)))
           (when (plusp omitted)
             (list nil
                   (list (terminal-span
                          ':dim
                          (format nil "… +~D more replacement~:P" omitted))))))))))

(defmethod application-tool-call-entry
    ((tool fs-edit-tool) (application application) (call hash-table))
  "Present an fs.edit destination and numbered colored replacement diff."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (path (or (and arguments (json-get arguments "path")) ""))
         (old-text (or (and arguments (json-get arguments "old-text")) ""))
         (new-text (or (and arguments (json-get arguments "new-text")) ""))
         (replace-all (and arguments (json-get arguments "replace-all"))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ fs.edit"
     :rows (append
            (list (list (terminal-span ':code path)))
            (when replace-all
              (application--tool-field-rows
               application
               (list (list :label "scope" :value "all occurrences"))))
            (list nil)
            (application--edit-hunk-rows
             application
             path
             old-text
             :new-text new-text
             :replace-all (and replace-all t))))))

(-> application--shell-command-rows (application string) list)
(defun application--shell-command-rows (application command)
  "Return a wrapped shell COMMAND preview with a prompt gutter."
  (let* ((columns (terminal-columns
                   (terminal-ui-terminal (application-ui application))))
         (width (max 8 (- columns 7)))
         (wrapped
           (loop for line in (or (application--display-lines command) (list ""))
                 append (or (wrap-text line width) (list ""))))
         (visible-count (min +application-tool-call-lines+ (length wrapped)))
         (omitted (- (length wrapped) visible-count)))
    (append
     (loop for line in (subseq wrapped 0 visible-count)
           for first-p = t then nil
           collect (list (terminal-span ':dim (if first-p "$ " "  "))
                         (terminal-span ':code line)))
     (when (plusp omitted)
       (list (list (terminal-span
                    ':dim
                    (format nil "… +~D more line~:P" omitted))))))))

(defmethod application-tool-call-entry
    ((tool shell-run-tool) (application application) (call hash-table))
  "Present a shell.run command as shell text with optional execution metadata."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (command (or (and arguments (json-get arguments "command")) ""))
         (directory (and arguments (json-get arguments "directory")))
         (timeout (and arguments (json-get arguments "timeout-seconds")))
         (metadata
           (let ((fields
                   (append
                    (when (non-empty-string-p directory)
                      (list (list :label "directory"
                                  :value directory
                                  :style ':code)))
                    (when (integerp timeout)
                      (list (list :label "timeout"
                                  :value (format nil "~D seconds" timeout)))))))
             (append (when fields (list nil))
                     (application--tool-field-rows application fields)))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ shell.run"
     :rows (append (application--shell-command-rows application command)
                   metadata))))

(defmethod application-tool-call-entry
    ((tool lisp-eval-tool) (application application) (call hash-table))
  "Present a lisp.eval form as bounded Lisp source."
  (declare (ignore tool))
  (application--lisp-call-entry application call "form"))

(defmethod application-tool-call-entry
    ((tool lisp-compile-tool) (application application) (call hash-table))
  "Present a lisp.compile form as bounded Lisp source."
  (declare (ignore tool))
  (application--lisp-call-entry application call "form"))

(defmethod application-tool-call-entry
    ((tool self-eval-tool) (application application) (call hash-table))
  "Present a self.eval form and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "form"))

(defmethod application-tool-call-entry
    ((tool self-exercise-tool) (application application) (call hash-table))
  "Present a self.exercise form as bounded Lisp source."
  (declare (ignore tool))
  (application--lisp-call-entry application call "form"))

(defmethod application-tool-call-entry
    ((tool self-redefine-tool) (application application) (call hash-table))
  "Present a self.redefine definition and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "definition"))

(defmethod application-tool-call-entry
    ((tool self-persist-definition-tool)
     (application application)
     (call hash-table))
  "Present a durable definition and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "definition"))

(defmethod application-tool-call-entry
    ((tool self-set-tool) (application application) (call hash-table))
  "Present a self.set binding, value form, and selected restart."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (symbol (or (and arguments (json-get arguments "symbol")) ""))
         (value (or (and arguments (json-get arguments "value")) "")))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ self.set"
     :rows (append
            (application--tool-field-rows
             application
             (list (list :label "symbol" :value symbol :style ':code)))
            (list (application--tool-section-row "value"))
            (application--preview-rows
             value ':code +application-tool-call-lines+ :gutter "│ ")
            (application--restart-call-rows arguments)))))

(defmethod application-tool-call-entry
    ((tool lisp-load-system-tool) (application application) (call hash-table))
  "Present the requested Lisp system."
  (declare (ignore tool))
  (application--simple-call-entry application call "system"))

(defmethod application-tool-call-entry
    ((tool lisp-run-tests-tool) (application application) (call hash-table))
  "Present the Lisp system whose tests will run."
  (declare (ignore tool))
  (application--simple-call-entry application call "system"))

(defmethod application-tool-call-entry
    ((tool lisp-describe-tool) (application application) (call hash-table))
  "Present the Lisp designator being described."
  (declare (ignore tool))
  (application--simple-call-entry application call "designator"))

(defmethod application-tool-call-entry
    ((tool self-inspect-tool) (application application) (call hash-table))
  "Present the active symbol being inspected."
  (declare (ignore tool))
  (application--simple-call-entry application call "symbol"))

(defmethod application-tool-call-entry
    ((tool self-source-tool) (application application) (call hash-table))
  "Present the active symbol whose tracked source is requested."
  (declare (ignore tool))
  (application--simple-call-entry application call "symbol"))

(defmethod application-tool-call-entry
    ((tool self-rollback-tool) (application application) (call hash-table))
  "Present the retained generation selected for rollback."
  (declare (ignore tool))
  (application--simple-call-entry application call "generation"))

(defmethod application-tool-call-entry
    ((tool self-commit-tool) (application application) (call hash-table))
  "Present a private image-commit title without raw JSON."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (title (or (and arguments (json-get arguments "title")) "")))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ self.commit"
     :rows (application--tool-field-rows
            application
            (list (list :label "title" :value title))))))


;;; Tool result layout

(-> application--tool-result-success-p (list) boolean)
(defun application--tool-result-success-p (record)
  "Return true when tool result RECORD has successful status."
  (eq (getf (rest record) :status) ':ok))

(-> application--tool-result-timing (list) (option string))
(defun application--tool-result-timing (record)
  "Return RECORD's CPU and real duration as a concise detail string."
  (let ((cpu (getf (rest record) :cpu-microseconds))
        (real (getf (rest record) :real-microseconds)))
    (when (and (typep cpu '(integer 0))
               (typep real '(integer 0)))
      (format nil "cpu ~,3Fs · real ~,3Fs"
              (/ cpu 1000000.0d0)
              (/ real 1000000.0d0)))))

(-> application--tool-result-entry
    (application list &key (:detail (option string)) (:rows list))
    list)
(defun application--tool-result-entry (application record &key detail rows)
  "Return RECORD's status header with optional DETAIL and styled ROWS."
  (let* ((success-p (application--tool-result-success-p record))
         (tool-name (getf (rest record) :tool))
         (timing (application--tool-result-timing record))
         (complete-detail
           (cond
             ((and detail timing)
              (format nil "~A · ~A" detail timing))
             (detail detail)
             (timing timing))))
    (application--tool-entry
     application
     :style (if success-p ':success ':failure)
     :header (format nil "~:[✗ ~A failed~;✓ ~A~]" success-p tool-name)
     :detail complete-detail
     :rows rows)))

(-> application--section-preview-rows
    (string string terminal-style &key (:limit integer))
    list)
(defun application--section-preview-rows
    (label text style &key (limit +application-tool-output-lines+))
  "Return a labeled, bounded transcript section for TEXT."
  (append
   (list (application--tool-section-row label))
   (if (non-empty-string-p text)
       (application--preview-rows text style limit :gutter "│ ")
       (list (list (terminal-span ':dim "│ (none)"))))))

(-> application--evaluation-parts
    (string)
    (values (option string) (option string)))
(defun application--evaluation-parts (output)
  "Return captured output and rendered values from an evaluation OUTPUT."
  (let* ((marker (format nil "Values:~%"))
         (values-position (search marker output :from-end t)))
    (if values-position
        (let* ((prefix (string-right-trim
                        '(#\Space #\Tab #\Newline #\Return)
                        (subseq output 0 values-position)))
               (captured (if (uiop:string-prefix-p "Output:" prefix)
                             (string-left-trim
                              '(#\Newline #\Return)
                              (subseq prefix (length "Output:")))
                             prefix))
               (values-text (string-trim
                             '(#\Space #\Tab #\Newline #\Return)
                             (subseq output
                                     (+ values-position (length marker))))))
          (values (and (plusp (length captured)) captured)
                  values-text))
        (values nil nil))))

(-> application--evaluation-result-rows (string) list)
(defun application--evaluation-result-rows (output)
  "Return styled output and values sections from evaluation OUTPUT."
  (multiple-value-bind (captured values-text)
      (application--evaluation-parts output)
    (if (or captured values-text)
        (append
         (when captured
           (application--section-preview-rows "output" captured ':plain))
         (when (and captured values-text) (list nil))
         (application--section-preview-rows "values"
                                            (or values-text "")
                                            ':code))
        (application--preview-rows output
                                   ':dim
                                   +application-tool-output-lines+
                                   :gutter "│ "))))

(-> application--labeled-output-rows (string &key (:limit integer)) list)
(defun application--labeled-output-rows
    (output &key (limit +application-tool-inspection-lines+))
  "Return OUTPUT as aligned fields, headings, and readable continuation rows."
  (let* ((lines (or (application--display-lines output) (list "")))
         (visible-count (min limit (length lines)))
         (omitted (- (length lines) visible-count)))
    (append
     (loop for line in (subseq lines 0 visible-count)
           for colon = (position #\: line)
           collect
           (cond
             ((zerop (length line))
              nil)
             ((and colon
                   (= colon (1- (length line))))
              (list (terminal-span ':strong (subseq line 0 colon))))
             ((and colon
                   (< colon (1- (length line)))
                   (char= (char line (1+ colon)) #\Space))
              (list (terminal-span ':dim
                                   (format nil "~18A " (subseq line 0 colon)))
                    (terminal-span ':plain
                                   (string-left-trim
                                    '(#\Space)
                                    (subseq line (1+ colon))))))
             ((member (char line 0) '(#\Space #\Tab))
              (list (terminal-span ':code line)))
             (t
              (list (terminal-span ':plain line)))))
     (when (plusp omitted)
       (list (list (terminal-span
                    ':dim
                    (format nil "… +~D more line~:P" omitted))))))))

(-> application--restart-row (string) list)
(defun application--restart-row (line)
  "Return one aligned restart NAME and report row parsed from LINE."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (separator (position-if (lambda (character)
                                   (member character '(#\Space #\Tab)))
                                 trimmed)))
    (if separator
        (list (terminal-span ':notice
                             (format nil "~18A " (subseq trimmed 0 separator)))
              (terminal-span ':plain
                             (string-left-trim
                              '(#\Space #\Tab)
                              (subseq trimmed separator))))
        (list (terminal-span ':notice trimmed)))))

(-> application--debugger-rows (string) list)
(defun application--debugger-rows (output)
  "Return separate condition, restart, and retry areas from debugger OUTPUT."
  (let* ((heading (format nil "Available restarts:~%"))
         (heading-position (search heading output))
         (restart-start (and heading-position
                             (+ heading-position (length heading))))
         (retry-position (and restart-start
                              (search "Retry the identical call"
                                      output
                                      :start2 restart-start)))
         (condition-text
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (subseq output 0 (or heading-position
                                             (length output)))))
         (restart-text
           (and restart-start
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (subseq output restart-start
                                     (or retry-position (length output))))))
         (retry-text
           (and retry-position
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (subseq output retry-position)))))
    (append
     (application--section-preview-rows "condition" condition-text ':plain)
     (when restart-text
       (append
        (list nil (application--tool-section-row "available restarts"))
        (loop for line in (subseq
                           (application--display-lines restart-text)
                           0
                           (min +application-tool-output-lines+
                                (length (application--display-lines
                                         restart-text))))
              collect (application--restart-row line))))
     (when retry-text
       (append (list nil (application--tool-section-row "retry"))
               (application--preview-rows
                retry-text ':hint +application-tool-output-lines+
                :gutter "│ "))))))

(-> application--failure-result-rows (string) list)
(defun application--failure-result-rows (output)
  "Return structured condition details for failed tool OUTPUT."
  (let ((restart-marker (format nil "Available restarts:~%"))
        (backtrace-marker (format nil "~%~%Backtrace:~%")))
    (cond
      ((search restart-marker output)
       (application--debugger-rows output))
      ((search backtrace-marker output)
       (let* ((position (search backtrace-marker output))
              (message (subseq output 0 position))
              (backtrace (subseq output
                                 (+ position (length backtrace-marker)))))
         (append
          (application--section-preview-rows "condition" message ':plain)
          (list nil)
          (application--section-preview-rows "backtrace"
                                             backtrace
                                             ':dim))))
      (t
       (application--preview-rows output
                                  ':plain
                                  +application-tool-output-lines+
                                  :gutter "│ ")))))

(-> application--generic-tool-result-entry (application list) list)
(defun application--generic-tool-result-entry (application record)
  "Return a readable fallback entry for tool result RECORD."
  (let ((output (or (getf (rest record) :output) "")))
    (application--tool-result-entry
     application
     record
     :rows (if (application--tool-result-success-p record)
               (application--preview-rows
                output ':dim +application-tool-output-lines+ :gutter "│ ")
               (application--failure-result-rows output)))))

(defmethod application-tool-result-entry
    ((tool tool) (application application) record)
  "Present RECORD using the generic readable result layout."
  (declare (ignore tool))
  (application--generic-tool-result-entry application record))

(defmethod application-tool-result-entry
    ((tool null) (application application) record)
  "Present an unregistered tool result using the generic readable layout."
  (declare (ignore tool))
  (application--generic-tool-result-entry application record))

(defmethod application-tool-result-entry
    ((tool fs-read-tool) (application application) record)
  "Present only fs.read's path and line window, never the returned file lines."
  (if (application--tool-result-success-p record)
      (let ((summary (first (application--display-lines
                             (or (getf (rest record) :output) "")))))
        (application--tool-result-entry
         application
         record
         :rows (when summary
                 (list (list (terminal-span ':code summary))))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool fs-list-tool) (application application) record)
  "Present fs.list output as bounded aligned text."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--preview-rows
              (or (getf (rest record) :output) "")
              ':code
              +application-tool-output-lines+
              :gutter "│ "))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool shell-run-tool) (application application) record)
  "Present shell output beneath an exit-status detail."
  (if (application--tool-result-success-p record)
      (let* ((lines (application--display-lines
                     (or (getf (rest record) :output) "")))
             (status (and lines
                          (uiop:string-prefix-p "exit " (first lines))
                          (first lines)))
             (output-lines (if status (rest lines) lines)))
        (application--tool-result-entry
         application
         record
         :detail status
         :rows (when output-lines
                 (application--preview-rows
                  (format nil "~{~A~^~%~}" output-lines)
                  ':dim
                  +application-tool-output-lines+
                  :gutter "│ "))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool lisp-tool) (application application) record)
  "Present successful worker evaluations as separate output and values areas."
  (if (application--tool-result-success-p record)
      (let ((output (or (getf (rest record) :output) "")))
        (multiple-value-bind (captured values-text)
            (application--evaluation-parts output)
          (if (or captured values-text)
              (application--tool-result-entry
               application
               record
               :rows (application--evaluation-result-rows output))
              (call-next-method))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-eval-tool) (application application) record)
  "Present self.eval output and values in separate bounded areas."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--evaluation-result-rows
              (or (getf (rest record) :output) "")))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-inspect-tool) (application application) record)
  "Present self.inspect output as aligned fields and named sections."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--labeled-output-rows
              (or (getf (rest record) :output) "")))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool lisp-describe-tool) (application application) record)
  "Present lisp.describe output as structured description and values sections."
  (if (application--tool-result-success-p record)
      (let ((output (or (getf (rest record) :output) "")))
        (multiple-value-bind (captured values-text)
            (application--evaluation-parts output)
          (application--tool-result-entry
           application
           record
           :rows (if (or captured values-text)
                     (append
                      (when captured
                        (append
                         (list (application--tool-section-row "description"))
                         (application--labeled-output-rows captured)))
                      (when (and captured values-text) (list nil))
                      (application--section-preview-rows
                       "values" (or values-text "") ':code))
                     (application--labeled-output-rows output)))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-source-tool) (application application) record)
  "Present tracked source as a bounded Lisp code area."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--preview-rows
              (or (getf (rest record) :output) "")
              ':code
              +application-tool-output-lines+
              :gutter "│ "))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-diff-tool) (application application) record)
  "Present pending active-image mutations as bounded code text."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--preview-rows
              (or (getf (rest record) :output) "")
              ':code
              +application-tool-output-lines+
              :gutter "│ "))
      (call-next-method)))
