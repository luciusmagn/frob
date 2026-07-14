(in-package #:autolith)

;;;; -- UI Construction --

(-> terminal-ui--maximum-live-rows (terminal) (integer 1))
(defun terminal-ui--maximum-live-rows (terminal)
  "Return the viewport row budget reserved for TERMINAL's unfinished content."
  (max 1 (1- (terminal-rows terminal))))

(-> terminal-completion-p (t) boolean)
(defun terminal-completion-p (value)
  "Return true when VALUE describes one interactive completion entry."
  (and (listp value)
       (non-empty-string-p (getf value :name))
       (typep (getf value :argument) '(option string))
       (stringp (getf value :description))))

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string)
          (:placeholder string) (:completions list))
    terminal-ui)
(defun terminal-ui-create
    (&key terminal editor (prompt "> ") (placeholder "") completions)
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (unless (every #'terminal-completion-p completions)
    (error 'terminal-error
           :message "Every completion entry needs a name and a description."
           :operation ':create-ui
           :cause nil))
  (let ((live-region
          (make-live-region
           :columns (terminal-columns terminal)
           :maximum-rows (terminal-ui--maximum-live-rows terminal)
           :write-function (lambda (text)
                             (terminal--write terminal text))
           :flush-function (lambda ()
                             (terminal-flush terminal)))))
    (make-instance 'terminal-ui
                   :terminal terminal
                   :editor (or editor
                               (line-editor-create
                                :history-limit +terminal-history-limit+))
                   :live-region live-region
                   :prompt prompt
                   :placeholder placeholder
                   :completions completions
                   :completion-selector
                   (make-selector
                    :visible-count +terminal-ui-visible-completions+
                    :arrangement ':vertical))))

(defmacro with-terminal-ui-locked ((ui) &body body)
  "Run BODY while holding UI's recursive presentation lock."
  (let ((locked-ui (gensym "UI")))
    `(let ((,locked-ui ,ui))
       (with-recursive-lock-held ((terminal-ui-lock ,locked-ui))
         ,@body))))


;;;; -- Terminal Presentation --

(-> terminal-ui-live-row-count (terminal-ui) (integer 0))
(defun terminal-ui-live-row-count (ui)
  "Return the number of live physical rows currently painted for UI."
  (live-region-row-count (terminal-ui-live-region ui)))

(-> terminal-ui-live-cursor-row (terminal-ui) (integer 0))
(defun terminal-ui-live-cursor-row (ui)
  "Return the physical live row currently holding UI's input cursor."
  (live-region-cursor-row (terminal-ui-live-region ui)))

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
  "Write trusted TEXT while making its line endings terminal-safe."
  (let ((line-start 0))
    (loop for newline = (position #\Newline text :start line-start)
          while newline
          do (terminal--write terminal (subseq text line-start newline))
             (terminal--write-newline terminal)
             (setf line-start (1+ newline))
          finally (terminal--write terminal (subseq text line-start))))
  nil)

(-> terminal--spans-text (list) string)
(defun terminal--spans-text (spans)
  "Return the sanitized visible text represented by SPANS."
  (with-output-to-string (stream)
    (dolist (span spans)
      (write-string (sanitize-text (terminal-span-text span)) stream))))

(-> terminal--render-spans (terminal list) string)
(defun terminal--render-spans (terminal spans)
  "Return trusted terminal presentation for sanitized semantic SPANS."
  (with-output-to-string (stream)
    (dolist (span spans)
      (let* ((text (sanitize-text (terminal-span-text span)))
             (sequence
               (and (terminal-styled-p terminal)
                    (terminal-style-sequence (terminal-span-style span)))))
        (when sequence
          (write-string sequence stream))
        (write-string text stream)
        (when sequence
          (write-string +terminal-style-reset+ stream))))))

(-> terminal--write-row (terminal list) null)
(defun terminal--write-row (terminal spans)
  "Write sanitized semantic SPANS as one trusted terminal row."
  (terminal--write-safe-text terminal (terminal--render-spans terminal spans))
  nil)

(-> terminal-ui--prompt-content (terminal-ui) (values list integer))
(defun terminal-ui--prompt-content (ui)
  "Return UI's multiline prompt spans and cursor character offset."
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (terminal-columns terminal))
         (editor (terminal-ui-editor ui))
         (safe-prompt (sanitize-text (terminal-ui-prompt ui)
                                     :single-line-p t)))
    (if (and (zerop (length (line-editor-text editor)))
             (non-empty-string-p (terminal-ui-placeholder ui)))
        (let ((spans
                (terminal--clip-spans
                 (list (terminal-span :brand safe-prompt)
                       (terminal-span :hint (terminal-ui-placeholder ui)))
                 columns)))
          (values spans
                  (min (length safe-prompt)
                       (length (terminal--spans-text spans)))))
        (let ((content-style
                (if (uiop:string-prefix-p "/" (line-editor-text editor))
                    ':user
                    ':plain)))
          (values (list (terminal-span ':brand safe-prompt)
                        (terminal-span content-style
                                       (line-editor-text editor)))
                  (+ (length safe-prompt)
                     (line-editor-cursor editor)))))))

;;;; -- Command Completion Suggestions --

(-> terminal-ui--matching-completions (terminal-ui) list)
(defun terminal-ui--matching-completions (ui)
  "Return UI completions whose names extend the command currently being typed."
  (let ((text (line-editor-text (terminal-ui-editor ui))))
    (if (and (terminal-interactive-p (terminal-ui-terminal ui))
             (terminal-ui-completions ui)
             (uiop:string-prefix-p "/" text)
             (not (find #\Space text))
             (not (find #\Newline text)))
        (remove-if-not (lambda (entry)
                         (uiop:string-prefix-p (string-downcase text)
                                               (getf entry :name)))
                       (terminal-ui-completions ui))
        nil)))

(-> terminal-ui--reconcile-completions (terminal-ui) list)
(defun terminal-ui--reconcile-completions (ui)
  "Return UI's current matches, resetting the selection when the set changes."
  (let ((selector (terminal-ui-completion-selector ui)))
    (if (terminal-ui-completion-active-p ui)
        (selector-items selector)
        (let ((matches (terminal-ui--matching-completions ui)))
          (selector-set-items selector matches)
          matches))))

(-> terminal-completion-label (list) string)
(defun terminal-completion-label (entry)
  "Return completion ENTRY's display name including its argument hint."
  (let ((argument (getf entry :argument)))
    (if argument
        (format nil "~A ~A" (getf entry :name) argument)
        (getf entry :name))))

(-> terminal-ui--choice-rows (selector integer) list)
(defun terminal-ui--choice-rows (selector row-width)
  "Return styled candidate rows from SELECTOR within ROW-WIDTH."
  (multiple-value-bind (index-rows column-widths)
      (selector-arrange selector
                        row-width
                        :width-function
                        (lambda (entry)
                          (text-cell-width
                           (terminal-completion-label entry))))
    (let ((label-width (or (first column-widths) 0)))
      (loop for index-row in index-rows
            for index = (first index-row)
            for entry = (nth index (selector-items selector))
            for selected-p = (= index (selector-selection selector))
            collect (terminal--clip-spans
                     (list (terminal-span (if selected-p
                                              :brand
                                              :dim)
                                          (if selected-p
                                              "▸ "
                                              "  "))
                           (terminal-span :user
                                          (format nil "~vA  "
                                                  label-width
                                                  (terminal-completion-label
                                                   entry)))
                           (terminal-span (if selected-p
                                              :plain
                                              :dim)
                                          (getf entry :description)))
                     row-width)))))

(-> terminal-ui--completion-rows (terminal-ui integer) list)
(defun terminal-ui--completion-rows (ui row-width)
  "Return styled rows for UI's matching command completions."
  (terminal-ui--reconcile-completions ui)
  (terminal-ui--choice-rows (terminal-ui-completion-selector ui) row-width))

(-> terminal-ui--accept-completion (terminal-ui list) null)
(defun terminal-ui--accept-completion (ui entry)
  "Replace UI's input with ENTRY's name, adding a space when it takes an argument."
  (line-editor-set-text
   (terminal-ui-editor ui)
   (sanitize-text
    (concatenate 'string
                 (getf entry :name)
                 (if (getf entry :argument)
                     " "
                     ""))))
  nil)

(-> terminal-ui--begin-completion (terminal-ui) null)
(defun terminal-ui--begin-completion (ui)
  "Begin choosing among UI's current command completion candidates."
  (unless (terminal-ui-completion-active-p ui)
    (setf (terminal-ui-completion-prefix ui)
          (line-editor-text (terminal-ui-editor ui))
          (terminal-ui-completion-active-p ui) t))
  nil)

(-> terminal-ui--end-completion (terminal-ui) null)
(defun terminal-ui--end-completion (ui)
  "Leave UI's active command completion selection without changing input."
  (setf (terminal-ui-completion-active-p ui) nil
        (terminal-ui-completion-prefix ui) nil)
  nil)

(-> terminal-ui--cancel-completion (terminal-ui) null)
(defun terminal-ui--cancel-completion (ui)
  "Cancel UI's completion selection and restore its original command prefix."
  (let ((prefix (terminal-ui-completion-prefix ui)))
    (when prefix
      (line-editor-set-text (terminal-ui-editor ui) prefix)))
  (selector-set-items (terminal-ui-completion-selector ui) nil)
  (terminal-ui--end-completion ui)
  nil)

(-> terminal-ui--handle-completion-event
    (terminal-ui t)
    (values (option keyword) (option string)))
(defun terminal-ui--handle-completion-event (ui event)
  "Apply EVENT to UI's completion suggestions and return its action when consumed."
  (terminal-ui--reconcile-completions ui)
  (let ((selector (terminal-ui-completion-selector ui)))
    (block nil
      (unless (selector-items selector)
        (return (values nil nil)))
      (unless (or (terminal-ui-completion-active-p ui)
                  (member event
                          '(:history-previous :history-next
                            :complete :complete-previous :submit)))
        (return (values nil nil)))
      (when (member event '(:history-previous :history-next
                            :complete :complete-previous))
        (terminal-ui--begin-completion ui))
      (multiple-value-bind (selector-action entry)
          (selector-handle-event selector event)
        (case selector-action
          (:changed
           (terminal-ui--accept-completion ui entry)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:accept
           (terminal-ui--end-completion ui)
           (terminal-ui--accept-completion ui entry)
           (cond
             ((getf entry :argument)
              (terminal-ui--repaint-live ui)
              (values :changed nil))
             (t
              (multiple-value-bind (action payload)
                  (line-editor-handle-event (terminal-ui-editor ui) :submit)
                (terminal-ui--repaint-live ui)
                (values action payload)))))
          (:cancel
           (terminal-ui--cancel-completion ui)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:dismiss
           (terminal-ui--end-completion ui)
           (values nil nil))
          (t
           (values nil nil)))))))


;;;; -- Live Region Composition --

(-> terminal-ui--rows-content
    (terminal list &key (:cursor-row integer) (:cursor-offset integer))
    (values string string integer))
(defun terminal-ui--rows-content
    (terminal rows &key (cursor-row 0) (cursor-offset 0))
  "Return ROWS as plain and styled text plus their cursor character index."
  (let ((plain-stream (make-string-output-stream))
        (display-stream (make-string-output-stream))
        (plain-length 0)
        (cursor-index nil))
    (loop for row in rows
          for index from 0
          for plain = (terminal--spans-text row)
          for display = (terminal--render-spans terminal row)
          do (when (= index cursor-row)
               (setf cursor-index
                     (+ plain-length
                        (min (max 0 cursor-offset) (length plain)))))
             (write-string plain plain-stream)
             (write-string display display-stream)
             (incf plain-length (length plain))
             (when (< (1+ index) (length rows))
               (write-char #\Newline plain-stream)
               (write-char #\Newline display-stream)
               (incf plain-length)))
    (unless cursor-index
      (error 'terminal-error
             :message "The live-region cursor row is outside its content."
             :operation ':render
             :cause nil))
    (values (get-output-stream-string plain-stream)
            (get-output-stream-string display-stream)
            cursor-index)))

(-> terminal-ui--live-content
    (terminal-ui)
    (values string string integer))
(defun terminal-ui--live-content (ui)
  "Return UI's complete plain and styled live content plus its cursor index."
  (let* ((terminal (terminal-ui-terminal ui))
         (row-width (max 1 (terminal-columns terminal)))
         (rows nil))
    (dolist (row (terminal-ui-preview-rows ui))
      (setf rows
            (append rows
                    (list (terminal--clip-spans row row-width)))))
    (let ((tail (terminal-ui-stream-tail ui)))
      (when tail
        (setf rows
              (append rows
                      (list
                       (terminal--clip-spans
                        (if (stringp tail)
                            (list (terminal-span ':plain tail))
                            tail)
                        row-width))))))
    (when (terminal-ui-status ui)
      (setf rows
            (append rows
                    (list
                     (terminal--clip-spans
                      (list (terminal-span ':brand "∙ ")
                            (terminal-span ':dim (terminal-ui-status ui)))
                      row-width)))))
    (when (plusp (terminal-ui-queued-input-count ui))
      (setf rows
            (append rows
                    (list
                     (terminal--clip-spans
                      (list
                       (terminal-span ':brand "∙ ")
                       (terminal-span
                        ':dim
                        (format nil "~D message~:P queued"
                                (terminal-ui-queued-input-count ui))))
                      row-width)))))
    (when rows
      (setf rows (append rows (list nil))))
    (let ((selector (terminal-ui-selector ui)))
      (cond
        (selector
         (let ((title-spans
                 (terminal--clip-spans
                  (list (terminal-span ':brand "∙ ")
                        (terminal-span ':plain
                                       (terminal-ui-selector-title ui))
                        (terminal-span ':hint "  enter selects, esc cancels"))
                  row-width)))
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list title-spans)
                           (terminal-ui--choice-rows
                            selector
                            row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset (length (terminal--spans-text title-spans))))))
        (t
         (multiple-value-bind (prompt-spans cursor-offset)
             (terminal-ui--prompt-content ui)
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list prompt-spans)
                           (terminal-ui--completion-rows
                            ui row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset cursor-offset))))))))

(-> terminal-ui--stream-output (terminal list) string)
(defun terminal-ui--stream-output (terminal rows)
  "Return streamed ROWS as trusted styled output ending on a fresh line."
  (with-output-to-string (stream)
    (dolist (row rows)
      (let ((safe-row
              (loop for span in row
                    collect (terminal-span
                             (terminal-span-style span)
                             (sanitize-text (terminal-span-text span)
                                            :single-line-p t)))))
        (write-string (terminal--render-spans terminal safe-row) stream)
        (write-char #\Newline stream)))))

(-> terminal-ui-stream-update
    (terminal-ui &key (:rows list) (:tail (or null string list)))
    terminal-ui)
(defun terminal-ui-stream-update (ui &key rows tail)
  "Append streamed single-line ROWS to the transcript and show TAIL as unfinished.

Each row is a styled span list appended once without a separating blank row, so
consecutive updates build one continuous transcript block. TAIL replaces the
live unfinished line continuing that block, or removes it when NIL."
  (with-terminal-ui-locked (ui)
    (let* ((terminal (terminal-ui-terminal ui))
           (output (terminal-ui--stream-output terminal rows)))
      (labels ((append-and-repaint ()
                 "Append committed rows and install the latest fluid tail."
                 (when (plusp (length output))
                   (terminal--write-safe-text terminal output))
                 (setf (terminal-ui-stream-tail ui) tail)
                 (terminal-ui--paint-live ui)))
        (if (terminal-interactive-p terminal)
            (call-with-live-region-suspended
             (terminal-ui-live-region ui)
             #'append-and-repaint)
            (append-and-repaint)))
      (terminal-flush terminal)))
  ui)

(-> terminal-ui--paint-live (terminal-ui) null)
(defun terminal-ui--paint-live (ui)
  "Present UI's unfinished content below ordinary terminal scrollback."
  (let ((terminal (terminal-ui-terminal ui)))
    (when (terminal-interactive-p terminal)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content ui)
        (live-region-present (terminal-ui-live-region ui)
                             text
                             :cursor cursor
                             :display display))))
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Recompose and repaint only UI's bounded live region."
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--finalized-content
    (terminal-ui (or string list))
    (values string string))
(defun terminal-ui--finalized-content (ui entry)
  "Return finalized ENTRY as plain and styled text with a blank separator."
  (let* ((terminal (terminal-ui-terminal ui))
         (spans (if (stringp entry)
                    (list (terminal-span ':plain entry))
                    entry))
         (plain (terminal--spans-text spans))
         (display (terminal--render-spans terminal spans)))
    (unless (and (plusp (length plain))
                 (char= (char plain (1- (length plain))) #\Newline))
      (setf plain (concatenate 'string plain (string #\Newline))
            display (concatenate 'string display (string #\Newline))))
    (values (concatenate 'string plain (string #\Newline))
            (concatenate 'string display (string #\Newline)))))


(-> terminal-ui-refresh-size
    (terminal-ui (option function))
    boolean)
(defun terminal-ui-refresh-size (ui callback)
  "Apply CALLBACK's pending terminal size to UI and report whether it repainted."
  (let ((size (and callback (funcall callback))))
    (cond
      ((null size)
       nil)
      ((typep size '(cons (integer 1) (integer 1)))
       (terminal-ui-resize ui (rest size) :rows (first size))
       t)
      (t
       (error 'terminal-error
              :message "A terminal resize callback returned an invalid size."
              :operation ':resize
              :cause nil)))))

(-> terminal-ui-select
    (terminal-ui &key (:title string) (:items list)
                 (:resize-callback (option function)))
    (option string))
(defun terminal-ui-select (ui &key (title "select") items resize-callback)
  "Run a modal picker over ITEMS and return the selected name, or NIL on cancel.

Items follow the completion entry shape. Up and Down move the selection. Tab
and Shift-Tab cycle it forward and backward, and Enter accepts it. Other
ordinary input dismisses the picker with the selected item. Escape, Ctrl-C, or
end of input cancels. Returns NIL immediately when ITEMS is empty or the
terminal is not interactive.

RESIZE-CALLBACK is queried before each blocking read and immediately after the
read returns. It returns positive pending rows and columns as a cons, or NIL
when no resize needs to be applied."
  (block nil
    (unless (and items
                 (every #'terminal-completion-p items)
                 (terminal-interactive-p (terminal-ui-terminal ui)))
      (return nil))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-selector ui)
            (make-selector
             :items items
             :visible-count +terminal-ui-visible-completions+
             :arrangement ':vertical)
            (terminal-ui-selector-title ui) title))
    (unwind-protect
         (loop
           (with-terminal-ui-locked (ui)
             (unless (terminal-ui-refresh-size ui resize-callback)
               (terminal-ui--repaint-live ui)))
           (let ((event (terminal-read-event (terminal-ui-terminal ui))))
             (with-terminal-ui-locked (ui)
               (terminal-ui-refresh-size ui resize-callback)
               (multiple-value-bind (action item)
                   (selector-handle-event (terminal-ui-selector ui) event)
                 (case action
                   (:accept
                    (return (getf item :name)))
                   (:cancel
                    (return nil))
                   (:dismiss
                    (return (getf item :name)))
                   (t
                    nil))))))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-selector ui) nil
              (terminal-ui-selector-title ui) nil)
        (terminal-ui--repaint-live ui)))))


;;;; -- Public UI Lifecycle and Events --

(-> terminal-ui-start (terminal-ui) terminal-ui)
(defun terminal-ui-start (ui)
  "Start UI on the primary screen and render its bounded live region."
  (with-terminal-ui-locked (ui)
    (unless (terminal-ui-started-p ui)
      (terminal-start (terminal-ui-terminal ui))
      (setf (terminal-ui-started-p ui) t)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-stop (terminal-ui) terminal-ui)
(defun terminal-ui-stop (ui)
  "Erase UI's unfinished rows and restore its terminal even after partial startup."
  (with-terminal-ui-locked (ui)
    (unwind-protect
         (when (terminal-ui-started-p ui)
           (live-region-dismiss (terminal-ui-live-region ui)))
      (setf (terminal-ui-started-p ui) nil)
      (terminal-stop (terminal-ui-terminal ui))))
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

(-> terminal-ui-mark-finalized (terminal-ui t) boolean)
(defun terminal-ui-mark-finalized (ui identifier)
  "Remember finalized IDENTIFIER and return true only on its first occurrence."
  (with-terminal-ui-locked (ui)
    (block nil
      (when (gethash identifier (terminal-ui-finalized-identifiers ui))
        (return nil))
      (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
      t)))

(-> terminal-ui-append-finalized (terminal-ui t (or string list)) boolean)
(defun terminal-ui-append-finalized (ui identifier entry)
  "Append finalized transcript ENTRY once for IDENTIFIER and return true when emitted."
  (with-terminal-ui-locked (ui)
    (block nil
      (unless (terminal-ui-mark-finalized ui identifier)
        (return nil))
      (multiple-value-bind (text display)
          (terminal-ui--finalized-content ui entry)
        (if (terminal-interactive-p (terminal-ui-terminal ui))
            (live-region-append (terminal-ui-live-region ui)
                                text
                                :display display)
            (progn
              (terminal--write-safe-text (terminal-ui-terminal ui) display)
              (terminal-flush (terminal-ui-terminal ui)))))
      t)))

(-> terminal-ui-set-preview-rows (terminal-ui list) terminal-ui)
(defun terminal-ui-set-preview-rows (ui rows)
  "Replace UI's transient styled ROWS and repaint only the live region."
  (unless (every #'terminal-styled-text-p rows)
    (error 'terminal-error
           :message "Every terminal preview row must contain styled spans."
           :operation ':set-preview
           :cause nil))
  (with-terminal-ui-locked (ui)
    (unless (equal rows (terminal-ui-preview-rows ui))
      (setf (terminal-ui-preview-rows ui) rows)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-status (terminal-ui (option string)) terminal-ui)
(defun terminal-ui-set-status (ui status)
  "Replace UI's unfinished one-row STATUS and repaint only the live region."
  (with-terminal-ui-locked (ui)
    (let ((safe-status (and status
                            (sanitize-text status :single-line-p t))))
      (unless (equal safe-status (terminal-ui-status ui))
        (setf (terminal-ui-status ui) safe-status)
        (terminal-ui--paint-live ui))))
  ui)

(-> terminal-ui-set-queued-input-count (terminal-ui integer) terminal-ui)
(defun terminal-ui-set-queued-input-count (ui count)
  "Set UI's queued message COUNT and repaint when it changes."
  (check-type count (integer 0))
  (with-terminal-ui-locked (ui)
    (unless (= count (terminal-ui-queued-input-count ui))
      (setf (terminal-ui-queued-input-count ui) count)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-input (terminal-ui string) terminal-ui)
(defun terminal-ui-set-input (ui text)
  "Replace UI's editable input with TEXT and repaint it."
  (with-terminal-ui-locked (ui)
    (line-editor-set-text (terminal-ui-editor ui) (sanitize-text text))
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-set-cursor-visible (terminal-ui boolean) terminal-ui)
(defun terminal-ui-set-cursor-visible (ui visible-p)
  "Set whether UI leaves its input cursor visible between terminal updates."
  (with-terminal-ui-locked (ui)
    (when (terminal-interactive-p (terminal-ui-terminal ui))
      (live-region-set-cursor-visible (terminal-ui-live-region ui) visible-p)))
  ui)

(-> terminal-ui-resize
    (terminal-ui integer &key (:rows (option integer)))
    terminal-ui)
(defun terminal-ui-resize (ui columns &key rows)
  "Set UI terminal dimensions and repaint only unfinished rows."
  (with-terminal-ui-locked (ui)
    (let* ((new-columns (max 1 columns))
           (new-rows (and rows (max 1 rows)))
           (region (terminal-ui-live-region ui)))
      (live-region-suspend region)
      (setf (terminal-columns (terminal-ui-terminal ui)) new-columns)
      (when new-rows
        (setf (terminal-rows (terminal-ui-terminal ui)) new-rows))
      (live-region-resize
       region
       new-columns
       :maximum-rows
       (terminal-ui--maximum-live-rows (terminal-ui-terminal ui)))
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-read-event (terminal-ui) t)
(defun terminal-ui-read-event (ui)
  "Read one semantic input event for UI without emitting fallback prompt controls."
  (terminal-read-event (terminal-ui-terminal ui)))

(-> terminal-ui--safe-editor-event (t) t)
(defun terminal-ui--safe-editor-event (event)
  "Return EVENT with direct text input sanitized for terminal presentation."
  (if (and (consp event)
           (member (first event) '(:insert :paste :line))
           (consp (rest event))
           (stringp (second event)))
      (list (first event) (sanitize-text (second event)))
      event))

(-> terminal-ui--apply-editor-event
    (terminal-ui t)
    (values keyword (option string)))
(defun terminal-ui--apply-editor-event (ui event)
  "Apply EVENT through Clinedi while preserving Autolith interaction policy."
  (let ((editor (terminal-ui-editor ui)))
    (cond
      ((and (eq event :interrupt)
            (plusp (length (line-editor-text editor))))
       (line-editor-clear editor)
       (values :cleared nil))
      ((eq event :complete)
       (line-editor-handle-event editor '(:insert "    "))
       (values :changed nil))
      ((eq event :clear-screen)
       (values :changed nil))
      (t
       (multiple-value-bind (action payload)
           (line-editor-handle-event
            editor
            (terminal-ui--safe-editor-event event))
         (values (if (eq action :continue) ':changed action)
                 payload))))))

(-> terminal-ui-process-event
    (terminal-ui t)
    (values keyword (option string)))
(defun terminal-ui-process-event (ui event)
  "Apply EVENT to UI's suggestions or editor and return its action and payload."
  (with-terminal-ui-locked (ui)
    (multiple-value-bind (completion-action completion-payload)
        (terminal-ui--handle-completion-event ui event)
      (if completion-action
          (values completion-action completion-payload)
          (multiple-value-bind (action payload)
              (terminal-ui--apply-editor-event ui event)
            (when (member action '(:changed :cleared :submit))
              (terminal-ui--repaint-live ui))
            (values action payload))))))
