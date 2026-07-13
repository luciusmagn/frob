(in-package #:autolith)

;;;; -- Markdown Test Helpers --

(-> markdown-tests--row-text (list) string)
(defun markdown-tests--row-text (row)
  "Return ROW's concatenated visible text."
  (apply #'concatenate 'string (mapcar #'terminal-span-text row)))


;;;; -- Focused Markdown Tests --

(-> test-markdown-inline-spans () null)
(defun test-markdown-inline-spans ()
  "Test emphasis, strong, and code span parsing with literal fallbacks."
  (let* ((renderer (markdown-renderer-create :width 60))
         (row (first (markdown-render-line
                      renderer
                      "plain **bold** *soft* `code` end"))))
    (test-assert (find (terminal-span :strong "bold") row :test #'equal)
                 "paired double asterisks render a strong span")
    (test-assert (find (terminal-span :emphasis "soft") row :test #'equal)
                 "paired single asterisks render an emphasis span")
    (test-assert (find (terminal-span :code "code") row :test #'equal)
                 "paired backticks render an inline code span")
    (test-assert (not (find #\* (markdown-tests--row-text row)))
                 "paired inline markers are consumed"))
  (let ((literal-cases '(("2 * 3 * 4" "spaced asterisks stay literal")
                         ("unclosed **bold" "unclosed markers stay literal")
                         ("lonely ` backtick" "unpaired backticks stay literal"))))
    (loop for (line description) in literal-cases
          do (let ((renderer (markdown-renderer-create :width 60)))
               (test-assert
                (string= (markdown-tests--row-text
                          (first (markdown-render-line renderer line)))
                         (format nil "  ~A" line))
                description))))
  (let* ((renderer (markdown-renderer-create :width 60))
         (row (first (markdown-render-line renderer "`keep *stars*` out"))))
    (test-assert (find (terminal-span :code "keep *stars*") row :test #'equal)
                 "inline code protects markers inside it"))
  nil)

(-> test-markdown-lists () null)
(defun test-markdown-lists ()
  "Test bullet and numbered items with hanging continuation indents."
  (let* ((renderer (markdown-renderer-create :width 30))
         (rows (markdown-render-line
                renderer
                "- item one that is long enough to wrap")))
    (test-assert (= (length rows) 2)
                 "long list items wrap into multiple rows")
    (test-assert (uiop:string-prefix-p "  • item one"
                                       (markdown-tests--row-text (first rows)))
                 "bullets render with an item marker")
    (test-assert (uiop:string-prefix-p "    "
                                       (markdown-tests--row-text (second rows)))
                 "list continuations align under the item text")
    (test-assert (find (terminal-span :brand "• ") (first rows) :test #'equal)
                 "bullet markers carry the brand style"))
  (let* ((renderer (markdown-renderer-create :width 40))
         (rows (markdown-render-line renderer "12. twelfth item")))
    (test-assert (string= (markdown-tests--row-text (first rows))
                          "  12. twelfth item")
                 "numbered items keep their original marker"))
  (let* ((renderer (markdown-renderer-create :width 40))
         (rows (markdown-render-line renderer "  - nested item")))
    (test-assert (string= (markdown-tests--row-text (first rows))
                          "    • nested item")
                 "nested bullets keep their extra indent"))
  nil)

(-> test-markdown-code-blocks () null)
(defun test-markdown-code-blocks ()
  "Test fenced code blocks with dim numbered gutters and state resets."
  (let ((renderer (markdown-renderer-create :width 40)))
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer "```lisp")))
                          "  ```lisp")
                 "opening fences show their language dim")
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer
                                                        "(defun foo ()")))
                          "    1 │ (defun foo ()")
                 "code lines render behind a numbered gutter")
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer "  42)")))
                          "    2 │   42)")
                 "code line numbers advance per logical line")
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer "```")))
                          "  ```")
                 "closing fences terminate the block")
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer
                                                        "*after* block")))
                          "  after block")
                 "inline parsing resumes after a code block")
    (markdown-render-line renderer "```")
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer "(new)")))
                          "    1 │ (new)")
                 "a new fence restarts line numbering"))
  nil)

(-> test-markdown-tables () null)
(defun test-markdown-tables ()
  "Test bounded pipe tables, relaxed separators, wrapping, and state exit."
  (let ((renderer (markdown-renderer-create :width 48)))
    (let ((header (first (markdown-render-line
                          renderer
                          "| Property | Clasp’s approach |"))))
      (test-assert (find (terminal-span ':strong "Property")
                         header
                         :test #'equal)
                   "table headers render as strong cells")
      (test-assert (search " │ " (markdown-tests--row-text header))
                   "table headers use a visible column boundary")
      (test-assert (not (find #\| (markdown-tests--row-text header)))
                   "source pipe markers do not leak into rendered tables"))
    (let ((divider (first (markdown-render-line renderer "|:--|:--|"))))
      (test-assert (search "┼" (markdown-tests--row-text divider))
                   "two-hyphen model table separators render as dividers"))
    (let ((rows (markdown-render-line
                 renderer
                 "| Main tradeoff | Considerably more machinery than a small standalone Lisp |")))
      (test-assert (> (length rows) 1)
                   "long table cells wrap within their assigned column")
      (test-assert (every (lambda (row)
                           (<= (text-cell-width (markdown-tests--row-text row))
                               48))
                         rows)
                   "wrapped table rows stay inside the renderer width"))
    (let ((ordinary (first (markdown-render-line renderer "after table"))))
      (test-assert (string= (markdown-tests--row-text ordinary)
                            "  after table")
                   "ordinary rendering resumes after a table")))
  nil)

(-> test-markdown-partial-streaming () null)
(defun test-markdown-partial-streaming ()
  "Test speculative wrapped tails retain enough source for correct completion."
  (let ((renderer (markdown-renderer-create :width 24)))
    (multiple-value-bind (rows tail retained)
        (markdown-render-partial renderer "alpha beta gamma delta epsilon")
      (test-assert (null rows)
                   "unfinished logical lines do not commit wrapped rows")
      (test-assert (search "epsilon" (markdown-tests--row-text tail))
                   "the tail previews the unfinished remainder")
      (test-assert (string= retained "alpha beta gamma delta epsilon")
                   "retained source keeps the complete logical line"))
    (let ((rows (markdown-render-line
                 renderer
                 "alpha beta gamma delta epsilon zeta")))
      (test-assert (string= (markdown-tests--row-text (first rows))
                            "  alpha beta gamma delta")
                   "committing the completed line renders its first row")
      (test-assert (search "epsilon zeta"
                           (markdown-tests--row-text (second rows)))
                   "committing the completed line renders its continuation")))
  (let ((renderer (markdown-renderer-create :width 24)))
    (multiple-value-bind (rows tail retained)
        (markdown-render-partial renderer
                                 "**bold text that goes on and on**")
      (declare (ignore tail))
      (test-assert (null rows)
                   "styled wrapped rows remain speculative")
      (test-assert (string= retained "**bold text that goes on and on**")
                   "retained source preserves paired delimiters")))
  (let* ((renderer (markdown-renderer-create :width 24))
         (first-fragment "123456789012345678901*X"))
    (multiple-value-bind (rows tail retained)
        (markdown-render-partial renderer first-fragment)
      (declare (ignore tail))
      (test-assert (null rows)
                   "a delimiter split after a visual wrap commits nothing")
      (multiple-value-bind (more-rows more-tail complete)
          (markdown-render-partial renderer
                                   (concatenate 'string retained "*"))
        (declare (ignore more-tail))
        (test-assert (null more-rows)
                     "closing a delimiter remains speculative until newline")
        (let ((committed (markdown-render-line renderer complete)))
          (test-assert
           (find (terminal-span :emphasis "X")
                 committed
                 :test (lambda (span rows)
                         (find span rows :test #'equal)))
           "a delimiter split at a wrap boundary styles its completed text")
          (test-assert
           (not (find #\* (format nil "~{~A~}"
                                  (mapcar #'markdown-tests--row-text
                                          committed))))
           "paired delimiters never leak into committed scrollback")))))
  (let ((renderer (markdown-renderer-create :width 40)))
    (markdown-render-line renderer "```")
    (multiple-value-bind (rows tail retained)
        (markdown-render-partial renderer "(partial")
      (test-assert (null rows)
                   "short partial code lines commit nothing")
      (test-assert (search "1 │ (partial" (markdown-tests--row-text tail))
                   "the code tail previews its upcoming line number")
      (test-assert (string= retained "(partial")
                   "partial code lines stay pending"))
    (test-assert (string= (markdown-tests--row-text
                           (first (markdown-render-line renderer
                                                        "(partial code)")))
                          "    1 │ (partial code)")
                 "completed code lines number from the preserved counter"))
  nil)

(-> run-markdown-tests () boolean)
(defun run-markdown-tests ()
  "Run focused markdown rendering tests and return true on success."
  (test-markdown-inline-spans)
  (test-markdown-lists)
  (test-markdown-code-blocks)
  (test-markdown-tables)
  (test-markdown-partial-streaming)
  t)
