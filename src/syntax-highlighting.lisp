(in-package #:autolith)

;;;; -- Semantic Syntax Highlighting --

(-> syntax--category-style (keyword) terminal-style)
(defun syntax--category-style (category)
  "Map a ColorLisp semantic CATEGORY onto Autolith's terminal palette."
  (case category
    (:comment ':syntax-comment)
    ((:keyword :macro) ':syntax-keyword)
    (:string ':syntax-string)
    ((:escape :special) ':syntax-escape)
    ((:number :constant) ':syntax-number)
    ((:type :namespace :builtin) ':syntax-type)
    ((:function :method) ':syntax-function)
    ((:property :attribute) ':syntax-property)
    (:heading ':syntax-heading)
    (:link ':syntax-link)
    (otherwise ':plain)))

(-> syntax--segments->lines (list) vector)
(defun syntax--segments->lines (segments)
  "Convert ColorLisp SEGMENTS into a vector of terminal-span rows."
  (let ((rows    nil)
        (current nil))
    (labels ((emit (style text start end)
               "Append TEXT between START and END to the current styled row."
               (when (< start end)
                 (push (terminal-span style (subseq text start end)) current)))

             (finish-row ()
               "Finish the current row and begin another."
               (push (nreverse current) rows)
               (setf current nil)))
      (dolist (segment segments)
        (let ((text (segment-text segment))
              (style (syntax--category-style (segment-category segment)))
              (start 0))
          (loop for newline = (position #\Newline text :start start)
                while newline
                do (emit style text start newline)
                   (finish-row)
                   (setf start (1+ newline))
                finally (emit style text start (length text)))))
      (finish-row))
    (coerce (nreverse rows) 'vector)))

(-> syntax--highlight-lines
    (string &key (:language (option language)) (:pathname t))
    (option vector))
(defun syntax--highlight-lines (source &key language pathname)
  "Return SOURCE as syntax-highlighted rows, or NIL without a language.

LANGUAGE is an already resolved ColorLisp language. When only PATHNAME is
given, detect its language from the pathname and SOURCE. ColorLisp failures
degrade to unstyled output at the caller."
  (handler-case
      (let ((resolved (or language
                          (and pathname
                               (language-detect pathname :source source)))))
        (and resolved
             (syntax--segments->lines
              (highlight-segments source :language resolved))))
    (colorlisp-error ()
      nil)))

(-> syntax--spans-subseq (list integer integer) list)
(defun syntax--spans-subseq (spans start end)
  "Return the character range from START to END within styled SPANS."
  (let ((position 0)
        (result nil))
    (dolist (span spans (nreverse result))
      (let* ((text (terminal-span-text span))
             (span-end (+ position (length text)))
             (part-start (max start position))
             (part-end (min end span-end)))
        (when (< part-start part-end)
          (push (terminal-span
                 (terminal-span-style span)
                 (subseq text
                         (- part-start position)
                         (- part-end position)))
                result))
        (setf position span-end)))))
