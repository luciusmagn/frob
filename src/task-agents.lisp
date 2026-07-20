(in-package #:autolith)

;;;; -- Native Task Agent Definitions --

(defclass task-agent-definition nil
  ((name
    :initarg :name
    :reader task-agent-definition-name
    :type non-empty-string
    :documentation "The stable normalized agent type requested by a task call.")
   (description
    :initarg :description
    :reader task-agent-definition-description
    :type non-empty-string
    :documentation "The concise model-visible purpose of this agent type.")
   (instructions
    :initarg :instructions
    :reader task-agent-definition-instructions
    :type non-empty-string
    :documentation "The specialized role instructions prepended to assignments.")
   (tools
    :initarg :tools
    :initform nil
    :reader task-agent-definition-tools
    :type t
    :documentation "NIL, an explicit child tool allowlist, or :ALL child-safe tools.")
   (spawns
    :initarg :spawns
    :initform nil
    :reader task-agent-definition-spawns
    :type t
    :documentation "NIL, a list of agent names, or :ALL for nested delegation.")
   (models
    :initarg :models
    :initform nil
    :reader task-agent-definition-models
    :type list
    :documentation "Preferred model aliases or identifiers in fallback order.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :reader task-agent-definition-reasoning-effort
    :type (option keyword)
    :documentation "The optional child reasoning effort or :AUTO sentinel.")
   (output
    :initarg :output
    :initform nil
    :reader task-agent-definition-output
    :type (option list)
    :documentation "An optional validated native structured-output contract.")
   (blocking-p
    :initarg :blocking-p
    :initform nil
    :reader task-agent-definition-blocking-p
    :type boolean
    :documentation "True when this agent must complete inline even in async mode.")
   (source
    :initarg :source
    :reader task-agent-definition-source
    :type keyword
    :documentation "The bundled, user, project, or programmatic origin.")
   (pathname
    :initarg :pathname
    :initform nil
    :reader task-agent-definition-pathname
    :type (option pathname)
    :documentation "The native source file for a discovered definition."))
  (:documentation "A role and fail-closed policy for one child agent."))

(-> task-agent-name-p (t) boolean)
(defun task-agent-name-p (value)
  "Return true when VALUE is a bounded portable child-role name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) +task-agent-name-maximum-characters+)
       (let ((name (string-downcase value)))
         (and (or (and (char>= (char name 0) #\a)
                       (char<= (char name 0) #\z)))
              (loop for character across name
                    always (or (and (char>= character #\a)
                                    (char<= character #\z))
                               (digit-char-p character)
                               (char= character #\-)))))))

(-> task-agent--validate-string-list
    (t keyword &key (:pathname (option pathname))
                   (:source keyword)
                   (:definition-name (option string))
                   (:allow-empty-p boolean))
    list)
(defun task-agent--validate-string-list
    (value field &key pathname source definition-name allow-empty-p)
  "Validate and copy a proper unique string list for FIELD."
  (unless (and (task--proper-list-p value)
               (or allow-empty-p value)
               (every #'non-empty-string-p value)
               (task--unique-list-p value :test #'string-equal))
    (task-agent-definition--error
     :pathname pathname :source source :field field
     :cause "The value must be a proper list of unique non-empty strings."
     :definition-name definition-name))
  (copy-list value))

(-> task-agent--tool-component-p (t) boolean)
(defun task-agent--tool-component-p (value)
  "Return true when VALUE is one canonical lowercase tool-name component."
  (and (stringp value)
       (plusp (length value))
       (char>= (char value 0) #\a)
       (char<= (char value 0) #\z)
       (loop for character across value
             always (or (and (char>= character #\a)
                             (char<= character #\z))
                        (digit-char-p character)
                        (member character '(#\- #\_) :test #'char=)))))

(-> task-agent--tool-specification-namespace (string) (option string))
(defun task-agent--tool-specification-namespace (specification)
  "Return a canonical grant's namespace, or NIL when it is malformed."
  (unless (string= specification "web_search")
    (let ((separator (position #\. specification)))
      (when (and separator
                 (= separator (position #\. specification :from-end t)))
        (let ((namespace (subseq specification 0 separator))
              (name (subseq specification (1+ separator))))
          (when (and (task-agent--tool-component-p namespace)
                     (or (string= name "*")
                         (task-agent--tool-component-p name)))
            namespace))))))

(-> task-agent--tool-specification-p (string) boolean)
(defun task-agent--tool-specification-p (specification)
  "Return true when SPECIFICATION is canonical and structurally child-safe."
  (or (string= specification "web_search")
      (let ((namespace
              (task-agent--tool-specification-namespace specification)))
        (and namespace
             (not (member namespace
                          +task-forbidden-child-tool-namespaces+
                          :test #'string=))))))

(-> task-agent--normalize-tools
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    t)
(defun task-agent--normalize-tools
    (value &key pathname source definition-name)
  "Validate fail-closed child tool policy VALUE."
  (cond
    ((null value) nil)
    ((eq value :all) :all)
    ((task--proper-list-p value)
     (let ((tools
             (task-agent--validate-string-list
              value :tools
              :pathname pathname
              :source source
              :definition-name definition-name
              :allow-empty-p t)))
       (dolist (specification tools)
         (unless (task-agent--tool-specification-p specification)
           (task-agent-definition--error
            :pathname pathname :source source :field :tools
            :cause (format nil "Malformed or forbidden child tool grant ~S."
                           specification)
            :definition-name definition-name)))
       tools))
    (t
     (task-agent-definition--error
      :pathname pathname :source source :field :tools
      :cause ":TOOLS must be NIL, :ALL, or a proper list of tool strings."
      :definition-name definition-name))))

(-> task-agent--normalize-spawns
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    t)
(defun task-agent--normalize-spawns
    (value &key pathname source definition-name)
  "Validate fail-closed nested role policy VALUE."
  (cond
    ((null value) nil)
    ((eq value :all) :all)
    ((task--proper-list-p value)
     (let ((spawns
             (task-agent--validate-string-list
              value :spawns
              :pathname pathname
              :source source
              :definition-name definition-name
              :allow-empty-p t)))
       (unless (every #'task-agent-name-p spawns)
         (task-agent-definition--error
          :pathname pathname :source source :field :spawns
          :cause "Every spawn grant must be a portable child-role name."
          :definition-name definition-name))
       (mapcar #'string-downcase spawns)))
    (t
     (task-agent-definition--error
      :pathname pathname :source source :field :spawns
      :cause ":SPAWNS must be NIL, :ALL, or a proper list of role-name strings."
      :definition-name definition-name))))

(-> task-agent--model-p (string) boolean)
(defun task-agent--model-p (model)
  "Return true when MODEL is a supported identifier or child alias."
  (and (or (member model +supported-models+ :test #'string=)
           (member model +task-model-aliases+ :test #'string=))
       t))

(-> task-agent--normalize-models
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    list)
(defun task-agent--normalize-models
    (value &key pathname source definition-name)
  "Validate child model preference VALUE."
  (when value
    (let ((models
            (task-agent--validate-string-list
             value :models
             :pathname pathname
             :source source
             :definition-name definition-name
             :allow-empty-p nil)))
      (unless (every #'task-agent--model-p models)
        (task-agent-definition--error
         :pathname pathname :source source :field :models
         :cause "A model entry is neither supported nor a documented alias."
         :definition-name definition-name))
      models)))

(-> task-agent--normalize-reasoning-effort
    (t &key (:pathname (option pathname))
            (:source keyword)
            (:definition-name (option string)))
    (option keyword))
(defun task-agent--normalize-reasoning-effort
    (value &key pathname source definition-name)
  "Validate native child reasoning effort VALUE."
  (unless (or (null value)
              (eq value :auto)
              (and (keywordp value)
                   (member (string-downcase (symbol-name value))
                           +supported-reasoning-efforts+
                           :test #'string=)))
    (task-agent-definition--error
     :pathname pathname :source source :field :reasoning-effort
     :cause ":REASONING-EFFORT must be NIL, :AUTO, or a supported effort keyword."
     :definition-name definition-name))
  value)

(-> task-agent-definition-create
    (&key (:name t) (:description t) (:instructions t) (:tools t)
          (:spawns t) (:models t) (:reasoning-effort t) (:output t)
          (:blocking-p t) (:source keyword) (:pathname (option pathname)))
    task-agent-definition)
(defun task-agent-definition-create
    (&key name description instructions tools spawns models reasoning-effort
          output blocking-p (source :programmatic) pathname)
  "Create and fully validate one native child-agent definition."
  (let ((normalized-name
          (and (stringp name) (string-downcase name)))
        (normalized-description
          (and (stringp description) (task--trim description)))
        (normalized-instructions
          (and (stringp instructions) (task--trim instructions))))
    (unless (task-agent-name-p name)
      (task-agent-definition--error
       :pathname pathname :source source :field :name
       :cause "The role name must start with an ASCII letter and contain only letters, digits, or hyphens."
       :definition-name normalized-name))
    (unless (and (non-empty-string-p normalized-description)
                 (<= (length normalized-description)
                     +task-agent-description-maximum-characters+))
      (task-agent-definition--error
       :pathname pathname :source source :field :description
       :cause "The description must be non-empty and within its character bound."
       :definition-name normalized-name))
    (unless (and (non-empty-string-p normalized-instructions)
                 (<= (length normalized-instructions)
                     +task-agent-instructions-maximum-characters+))
      (task-agent-definition--error
       :pathname pathname :source source :field :instructions
       :cause "The instructions must be non-empty and within their character bound."
       :definition-name normalized-name))
    (unless (typep blocking-p 'boolean)
      (task-agent-definition--error
       :pathname pathname :source source :field :blocking-p
       :cause ":BLOCKING-P must be T or NIL."
       :definition-name normalized-name))
    (make-instance
     'task-agent-definition
     :name normalized-name
     :description normalized-description
     :instructions normalized-instructions
     :tools (task-agent--normalize-tools
             tools
             :pathname pathname :source source
             :definition-name normalized-name)
     :spawns (task-agent--normalize-spawns
              spawns
              :pathname pathname :source source
              :definition-name normalized-name)
     :models (task-agent--normalize-models
              models
              :pathname pathname :source source
              :definition-name normalized-name)
     :reasoning-effort
     (task-agent--normalize-reasoning-effort
      reasoning-effort
      :pathname pathname :source source
      :definition-name normalized-name)
     :output
     (and output
          (task-output-schema-normalize
           output
           :pathname pathname :source source
           :definition-name normalized-name))
     :blocking-p blocking-p
     :source source
     :pathname pathname)))

(-> task-bundled-agent-definitions () list)
(defun task-bundled-agent-definitions ()
  "Return fresh bundled child-agent definitions in presentation order."
  (list
   (task-agent-definition-create
    :name "scout"
    :description "Fast read-only codebase research and compressed handoff context."
    :instructions
    "Investigate rapidly and return source-grounded findings. Stay read-only. Search broadly, read only relevant sections, cite paths and line ranges, explain how the pieces connect, and finish with a concise handoff."
    :tools '("fs.read" "fs.list" "search.*" "web_search")
    :models '("@smol")
    :reasoning-effort :medium
    :source :bundled)
   (task-agent-definition-create
    :name "designer"
    :description "UI and UX specialist for implementation, review, and visual refinement."
    :instructions
    "Act as a pragmatic product and interface designer. Inspect the existing design language before changing it, preserve accessibility and terminal constraints, implement concrete improvements when asked, and report the rationale and verification."
    :tools :all
    :models '("@designer")
    :reasoning-effort :high
    :source :bundled)
   (task-agent-definition-create
    :name "reviewer"
    :description "Code review specialist for correctness, security, and regression analysis."
    :instructions
    "Review the requested change as a senior maintainer. Prioritize concrete correctness, security, data-loss, concurrency, and compatibility defects. Verify claims against source and tests. Return actionable findings ordered by severity, with paths and line ranges, and avoid stylistic noise."
    :tools '("fs.read" "fs.list" "shell.run" "search.*" "web_search")
    :spawns '("scout")
    :models '("@slow")
    :reasoning-effort :high
    :blocking-p t
    :source :bundled)
   (task-agent-definition-create
    :name "librarian"
    :description "Source-verifying researcher for external libraries, APIs, and standards."
    :instructions
    "Research external behavior from authoritative documentation and source. Prefer installed dependency source and primary references over memory. State versions and uncertainty, quote exact APIs where useful, and return a concise implementation-ready answer."
    :tools '("fs.read" "fs.list" "shell.run" "lisp.*" "search.*"
             "web_search")
    :models '("@smol")
    :reasoning-effort :low
    :source :bundled)
   (task-agent-definition-create
    :name "task"
    :description "General-purpose child agent for delegated multi-step work."
    :instructions
    "Own the delegated assignment end to end. Inspect before changing, preserve unrelated work, use the available tools directly, verify proportionally to risk, and return concrete results rather than a plan. Delegate only when it materially helps."
    :tools :all
    :spawns :all
    :models '("@task")
    :source :bundled)
   (task-agent-definition-create
    :name "sonic"
    :description "Low-overhead agent for strictly mechanical updates or data collection."
    :instructions
    "Perform only the narrowly specified mechanical work. Avoid redesign and speculative cleanup. Make the smallest correct change, run a focused verification, and report exactly what changed."
    :tools :all
    :models '("@smol")
    :reasoning-effort :medium
    :source :bundled)))


;;;; -- Safe Native Reader --

(-> task-agent--source-line (string integer) (integer 1))
(defun task-agent--source-line (source offset)
  "Return the one-based line containing OFFSET in SOURCE."
  (1+ (count #\Newline source :end (min offset (length source)))))

(-> task-agent--file-byte-length (pathname) (integer 0))
(defun task-agent--file-byte-length (pathname)
  "Return PATHNAME's byte length without allocating its contents."
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (file-length stream)))

(-> task-agent--read-bounded-contents (pathname keyword string) string)
(defun task-agent--read-bounded-contents (pathname source definition-name)
  "Read PATHNAME as UTF-8 while enforcing the byte bound on bytes consumed."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((limit +task-agent-file-maximum-bytes+)
           (octets (make-array (1+ limit)
                               :element-type '(unsigned-byte 8)))
           (count (read-sequence octets stream))
           (end (gensym "END"))
           (next (read-byte stream nil end)))
      (when (or (> count limit) (not (eq next end)))
        (task-agent-definition--error
         :pathname pathname :source source
         :cause "The native role file exceeds its byte bound."
         :definition-name definition-name))
      (sb-ext:octets-to-string octets
                               :start 0
                               :end count
                               :external-format :utf-8))))

(-> task-agent--source-token-delimiter-p (character) boolean)
(defun task-agent--source-token-delimiter-p (character)
  "Return true when CHARACTER terminates an atom in restricted role syntax."
  (not
   (null
    (member character
            '(#\Space #\Tab #\Newline #\Return #\Page
              #\( #\) #\" #\; #\# #\' #\` #\, #\\ #\|)
            :test #'char=))))

(-> task-agent--validate-source-shape (string pathname keyword string) null)
(defun task-agent--validate-source-shape
    (contents pathname source definition-name)
  "Bound native list depth and reject reader macros outside block comments."
  (let ((depth 0)
        (block-comment-depth 0)
        (line-comment-p nil)
        (string-p nil)
        (escaped-p nil))
    (labels ((source-error (offset message &optional field)
               (task-agent-definition--error
                :pathname pathname
                :source source
                :line (task-agent--source-line contents offset)
                :field field
                :cause message
                :definition-name definition-name)))
      (loop with length = (length contents)
            for index from 0 below length
            for character = (char contents index)
            for next = (and (< (1+ index) length)
                            (char contents (1+ index)))
            do
               (cond
                 (string-p
                  (cond
                    (escaped-p
                     (setf escaped-p nil))
                    ((char= character #\\)
                     (setf escaped-p t))
                    ((char= character #\")
                     (setf string-p nil))))
                 (line-comment-p
                  (when (member character '(#\Newline #\Return)
                                :test #'char=)
                    (setf line-comment-p nil)))
                 ((plusp block-comment-depth)
                  (cond
                    ((and (char= character #\#)
                          next
                          (char= next #\|))
                     (incf block-comment-depth)
                     (incf index))
                    ((and (char= character #\|)
                          next
                          (char= next #\#))
                     (decf block-comment-depth)
                     (incf index))))
                 ((char= character #\;)
                  (setf line-comment-p t))
                 ((char= character #\")
                  (setf string-p t))
                 ((and (char= character #\#)
                       next
                       (char= next #\|))
                  (setf block-comment-depth 1)
                  (incf index))
                 ((char= character #\:)
                  (let* ((previous (and (plusp index)
                                        (char contents (1- index))))
                         (token-start (1+ index))
                         (token-end
                           (or (position-if
                                #'task-agent--source-token-delimiter-p
                                contents
                                :start token-start)
                               length))
                         (token (subseq contents token-start token-end)))
                    (when (or (and next (char= next #\:))
                              (and previous
                                   (not (or (member previous
                                                    '(#\Space #\Tab #\Newline
                                                      #\Return #\Page)
                                                    :test #'char=)
                                            (char= previous #\()
                                            (char= previous #\))
                                            (char= previous #\")
                                            (char= previous #\#)))))
                      (source-error
                       index
                       "Package-qualified symbols are not allowed in native role files."))
                    (unless (member token
                                    +task-agent-native-keyword-names+
                                    :test #'string-equal)
                      (let ((existing-keyword
                              (find-symbol (string-upcase token)
                                           '#:keyword)))
                        (source-error
                         index
                         (format nil
                                 "Unknown native role keyword :~A."
                                 token)
                         existing-keyword)))))
                 ((member character '(#\# #\' #\` #\, #\\ #\|)
                          :test #'char=)
                  (source-error
                   index
                   "Only lists, strings, keywords, numbers, booleans, and block comments are allowed in native role files."))
                 ((char= character #\()
                  (incf depth)
                  (when (> depth +task-agent-form-maximum-depth+)
                    (source-error
                     index
                     "The native role form exceeds its nesting-depth bound.")))
                 ((char= character #\))
                  (decf depth)
                  (when (minusp depth)
                    (source-error index
                                  "The native role form has an unmatched closing parenthesis.")))))
      (when (plusp block-comment-depth)
        (source-error (length contents)
                      "The native role file has an unterminated block comment."))
      (when string-p
        (source-error (length contents)
                      "The native role file has an unterminated string."))
      (unless (zerop depth)
        (source-error (length contents)
                      "The native role file has unbalanced parentheses."))))
  nil)

(-> task-agent--validate-readable-tree
    (t &key (:pathname pathname) (:source keyword) (:definition-name string))
    null)
(defun task-agent--validate-readable-tree
    (form &key pathname source definition-name)
  "Reject shared, circular, oversized, or non-portable objects in FORM."
  (let ((seen (make-hash-table :test #'eq))
        (nodes 0)
        (pending (list form)))
    (loop while pending
          for value = (pop pending)
          do
             (incf nodes)
             (when (> nodes +task-agent-form-maximum-nodes+)
               (task-agent-definition--error
                :pathname pathname :source source
                :cause "The native role form exceeds its node bound."
                :definition-name definition-name))
             (typecase value
               (null nil)
               (cons
                (when (gethash value seen)
                  (task-agent-definition--error
                   :pathname pathname :source source
                   :cause "Shared and circular reader objects are not allowed."
                   :definition-name definition-name))
                (setf (gethash value seen) t)
                (push (rest value) pending)
                (push (first value) pending))
               (string
                (when (gethash value seen)
                  (task-agent-definition--error
                   :pathname pathname :source source
                   :cause "Shared reader strings are not allowed."
                   :definition-name definition-name))
                (setf (gethash value seen) t)
                (when (> (length value)
                         +task-agent-string-maximum-characters+)
                  (task-agent-definition--error
                   :pathname pathname :source source
                   :cause "A native role string exceeds its character bound."
                   :definition-name definition-name)))
               (symbol
                (unless (or (keywordp value) (eq value t))
                  (task-agent-definition--error
                   :pathname pathname :source source
                   :cause (format nil "Non-keyword symbol ~S is not portable role data."
                                  value)
                   :definition-name definition-name)))
               (integer nil)
               (float
                (unless (task-output--json-number-p value)
                  (task-agent-definition--error
                   :pathname pathname :source source
                   :cause "A native role contains a non-finite float."
                   :definition-name definition-name)))
               (t
                (task-agent-definition--error
                 :pathname pathname :source source
                 :cause (format nil "Object ~S is not supported native role data."
                                value)
                 :definition-name definition-name)))))
  nil)

(-> task-agent--make-reader-package () package)
(defun task-agent--make-reader-package ()
  "Return a fresh temporary package for reading untrusted native role data."
  (loop for name = (symbol-name (gensym "AUTOLITH-TASK-READER-"))
        unless (find-package name)
          return (make-package name :use '(#:cl))))

(-> task-agent--read-native-form (pathname keyword string) t)
(defun task-agent--read-native-form (pathname source definition-name)
  "Read exactly one bounded native role form from PATHNAME."
  (handler-case
      (progn
        (when (> (task-agent--file-byte-length pathname)
                 +task-agent-file-maximum-bytes+)
          (task-agent-definition--error
           :pathname pathname :source source
           :cause "The native role file exceeds its byte bound."
           :definition-name definition-name))
        (let ((contents
                (task-agent--read-bounded-contents
                 pathname source definition-name)))
          (task-agent--validate-source-shape
           contents pathname source definition-name)
          (let ((reader-package (task-agent--make-reader-package)))
            (unwind-protect
                 (with-input-from-string (stream contents)
                   (let* ((*package* reader-package)
                          (*read-base* 10)
                          (*read-default-float-format* 'double-float)
                          (*read-eval* nil)
                          (*read-suppress* nil)
                          (*readtable* (copy-readtable nil))
                          (end (gensym "END")))
                     (handler-case
                         (let ((form (read stream nil end)))
                           (when (eq form end)
                             (task-agent-definition--error
                              :pathname pathname :source source :line 1
                              :cause "The native role file contains no form."
                              :definition-name definition-name))
                           (let ((extra (read stream nil end)))
                             (unless (eq extra end)
                               (task-agent-definition--error
                                :pathname pathname :source source
                                :line (task-agent--source-line
                                       contents (or (file-position stream) 0))
                                :cause
                                "The native role file contains more than one form."
                                :definition-name definition-name)))
                           (task-agent--validate-readable-tree
                            form
                            :pathname pathname
                            :source source
                            :definition-name definition-name)
                           form)
                       (task-agent-definition-error (condition)
                         (error condition))
                       (error (condition)
                         (task-agent-definition--error
                          :pathname pathname :source source
                          :line (task-agent--source-line
                                 contents (or (file-position stream) 0))
                          :cause condition
                          :definition-name definition-name)))))
              (delete-package reader-package)))))
    (task-agent-definition-error (condition)
      (error condition))
    (error (condition)
      (task-agent-definition--error
       :pathname pathname :source source
       :line 1
       :cause condition
       :definition-name definition-name))))

(-> task-agent--required-value
    (keyword list &key (:pathname pathname) (:source keyword)
                       (:definition-name string))
    t)
(defun task-agent--required-value
    (field pairs &key pathname source definition-name)
  "Return required FIELD from PAIRS or signal a native role diagnostic."
  (multiple-value-bind (value present-p)
      (task--alist-value field pairs)
    (unless present-p
      (task-agent-definition--error
       :pathname pathname :source source :field field
       :cause "The required property is absent."
       :definition-name definition-name))
    value))

(-> task-parse-agent-file (pathname keyword) task-agent-definition)
(defun task-parse-agent-file (pathname source)
  "Parse one safe native PATHNAME into a child-agent definition from SOURCE."
  (let* ((basename (string-downcase (pathname-name pathname)))
         (form (task-agent--read-native-form pathname source basename))
         (pairs
           (task--plist-alist
            form +task-agent-definition-fields+
            :pathname pathname
            :source source
            :line 1
            :definition-name basename))
         (name
           (task-agent--required-value
            :name pairs
            :pathname pathname :source source :definition-name basename))
         (description
           (task-agent--required-value
            :description pairs
            :pathname pathname :source source :definition-name basename))
         (instructions
           (task-agent--required-value
            :instructions pairs
            :pathname pathname :source source :definition-name basename)))
    (unless (and (stringp name)
                 (string= (string-downcase name) basename))
      (task-agent-definition--error
       :pathname pathname :source source :field :name
       :cause (format nil "The role name must match file basename ~S." basename)
       :definition-name basename))
    (task-agent-definition-create
     :name name
     :description description
     :instructions instructions
     :tools (rest (assoc :tools pairs :test #'eq))
     :spawns (rest (assoc :spawns pairs :test #'eq))
     :models (rest (assoc :models pairs :test #'eq))
     :reasoning-effort (rest (assoc :reasoning-effort pairs :test #'eq))
     :output (rest (assoc :output pairs :test #'eq))
     :blocking-p (rest (assoc :blocking-p pairs :test #'eq))
     :source source
     :pathname pathname)))


;;;; -- Fail-Closed Discovery --

(-> task--agent-files (pathname) list)
(defun task--agent-files (directory)
  "Return sorted native role files immediately inside DIRECTORY."
  (when (uiop/filesystem:directory-exists-p directory)
    (sort (copy-list (uiop/filesystem:directory-files directory "*.sexp"))
          #'string<
          :key #'namestring)))

(-> task--user-agents-directory (configuration) pathname)
(defun task--user-agents-directory (configuration)
  "Return the user child-agent directory under CONFIGURATION's config root."
  (merge-pathnames "agents/" (configuration-config-root configuration)))

(-> task--project-agents-directory (configuration) (option pathname))
(defun task--project-agents-directory (configuration)
  "Return the nearest project .autolith/agents directory, or NIL."
  (let* ((start (configuration-working-directory configuration))
         (root (system-prompt--project-root start)))
    (loop repeat 64
          for directory = start
            then (uiop/pathname:pathname-parent-directory-pathname directory)
          for candidate = (merge-pathnames ".autolith/agents/" directory)
          when (uiop/filesystem:directory-exists-p candidate)
            return candidate
          when (equal directory root)
            return nil)))

(-> task-agent--basename-diagnostic (pathname keyword string) condition)
(defun task-agent--basename-diagnostic (pathname source basename)
  "Return a typed diagnostic for invalid role file BASENAME."
  (make-condition
   'task-agent-definition-error
   :message (format nil "Invalid task agent basename ~S in ~A." basename pathname)
   :tool-name "task.run"
   :pathname pathname
   :source source
   :line nil
   :field :name
   :cause "A role basename must use the portable role-name grammar."
   :definition-name basename))

(-> task-agent--duplicate-diagnostic (list keyword string) condition)
(defun task-agent--duplicate-diagnostic (pathnames source basename)
  "Return a typed diagnostic for duplicate normalized BASENAME files."
  (make-condition
   'task-agent-definition-error
   :message (format nil "Duplicate ~A task agent basename ~S." source basename)
   :tool-name "task.run"
   :pathname (first pathnames)
   :source source
   :line nil
   :field :name
   :cause
   (format nil "Multiple files claim the same normalized role name: ~{~A~^, ~}."
           pathnames)
   :definition-name basename))

(-> task-agent--origin-groups (pathname keyword) (values list list))
(defun task-agent--origin-groups (directory source)
  "Return valid basename groups and immediate filename diagnostics for SOURCE."
  (let ((groups (make-hash-table :test #'equal))
        (order nil)
        (diagnostics nil))
    (dolist (pathname (task--agent-files directory))
      (let* ((raw (pathname-name pathname))
             (basename (string-downcase raw)))
        (if (task-agent-name-p basename)
            (progn
              (unless (gethash basename groups)
                (push basename order))
              (push pathname (gethash basename groups)))
            (push (task-agent--basename-diagnostic pathname source basename)
                  diagnostics))))
    (values
     (loop for basename in (nreverse order)
           collect (cons basename
                         (nreverse (gethash basename groups))))
     (nreverse diagnostics))))

(-> task-agent--load-origin
    ((option pathname) keyword hash-table)
    (values list list))
(defun task-agent--load-origin (directory source claimed)
  "Load unclaimed roles from one DIRECTORY and return definitions and diagnostics."
  (let ((definitions nil)
        (diagnostics nil))
    (when directory
      (multiple-value-bind (groups filename-diagnostics)
          (task-agent--origin-groups directory source)
        (setf diagnostics filename-diagnostics)
        (dolist (group groups)
          (let ((basename (first group))
                (pathnames (rest group)))
            (unless (gethash basename claimed)
              (setf (gethash basename claimed) t)
              (cond
                ((rest pathnames)
                 (setf diagnostics
                       (nconc
                        diagnostics
                        (list
                         (task-agent--duplicate-diagnostic
                          pathnames source basename)))))
                (t
                 (handler-case
                     (setf definitions
                           (nconc definitions
                                  (list
                                   (task-parse-agent-file
                                    (first pathnames) source))))
                   (task-agent-definition-error (condition)
                     (setf diagnostics
                           (nconc diagnostics (list condition))))))))))))
    (values definitions diagnostics)))

(-> task-discover-agents (configuration) (values list list))
(defun task-discover-agents (configuration)
  "Discover effective roles and typed diagnostics by fail-closed precedence.

Project basenames claim a name before parsing, followed by user basenames and
then bundled roles. An invalid higher-precedence file therefore blocks only its
own lower-precedence name. The second value contains diagnostics suitable for
task.agents and for re-signaling when task.run requests a blocked role."
  (let ((claimed (make-hash-table :test #'equal))
        (definitions nil)
        (diagnostics nil))
    (flet ((load-origin (directory source)
             (multiple-value-bind (new-definitions new-diagnostics)
                 (task-agent--load-origin directory source claimed)
               (setf definitions (nconc definitions new-definitions)
                     diagnostics (nconc diagnostics new-diagnostics)))))
      (load-origin (task--project-agents-directory configuration) :project)
      (load-origin (task--user-agents-directory configuration) :user))
    (dolist (definition (task-bundled-agent-definitions))
      (let ((name (task-agent-definition-name definition)))
        (unless (gethash name claimed)
          (setf (gethash name claimed) t
                definitions (nconc definitions (list definition))))))
    (values definitions diagnostics)))

(-> task-find-agent-definition (list string) (option task-agent-definition))
(defun task-find-agent-definition (definitions name)
  "Return the case-insensitively named definition from DEFINITIONS."
  (find name definitions
        :test #'string-equal
        :key #'task-agent-definition-name))

(-> task-find-agent-diagnostic (list string) (option condition))
(defun task-find-agent-diagnostic (diagnostics name)
  "Return the typed diagnostic reserving NAME, when one exists."
  (find name diagnostics
        :test #'string-equal
        :key #'task-agent-definition-error-definition-name))
