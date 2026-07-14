(in-package #:autolith)

;;;; -- In-Process Task Orchestration --

(define-constant +task-default-maximum-concurrency+ 32 :documentation
                 "The default number of child agents that may run concurrently.")

(define-constant +task-default-maximum-depth+ 2 :documentation
                 "The default maximum child-agent depth below the primary agent.")

(define-constant +task-default-maximum-output-bytes+ 500000 :documentation
                 "The default maximum UTF-8 bytes retained from one child result.")

(define-constant +task-default-maximum-output-lines+ 5000 :documentation
                 "The default maximum lines retained from one child result.")

(define-constant +task-progress-output-limit+ 8000 :documentation
                 "The assistant-text tail retained in a live child progress snapshot.")

(define-constant +task-result-preview-limit+ 6000 :documentation
                 "The result characters shown inline before referring to an artifact.")

(define-condition task-error
    (tool-error)
  ((task-id :initarg :task-id :initform nil :reader task-error-task-id :type
	    (option string) :documentation
	    "The child or job identifier involved in the failure, when known."))
  (:documentation
   "A task request, child run, or task job violated its contract."))

(define-condition task-aborted
    (autolith-error)
  ((reason :initarg :reason :reader task-aborted-reason :type keyword
	   :documentation "The structured reason the child run was interrupted."))
  (:documentation
   "A child agent was deliberately interrupted before normal completion."))

(define-condition task-yield-error
    (task-error)
  nil
  (:documentation
   "A child agent supplied an invalid or duplicate terminal yield."))

(defclass task-agent-definition nil
  ((name :initarg :name :reader task-agent-definition-name :type
         non-empty-string :documentation
         "The stable agent type requested by a task call.")
   (description :initarg :description :reader
		task-agent-definition-description :type non-empty-string
		:documentation
		"The concise model-visible purpose of this agent type.")
   (system-prompt :initarg :system-prompt :reader
                  task-agent-definition-system-prompt :type string
                  :documentation
                  "The specialized role instructions prepended to each assignment.")
   (tools :initarg :tools :initform nil :reader
          task-agent-definition-tools :type list :documentation
          "The optional child tool allowlist.")
   (spawns :initarg :spawns :initform nil :reader
           task-agent-definition-spawns :type t :documentation
           "NIL, a list of agent names, or :ALL for nested delegation.")
   (models :initarg :models :initform nil :reader
           task-agent-definition-models :type list :documentation
           "Preferred model aliases or identifiers in fallback order.")
   (thinking-level :initarg :thinking-level :initform nil :reader
		   task-agent-definition-thinking-level :type (option string)
		   :documentation
		   "The optional child reasoning effort or AUTO sentinel.")
   (output :initarg :output :initform nil :reader
           task-agent-definition-output :type t :documentation
           "An optional structured-output definition for yield data.")
   (blocking-p :initarg :blocking-p :initform nil :reader
               task-agent-definition-blocking-p :type boolean :documentation
               "True when this agent must complete inline even in async mode.")
   (autoload-skills :initarg :autoload-skills :initform nil :reader
		    task-agent-definition-autoload-skills :type list :documentation
		    "Skill names advisory-loaded into the child prompt.")
   (read-summarize-p :initarg :read-summarize-p :initform nil :reader
		     task-agent-definition-read-summarize-p :type boolean :documentation
		     "True when file reads should be summarized rather than verbatim.")
   (source :initarg :source :reader task-agent-definition-source :type
           keyword :documentation
           "The bundled, user, or project origin of this definition.")
   (pathname :initarg :pathname :initform nil :reader
             task-agent-definition-pathname :type (option pathname)
             :documentation
             "The Markdown source file for a discovered definition."))
  (:documentation
   "A role and policy object used to configure one child agent."))

(defun task-agent-definition-create
    (
     &key name description system-prompt tools spawns models thinking-level
     output blocking-p autoload-skills read-summarize-p source pathname)
  "Create and validate one child-agent definition."
  (unless (non-empty-string-p name)
    (error 'task-error :message
           "An agent definition requires a non-empty name." :tool-name
           "task.run"))
  (unless (non-empty-string-p description)
    (error 'task-error :message
           (format nil "Agent ~A requires a non-empty description." name)
           :tool-name "task.run"))
  (make-instance 'task-agent-definition :name (string-downcase name)
                 :description description :system-prompt (or system-prompt "")
                 :tools tools :spawns spawns :models models :thinking-level
                 thinking-level :output output :blocking-p (and blocking-p t)
                 :autoload-skills autoload-skills :read-summarize-p
                 (and read-summarize-p t) :source source :pathname pathname))

(defun task-bundled-agent-definitions ()
  "Return fresh bundled child-agent definitions in presentation order."
  (list
   (task-agent-definition-create :name "scout" :description
                                 "Fast read-only codebase research and compressed handoff context."
                                 :system-prompt
                                 "Investigate rapidly and return source-grounded findings. Stay read-only. Search broadly, read only relevant sections, cite paths and line ranges, explain how the pieces connect, and finish with a concise handoff."
                                 :tools '("fs.read" "fs.list" "web_search")
                                 :models '("@smol") :thinking-level "medium"
                                 :source :bundled)
   (task-agent-definition-create :name "designer" :description
                                 "UI and UX specialist for implementation, review, and visual refinement."
                                 :system-prompt
                                 "Act as a pragmatic product and interface designer. Inspect the existing design language before changing it, preserve accessibility and terminal constraints, implement concrete improvements when asked, and report the rationale and verification."
                                 :models '("@designer") :thinking-level "high"
                                 :source :bundled)
   (task-agent-definition-create :name "reviewer" :description
                                 "Code review specialist for correctness, security, and regression analysis."
                                 :system-prompt
                                 "Review the requested change as a senior maintainer. Prioritize concrete correctness, security, data-loss, concurrency, and compatibility defects. Verify claims against source and tests. Return actionable findings ordered by severity, with paths and line ranges, and avoid stylistic noise."
                                 :tools
                                 '("fs.read" "fs.list" "shell.run"
                                   "web_search")
                                 :spawns '("scout") :models '("@slow")
                                 :thinking-level "high" :blocking-p t :source
                                 :bundled)
   (task-agent-definition-create :name "librarian" :description
                                 "Source-verifying researcher for external libraries, APIs, and standards."
                                 :system-prompt
                                 "Research external behavior from authoritative documentation and source. Prefer installed dependency source and primary references over memory. State versions and uncertainty, quote exact APIs where useful, and return a concise implementation-ready answer."
                                 :tools
                                 '("fs.read" "fs.list" "shell.run" "lisp.*"
                                   "web_search")
                                 :models '("@smol") :thinking-level "low"
                                 :source :bundled)
   (task-agent-definition-create :name "task" :description
                                 "General-purpose child agent for delegated multi-step work."
                                 :system-prompt
                                 "Own the delegated assignment end to end. Inspect before changing, preserve unrelated work, use the available tools directly, verify proportionally to risk, and return concrete results rather than a plan. Delegate only when it materially helps."
                                 :spawns :all :models '("@task") :source
                                 :bundled)
   (task-agent-definition-create :name "sonic" :description
                                 "Low-overhead agent for strictly mechanical updates or data collection."
                                 :system-prompt
                                 "Perform only the narrowly specified mechanical work. Avoid redesign and speculative cleanup. Make the smallest correct change, run a focused verification, and report exactly what changed."
                                 :models '("@smol") :thinking-level "medium"
                                 :source :bundled)))

(defun task--trim (text)
  "Return TEXT without surrounding horizontal or line whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun task--split-lines (text)
  "Return TEXT as a list of lines without newline characters."
  (loop with start = 0
        for
        end = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return)
                                   (subseq text start (or end (length text))))
        while
        end
        do (setf start (1+ end))))

(defun task--split-commas (text)
  "Return trimmed non-empty comma-separated fields from TEXT."
  (loop with start = 0
        for
        end = (position #\COMMA text :start start)
        for piece = (task--trim (subseq text start (or end (length text))))
        when (non-empty-string-p piece)
        collect piece
        while
        end
        do (setf start (1+ end))))

(defun task--unquote (text)
  "Decode a simple quoted frontmatter scalar in TEXT."
  (let ((trimmed (task--trim text)))
    (cond
      ((and (> (length trimmed) 1) (char= (char trimmed 0) #\QUOTATION_MARK)
            (char= (char trimmed (1- (length trimmed))) #\QUOTATION_MARK))
       (handler-case
	   (let ((decoded (json-decode trimmed)))
             (if (stringp decoded)
		 decoded
		 trimmed))
	 (error nil (subseq trimmed 1 (1- (length trimmed))))))
      ((and (> (length trimmed) 1) (char= (char trimmed 0) #\APOSTROPHE)
            (char= (char trimmed (1- (length trimmed))) #\APOSTROPHE))
       (subseq trimmed 1 (1- (length trimmed))))
      (t trimmed))))

(defun task--frontmatter-scalar (text)
  "Parse one small YAML-compatible frontmatter scalar."
  (let ((value (task--unquote text)))
    (cond ((string-equal value "true") t) ((string-equal value "false") nil)
          ((string= value "*") :all)
          ((and (> (length value) 1)
                (char= (char value 0) #\LEFT_SQUARE_BRACKET)
                (char= (char value (1- (length value)))
                       #\RIGHT_SQUARE_BRACKET))
           (mapcar #'task--unquote
                   (task--split-commas (subseq value 1 (1- (length value))))))
          ((and (> (length value) 1)
                (member (char value 0)
                        '(#\LEFT_CURLY_BRACKET #\LEFT_SQUARE_BRACKET) :test
                        #'char=))
           (handler-case (json-decode value) (error nil value)))
          (t value))))

(defun task--frontmatter-list (value)
  "Normalize a frontmatter VALUE into a list of strings."
  (cond ((null value) nil) ((stringp value) (task--split-commas value))
        ((listp value) (mapcar #'string value))
        ((vectorp value)
         (loop for item across value
               collect (string item)))
        (t (list (princ-to-string value)))))

(defun task--markdown-frontmatter (contents pathname)
  "Return top-level frontmatter, body, and raw nested fields from CONTENTS."
  (let ((lines (task--split-lines contents)))
    (unless (and lines (string= (task--trim (first lines)) "---"))
      (error 'task-error :message
             (format nil "Agent file ~A has no YAML frontmatter." pathname)
             :tool-name "task.run"))
    (let ((fields (make-hash-table :test #'equal))
          (nested (make-hash-table :test #'equal))
          (body-lines nil)
          (frontmatter-p t)
          (current-key nil))
      (dolist (line (rest lines))
        (cond
          ((and frontmatter-p (string= (task--trim line) "---"))
           (setf frontmatter-p nil
                 current-key nil))
          (frontmatter-p
           (let ((indented-p
                  (and (plusp (length line))
                       (member (char line 0) '(#\Space #\Tab) :test #'char=))))
             (if indented-p
                 (when current-key
                   (setf (gethash current-key nested)
                         (append (gethash current-key nested) (list line))))
                 (let ((separator (position #\COLON line)))
                   (when separator
                     (let ((key
                            (string-downcase
                             (task--trim (subseq line 0 separator))))
                           (value (task--trim (subseq line (1+ separator)))))
                       (setf current-key key)
                       (if (non-empty-string-p value)
                           (setf (gethash key fields)
                                 (task--frontmatter-scalar value))
                           (setf (gethash key fields) nil))))))))
          (t (push line body-lines))))
      (values fields
              (task--trim (format nil "~{~A~^~%~}" (nreverse body-lines)))
              nested))))

(defun task-parse-agent-file (pathname source)
  "Parse one Markdown PATHNAME into a child-agent definition from SOURCE."
  (multiple-value-bind (fields body nested)
      (task--markdown-frontmatter (uiop/stream:read-file-string pathname)
                                  pathname)
    (let* ((name (gethash "name" fields))
           (description (gethash "description" fields))
           (tools (task--frontmatter-list (gethash "tools" fields)))
           (models (task--frontmatter-list (gethash "model" fields)))
           (spawn-value (gethash "spawns" fields))
           (spawns
            (if (eq spawn-value :all)
                :all
                (task--frontmatter-list spawn-value)))
           (output
            (or (gethash "output" fields)
                (let ((lines (gethash "output" nested)))
                  (and lines (format nil "~{~A~^~%~}" lines))))))
      (task-agent-definition-create :name (and name (string name)) :description
                                    (and description (string description))
                                    :system-prompt body :tools tools :spawns
                                    spawns :models models :thinking-level
                                    (let ((value
                                           (or
                                            (gethash "thinking-level" fields)
                                            (gethash "thinkinglevel" fields))))
                                      (and value
                                           (string-downcase (string value))))
                                    :output output :blocking-p
                                    (gethash "blocking" fields)
                                    :autoload-skills
                                    (task--frontmatter-list
                                     (or (gethash "autoload-skills" fields)
                                         (gethash "autoloadskills" fields)))
                                    :read-summarize-p
                                    (or (gethash "read-summarize" fields)
                                        (gethash "readsummarize" fields))
                                    :source source :pathname pathname))))

(defun task--agent-files (directory)
  "Return sorted Markdown files immediately inside DIRECTORY."
  (when (uiop/filesystem:directory-exists-p directory)
    (sort (copy-list (uiop/filesystem:directory-files directory "*.md"))
          #'string< :key #'namestring)))

(defun task--user-agents-directory (configuration)
  "Return the user child-agent directory under CONFIGURATION's config root."
  (merge-pathnames "agents/" (configuration-config-root configuration)))

(defun task--project-agents-directory (configuration)
  "Return the nearest project .autolith/agents directory, or NIL."
  (let* ((start (configuration-working-directory configuration))
         (root (system-prompt--project-root start)))
    (loop repeat 64
          for directory = start then (uiop/pathname:pathname-parent-directory-pathname
                                      directory)
          for candidate = (merge-pathnames ".autolith/agents/" directory)
          when (uiop/filesystem:directory-exists-p candidate) return candidate
          when (equal directory root) return nil)))

(defun task-discover-agents (configuration)
  "Discover project and user agents before bundled definitions, first name wins."
  (let ((seen (make-hash-table :test #'equal))
        (definitions nil)
        (project-directory (task--project-agents-directory configuration))
        (user-directory
         (task--user-agents-directory configuration)))
    (labels ((consider (definition)
               (let ((name (task-agent-definition-name definition)))
                 (unless (gethash name seen)
                   (setf (gethash name seen) t)
                   (setf definitions (nconc definitions (list definition))))))
             (load-directory (directory source)
               (dolist (pathname (task--agent-files directory))
                 (handler-case
                     (consider (task-parse-agent-file pathname source))
                   (error nil nil)))))
      (when project-directory (load-directory project-directory :project))
      (load-directory user-directory :user)
      (dolist (definition (task-bundled-agent-definitions))
        (consider definition)))
    definitions))

(defun task-find-agent-definition (definitions name)
  "Return the case-insensitively named definition from DEFINITIONS."
  (find name definitions :test #'string-equal :key
        #'task-agent-definition-name))

(defclass task-completion nil
  ((called-p :initform nil :accessor task-completion-called-p :type
             boolean :documentation
             "True after the child accepted one terminal yield.")
   (status :initform nil :accessor task-completion-status :type
           (option keyword) :documentation
           "The success, failed, or aborted yield status.")
   (text :initform nil :accessor task-completion-text :type
         (option string) :documentation
         "The optional human-readable yield result.")
   (data :initform nil :accessor task-completion-data :type t
         :documentation "The optional structured yield value.")
   (error :initform nil :accessor task-completion-error :type
          (option string) :documentation
          "The optional child-reported failure text.")
   (label :initform nil :accessor task-completion-label :type
          (option string) :documentation
          "The optional concise result label."))
  (:documentation
   "The explicit terminal protocol state of one child agent."))

(defclass task-progress nil
  ((lock :initform (make-lock "Autolith task progress") :reader
         task-progress-lock :documentation
         "The lock protecting snapshots read by job tools.")
   (status :initform :queued :accessor task-progress-status :type
           keyword :documentation
           "The queued, running, completed, failed, or aborted state.")
   (current-tool :initform nil :accessor task-progress-current-tool
		 :type (option string) :documentation
		 "The tool currently executing in the child.")
   (recent-tools :initform nil :accessor task-progress-recent-tools
		 :type list :documentation
		 "The newest completed child tools, newest first.")
   (output-tail :initform "" :accessor task-progress-output-tail :type
		string :documentation
		"The bounded tail of streamed assistant text.")
   (request-count :initform 0 :accessor task-progress-request-count
		  :type (integer 0) :documentation
		  "The provider requests started by the child.")
   (usage :initform nil :accessor task-progress-usage :type t
          :documentation "The newest portable provider usage snapshot.")
   (started-at :initform nil :accessor task-progress-started-at :type t
               :documentation "The internal real time at which execution began.")
   (updated-at :initform (get-internal-real-time) :accessor
               task-progress-updated-at :type integer :documentation
               "The internal real time of the newest progress event."))
  (:documentation "A normalized, thread-safe child progress snapshot."))

(defclass task-orchestrator nil
  ((lock :initform (make-lock "Autolith task orchestrator") :reader
         task-orchestrator-lock :documentation
         "The lock protecting concurrency, identities, jobs, and listeners.")
   (condition-variable :initform (make-condition-variable) :reader
		       task-orchestrator-condition-variable :documentation
		       "The condition used by queued children awaiting capacity.")
   (maximum-concurrency :initarg :maximum-concurrency :accessor
			task-orchestrator-maximum-concurrency :type (integer 0)
			:documentation
			"The shared child concurrency limit; zero means unlimited.")
   (maximum-depth :initarg :maximum-depth :accessor
		  task-orchestrator-maximum-depth :type (integer 1) :documentation
		  "The maximum child depth below the primary agent.")
   (maximum-runtime-milliseconds :initarg :maximum-runtime-milliseconds
				 :accessor task-orchestrator-maximum-runtime-milliseconds :type
				 (integer 0) :documentation
				 "The wall-clock cap for one child, or zero when disabled.")
   (active-count :initform 0 :accessor task-orchestrator-active-count
		 :type (integer 0) :documentation
		 "The children currently holding a concurrency permit.")
   (next-index :initform 0 :accessor task-orchestrator-next-index :type
               (integer 0) :documentation
               "The monotonically increasing child identity source.")
   (names :initform (make-hash-table :test #'equal) :reader
          task-orchestrator-names :type hash-table :documentation
          "Lowercase child identifiers reserved for this session.")
   (jobs :initform (make-hash-table :test #'equal) :reader
         task-orchestrator-jobs :type hash-table :documentation
         "Task identifiers mapped to live and completed jobs.")
   (listeners :initform nil :accessor task-orchestrator-listeners :type
              list :documentation
              "Callbacks receiving portable task lifecycle and progress events."))
  (:documentation
   "Session-scoped child identity, concurrency, event, and job state."))

(defclass task-job nil
  ((orchestrator :initarg :orchestrator :reader task-job-orchestrator
		 :type task-orchestrator :documentation
		 "The session orchestrator owning this job.")
   (identity :initarg :identity :reader task-job-identity :type list
             :documentation "The stable child identity plist.")
   (definition :initarg :definition :reader task-job-definition :type
               task-agent-definition :documentation
               "The resolved child role and policy.")
   (item :initarg :item :reader task-job-item :type list :documentation
         "The normalized assignment plist.")
   (parent-agent :initarg :parent-agent :reader task-job-parent-agent
		 :type agent :documentation
		 "The parent session supplying provider and workspace context.")
   (parent-call-id :initarg :parent-call-id :initform nil :reader
		   task-job-parent-call-id :type (option string) :documentation
		   "The task.run function call that created this child.")
   (detached-p :initarg :detached-p :reader task-job-detached-p :type
               boolean :documentation
               "True when the parent did not wait for this child.")
   (lock :initform (make-lock "Autolith task job") :reader
         task-job-lock :documentation
         "The lock protecting mutable job lifecycle fields.")
   (state :initform :queued :accessor task-job-state :type keyword
          :documentation
          "The queued, running, completed, failed, or aborted state.")
   (thread :initform nil :accessor task-job-thread :type t
           :documentation "The supervisory thread executing this job.")
   (result :initform nil :accessor task-job-result :type t
           :documentation "The final portable SingleResult-style plist.")
   (condition :initform nil :accessor task-job-condition :type t
              :documentation
              "The unexpected condition that failed this job, when any.")
   (cancellation-reason :initform nil :accessor
			task-job-cancellation-reason :type (option keyword) :documentation
			"The structured cancellation reason requested by a controller.")
   (progress :initform (make-instance 'task-progress) :reader
             task-job-progress :type task-progress :documentation
             "The normalized progress visible to job inspection.")
   (created-at :initform (get-internal-real-time) :reader
               task-job-created-at :type integer :documentation
               "The internal real time at job creation.")
   (started-at :initform nil :accessor task-job-started-at :type t
               :documentation "The internal real time at child execution start.")
   (ended-at :initform nil :accessor task-job-ended-at :type t
             :documentation "The internal real time at terminal completion."))
  (:documentation "One synchronous or detached child-agent execution."))

(defclass task-child-agent (agent)
  ((definition :initarg :definition :reader task-child-agent-definition
               :type task-agent-definition :documentation
               "The role and policy configuring this child.")
   (identity :initarg :identity :reader task-child-agent-identity :type
             list :documentation "The stable identity of this child.")
   (depth :initarg :depth :reader task-child-agent-depth :type
          (integer 1) :documentation
          "The explicit child depth below the primary agent.")
   (completion :initarg :completion :reader task-child-agent-completion
               :type task-completion :documentation
               "The required terminal yield state.")
   (orchestrator :initarg :orchestrator :reader
		 task-child-agent-orchestrator :type task-orchestrator
		 :documentation "The shared session task orchestrator.")
   (job :initarg :job :reader task-child-agent-job :type task-job
        :documentation
        "The lifecycle and progress record for this child."))
  (:documentation
   "A real in-process agent session that must finish through yield.submit."))

(defclass task-tool-result (tool-result)
  ((details :initarg :details :reader task-tool-result-details
            :reader tool-result-details :type t
            :documentation
            "Portable machine-readable task or job orchestration details."))
  (:documentation
   "A normal tool result carrying structured orchestration metadata."))

(defclass task-run-tool (tool)
  ((orchestrator :initarg :orchestrator :reader
		 task-run-tool-orchestrator :type task-orchestrator :documentation
		 "The session-scoped task orchestrator used by this tool."))
  (:documentation
   "Spawn one child agent or a concurrency-limited batch."))

(defclass task-job-tool (tool)
  ((orchestrator :initarg :orchestrator :reader
		 task-job-tool-orchestrator :type task-orchestrator :documentation
		 "The session-scoped task orchestrator inspected by this tool."))
  (:documentation "Inspect, wait for, or cancel detached task jobs."))

(defclass task-yield-tool (tool) nil
  (:documentation
   "Submit the required terminal result from a child agent."))

(defun task--environment-integer (name fallback &key minimum)
  "Return validated integer environment NAME or FALLBACK."
  (let ((value (uiop/os:getenv name)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (if (and (integerp parsed) (or (null minimum) (>= parsed minimum)))
		  parsed
		  fallback))
          (error nil fallback))
        fallback)))

(defun task-orchestrator-create ()
  "Create an orchestrator from the current task environment settings."
  (make-instance 'task-orchestrator :maximum-concurrency
                 (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                            +task-default-maximum-concurrency+
                                            :minimum 0)
                 :maximum-depth
                 (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                            +task-default-maximum-depth+
                                            :minimum 1)
                 :maximum-runtime-milliseconds
                 (task--environment-integer "AUTOLITH_TASK_MAX_RUNTIME_MS" 0
                                            :minimum 0)))

(defun task-orchestrator-refresh (orchestrator)
  "Apply current environment limits to ORCHESTRATOR and wake queued children."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-maximum-concurrency orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                     +task-default-maximum-concurrency+
                                     :minimum 0)
          (task-orchestrator-maximum-depth orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                     +task-default-maximum-depth+ :minimum 1)
          (task-orchestrator-maximum-runtime-milliseconds orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_RUNTIME_MS" 0
                                     :minimum 0))
    (loop repeat (max 1 (task-orchestrator-maximum-concurrency orchestrator))
          do (condition-notify
              (task-orchestrator-condition-variable orchestrator))))
  orchestrator)

(defun task-orchestrator-add-listener (orchestrator listener)
  "Register LISTENER for portable task events and return it."
  (check-type listener function)
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (pushnew listener (task-orchestrator-listeners orchestrator) :test #'eq))
  listener)

(defun task-orchestrator-remove-listener (orchestrator listener)
  "Remove LISTENER from ORCHESTRATOR."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator)
          (remove listener (task-orchestrator-listeners orchestrator) :test
                  #'eq)))
  nil)

(defun task-orchestrator-emit (orchestrator channel payload)
  "Deliver portable CHANNEL and PAYLOAD to a snapshot of listeners."
  (let ((listeners
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (copy-list (task-orchestrator-listeners orchestrator)))))
    (dolist (listener listeners)
      (handler-case (funcall listener channel payload) (error nil nil))))
  nil)

(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((text (string-downcase (task--trim (or value ""))))
         (mapped
          (map 'string
               (lambda (character)
                 (if (or (alphanumericp character)
                         (member character '(#\HYPHEN-MINUS #\LOW_LINE) :test
                                 #'char=))
                     character
                     #\HYPHEN-MINUS))
               text))
         (trimmed (string-trim '(#\HYPHEN-MINUS) mapped)))
    (and (non-empty-string-p trimmed) trimmed)))

(defun task-orchestrator-create-identity
    (orchestrator requested-name agent-type)
  "Reserve and return a stable unique child identity plist."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (incf (task-orchestrator-next-index orchestrator))
    (let* ((index (task-orchestrator-next-index orchestrator))
           (adjectives
            #("amber" "brisk" "calm" "clear" "keen" "quiet" "rapid" "steady"
              "vivid" "wise"))
           (nouns
            #("badger" "falcon" "heron" "lynx" "otter" "raven" "sparrow" "tern"
              "wolf" "wren"))
           (generated
            (format nil "~A-~A"
                    (aref adjectives (mod (1- index) (length adjectives)))
                    (aref nouns
                          (mod (floor (1- index) (length adjectives))
                               (length nouns)))))
           (base (or (task--identifier-fragment requested-name) generated))
           (candidate base)
           (suffix 1))
      (loop while (gethash candidate (task-orchestrator-names orchestrator))
            do (incf suffix) (setf candidate (format nil "~A-~D" base suffix)))
      (setf (gethash candidate (task-orchestrator-names orchestrator)) t)
      (list :id candidate :display-name (or requested-name candidate)
            :agent-type agent-type :index index))))

(defun task-orchestrator-acquire (orchestrator)
  "Wait for and consume one shared child concurrency permit."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (loop for maximum = (task-orchestrator-maximum-concurrency orchestrator)
          while (and (plusp maximum)
                     (>= (task-orchestrator-active-count orchestrator)
                         maximum))
          do (condition-wait
              (task-orchestrator-condition-variable orchestrator)
              (task-orchestrator-lock orchestrator)))
    (incf (task-orchestrator-active-count orchestrator)))
  t)

(defun task-orchestrator-release (orchestrator)
  "Release one shared child concurrency permit."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (when (plusp (task-orchestrator-active-count orchestrator))
      (decf (task-orchestrator-active-count orchestrator)))
    (condition-notify (task-orchestrator-condition-variable orchestrator)))
  nil)

(defun task--milliseconds-between (start end)
  "Return elapsed milliseconds between internal real times START and END."
  (round (* 1000 (- end start)) internal-time-units-per-second))

(defun task-progress-append-output (progress text)
  "Append streamed TEXT while retaining only a bounded tail."
  (with-lock-held ((task-progress-lock progress))
    (let* ((combined
            (concatenate 'string (task-progress-output-tail progress) text))
           (start (max 0 (- (length combined) +task-progress-output-limit+))))
      (setf (task-progress-output-tail progress) (subseq combined start)
            (task-progress-updated-at progress) (get-internal-real-time))))
  nil)

(defun task-progress-note-status (job status details)
  "Update JOB's normalized progress from one child observer STATUS event."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (case status
        (:provider-request-started
         (setf (task-progress-request-count progress)
               (or (getf details :request-number)
                   (1+ (task-progress-request-count progress)))))
        (:provider-request-completed
         (setf (task-progress-usage progress) (getf details :usage)))
        (:tool-call-started
         (setf (task-progress-current-tool progress) (getf details :tool)))
        (:tool-call-completed
         (let ((tool (getf details :tool)))
           (when tool
             (push tool (task-progress-recent-tools progress))
             (setf (task-progress-recent-tools progress)
                   (subseq (task-progress-recent-tools progress) 0
                           (min 8
                                (length
                                 (task-progress-recent-tools progress)))))))
         (setf (task-progress-current-tool progress) nil)))
      (setf (task-progress-updated-at progress) (get-internal-real-time)))
    (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-progress
                            (list :id (getf (task-job-identity job) :id)
                                  :status (task-progress-status progress)
                                  :current-tool
                                  (task-progress-current-tool progress)
                                  :request-count
                                  (task-progress-request-count progress))))
  nil)

(defun task-progress-snapshot (job)
  "Return a portable snapshot of JOB's current progress."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (list :id (getf (task-job-identity job) :id) :agent
            (task-agent-definition-name (task-job-definition job)) :status
            (task-progress-status progress) :current-tool
            (task-progress-current-tool progress) :recent-tools
            (reverse (copy-list (task-progress-recent-tools progress)))
            :recent-output (task-progress-output-tail progress) :request-count
            (task-progress-request-count progress) :usage
            (task-progress-usage progress) :duration-ms
            (and (task-progress-started-at progress)
                 (task--milliseconds-between
                  (task-progress-started-at progress)
                  (or (task-job-ended-at job) (get-internal-real-time))))
            :model
            (configuration-model
             (task-configuration-for-definition
              (agent-configuration (task-job-parent-agent job))
              (task-job-definition job)))))))

(defun task-job-terminal-p (job)
  "Return true when JOB cannot make another state transition."
  (member (task-job-state job) '(:completed :failed :aborted) :test #'eq))

(defun task-job-snapshot (job)
  "Return JOB's portable lifecycle, progress, and result snapshot."
  (with-lock-held ((task-job-lock job))
    (list :job-id (getf (task-job-identity job) :id) :type :task :state
          (task-job-state job) :detached (task-job-detached-p job) :agent
          (task-agent-definition-name (task-job-definition job)) :assignment
          (getf (task-job-item job) :task) :progress
          (task-progress-snapshot job) :result (task-job-result job)
          :cancellation-reason (task-job-cancellation-reason job))))

(defun task-orchestrator-find-job (orchestrator identifier)
  "Return IDENTIFIER's job or signal a typed task error."
  (let ((job
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (gethash identifier (task-orchestrator-jobs orchestrator)))))
    (or job
        (error 'task-error :message
               (format nil "No task job named ~A exists." identifier)
               :tool-name "job.get" :task-id identifier))))

(defun task-orchestrator-list-jobs (orchestrator)
  "Return all jobs sorted by child index."
  (let ((jobs nil))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (maphash
       (lambda (identifier job) (declare (ignore identifier)) (push job jobs))
       (task-orchestrator-jobs orchestrator)))
    (sort jobs #'< :key (lambda (job) (getf (task-job-identity job) :index)))))

(defun task-job-cancel (job reason)
  "Request cancellation REASON for JOB and interrupt its supervisor."
  (let ((thread nil) (cancel-p nil))
    (with-lock-held ((task-job-lock job))
      (unless (task-job-terminal-p job)
        (setf (task-job-cancellation-reason job) reason
              thread (task-job-thread job)
              cancel-p t)))
    (when (and cancel-p thread (thread-alive-p thread))
      (interrupt-thread thread
                        (lambda ()
                          (error 'task-aborted :message
                                 (format nil "Task ~A was ~A."
                                         (getf (task-job-identity job) :id)
                                         reason)
                                 :reason reason))))
    cancel-p))

(defun task-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS for JOB, returning its current snapshot."
  (let ((deadline
         (and timeout-seconds
              (+ (get-internal-real-time)
                 (* timeout-seconds internal-time-units-per-second)))))
    (loop until (with-lock-held ((task-job-lock job))
                  (task-job-terminal-p job))
          when (and deadline (>= (get-internal-real-time) deadline)) return nil
          do (sleep 0.05)))
  (task-job-snapshot job))
(defun task-parent-depth (agent)
  "Return AGENT's explicit task depth, treating the primary agent as zero."
  (if (typep agent 'task-child-agent)
      (task-child-agent-depth agent)
      0))

(defun task-parent-spawn-policy (agent)
  "Return AGENT's effective child-agent spawn policy."
  (if (typep agent 'task-child-agent)
      (task-agent-definition-spawns (task-child-agent-definition agent))
      :all))

(defun task-parent-can-spawn-p (agent child-name orchestrator)
  "Return true when AGENT may create CHILD-NAME at the configured depth."
  (let ((policy (task-parent-spawn-policy agent)))
    (and
     (< (task-parent-depth agent)
        (task-orchestrator-maximum-depth orchestrator))
     (or (eq policy :all)
         (and (listp policy) (member child-name policy :test #'string-equal)))
     t)))

(defun task--tool-spec-matches-p (spec tool)
  "Return true when agent tool SPEC permits TOOL."
  (let* ((normalized (string-downcase spec))
         (canonical (string-downcase (tool-canonical-name tool)))
         (namespace (string-downcase (tool-namespace tool))))
    (or (string= normalized canonical) (string= normalized namespace)
        (string= normalized (format nil "~A.*" namespace))
        (and (string= normalized "read")
             (member canonical '("fs.read" "fs.list") :test #'string=))
        (and (member normalized '("bash" "exec" "grep" "glob") :test #'string=)
             (string= canonical "shell.run"))
        (and (string= normalized "lisp") (string= namespace "lisp"))
        (and (string= normalized "task") (string= canonical "task.run"))
        (and (string= normalized "job") (string= namespace "job")))))

(defun task--definition-allows-tool-p (definition tool)
  "Return true when DEFINITION permits ordinary TOOL."
  (let ((specs (task-agent-definition-tools definition))
        (namespace (tool-namespace tool)))
    (and (member namespace '("fs" "shell" "lisp") :test #'string=)
         (or (null specs)
             (some (lambda (spec) (task--tool-spec-matches-p spec tool))
                   specs)))))

(defun task-child-tool-registry (parent-registry definition orchestrator depth)
  "Build a restricted child registry with yield and structurally bounded spawning."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools parent-registry))
      (when (task--definition-allows-tool-p definition tool)
        (tool-registry-register registry tool)))
    (when
        (and (task-agent-definition-spawns definition)
             (< depth (task-orchestrator-maximum-depth orchestrator)))
      (let ((task-tool (tool-registry-find parent-registry "task" "run")))
        (when task-tool (tool-registry-register registry task-tool)))
      (dolist (tool (tool-registry-tools parent-registry))
        (when (string= (tool-namespace tool) "job")
          (tool-registry-register registry tool))))
    (tool-registry-register registry
                            (make-instance 'task-yield-tool :namespace "yield"
                                           :name "submit" :description
                                           "Submit the required terminal child result. Call exactly once when the assignment is complete or cannot continue."
                                           :parameters
                                           (tool-object-schema
                                            (json-object "status"
                                                         (json-object "type"
                                                                      "string"
                                                                      "description"
                                                                      "Terminal result status."
                                                                      "enum"
                                                                      (json-array
                                                                       "success"
                                                                       "failed"
                                                                       "aborted"))
                                                         "text"
                                                         (tool-string-property
                                                          "Human-readable result for the parent; include concrete findings or changes.")
                                                         "data"
                                                         (json-object
                                                          "description"
                                                          "Optional structured result matching the agent output definition.")
                                                         "error"
                                                         (tool-string-property
                                                          "Failure or abort explanation when status is not success.")
                                                         "label"
                                                         (tool-string-property
                                                          "Optional short result label."))
                                            '("status"))))
    registry))

(defun task--model-alias (alias parent-model)
  "Resolve one child model ALIAS relative to PARENT-MODEL."
  (let ((value (string-downcase alias)))
    (cond
      ((member value '("@task" "@parent" "@auto") :test #'string=) parent-model)
      ((string= value "@smol") "gpt-5.6-luna")
      ((member value '("@slow" "@designer") :test #'string=) "gpt-5.6-terra")
      ((member alias +supported-models+ :test #'string=) alias) (t nil))))

(defun task--thinking-effort (level parent-effort)
  "Resolve child reasoning LEVEL to a supported provider effort."
  (let ((value (and level (string-downcase level))))
    (cond
      ((or (null value) (member value '("auto" "@auto") :test #'string=))
       parent-effort)
      ((string= value "minimal") "low")
      ((member value +supported-reasoning-efforts+ :test #'string=) value)
      (t parent-effort))))

(defun task-configuration-for-definition (parent-configuration definition)
  "Copy PARENT-CONFIGURATION with DEFINITION's model, effort, and web policy."
  (let* ((parent-model (configuration-model parent-configuration))
         (model
          (or
           (loop for candidate in (task-agent-definition-models definition)
                 for resolved = (task--model-alias candidate parent-model)
                 when resolved return resolved)
           parent-model))
         (effort
          (task--thinking-effort
           (task-agent-definition-thinking-level definition)
           (configuration-reasoning-effort parent-configuration)))
         (web-enabled-p
          (or (null (task-agent-definition-tools definition))
              (member "web_search" (task-agent-definition-tools definition)
                      :test #'string-equal))))
    (make-instance 'configuration :source-root
                   (configuration-source-root parent-configuration)
                   :working-directory
                   (configuration-working-directory parent-configuration)
                   :config-root (configuration-config-root parent-configuration)
                   :data-root (configuration-data-root parent-configuration)
                   :state-root (configuration-state-root parent-configuration)
                   :cache-root (configuration-cache-root parent-configuration)
                   :codex-auth-path
                   (configuration-codex-auth-path parent-configuration) :model
                   model :reasoning-effort effort :web-search-mode
                   (if web-enabled-p
                       (configuration-web-search-mode parent-configuration)
                       "disabled")
                   :context-window (configuration--context-window-for model)
                   :compaction-threshold-percent
                   (configuration-compaction-threshold-percent
                    parent-configuration)
                   :provider-endpoint
                   (configuration-provider-endpoint parent-configuration))))

(defun task-definition-request-budget (definition)
  "Return the soft provider-request budget for DEFINITION."
  (task--environment-integer "AUTOLITH_TASK_REQUEST_BUDGET"
                             (if (member
                                  (task-agent-definition-name definition)
                                  '("scout" "sonic") :test #'string=)
                                 100
                                 200)
                             :minimum 1))

(defun task-output-definition-text (definition)
  "Return DEFINITION's output contract as prompt text, or NIL."
  (let ((output (task-agent-definition-output definition)))
    (cond ((null output) nil) ((json-object-p output) (json-encode output))
          (t (princ-to-string output)))))

(defun task-child-goal-context (job child-configuration)
  "Build the transient developer instructions for JOB's child session."
  (let* ((definition (task-job-definition job))
         (identity (task-job-identity job))
         (item (task-job-item job))
         (context (getf item :context))
         (output (task-output-definition-text definition))
         (skills (task-agent-definition-autoload-skills definition)))
    (format nil
            "You are child agent ~A of type ~A, depth ~D. Your specialized role follows.~2%~A~@[~2%Shared parent context:~%~A~]~@[~2%Autoload these skills when relevant: ~{~A~^, ~}.~]~@[~2%Your yield data must satisfy this output definition:~%~A~]~2%You are not the primary Autolith session. self.* tools are deliberately unavailable. Work only in ~A. Complete the assignment in the user message. You MUST end by calling yield.submit exactly once. A normal assistant stop without yield is a failed child run. Put the useful parent-facing answer in yield.text and structured data in yield.data when requested."
            (getf identity :id) (task-agent-definition-name definition)
            (1+ (task-parent-depth (task-job-parent-agent job)))
            (task-agent-definition-system-prompt definition) context skills
            output
            (namestring
             (configuration-working-directory child-configuration)))))

(defun task-json-schema-valid-p (value schema)
  "Return true when VALUE satisfies the supported structural subset of SCHEMA."
  (labels ((matches-type-p (candidate type)
             (cond ((string= type "object") (json-object-p candidate))
                   ((string= type "array") (vectorp candidate))
                   ((string= type "string") (stringp candidate))
                   ((string= type "integer") (integerp candidate))
                   ((string= type "number") (numberp candidate))
                   ((string= type "boolean")
                    (or (eq candidate t) (eq candidate false)
                        (null candidate)))
                   ((string= type "null") (null candidate)) (t t)))
           (validate (candidate rule)
             (if (not (json-object-p rule))
                 t
                 (let ((type (json-get rule "type"))
                       (enum (json-get rule "enum")))
                   (and
                    (or (null type)
                        (and (stringp type) (matches-type-p candidate type)))
                    (or (null enum)
                        (and (vectorp enum)
                             (find candidate enum :test #'equal)))
                    (if (json-object-p candidate)
                        (let ((required (json-get rule "required"))
                              (properties (json-get rule "properties")))
                          (and
                           (or (null required)
                               (loop for key across required
                                     always (nth-value 1
                                                       (gethash key
                                                                candidate))))
                           (or (not (json-object-p properties))
                               (loop for key being the hash-keys of properties using (hash-value
                                                                                      child-rule)
                                     always (multiple-value-bind
                                                  (child present-p)
                                                (gethash key candidate)
                                              (or (not present-p)
                                                  (validate child
							    child-rule)))))))
                        t)
                    (if (and (vectorp candidate) (json-get rule "items"))
                        (loop for child across candidate
                              always (validate child (json-get rule "items")))
                        t))))))
    (validate value schema)))

(defmethod tool-execute
    ((tool task-yield-tool) (context tool-context) arguments)
  "Validate and record one terminal child yield."
  (declare (ignore tool))
  (let ((agent (tool-context-agent context)))
    (unless (typep agent 'task-child-agent)
      (error 'task-yield-error :message
             "yield.submit is available only inside a child agent." :tool-name
             "yield.submit"))
    (let* ((completion (task-child-agent-completion agent))
           (status-text (tool-argument arguments "status" :required t))
           (status
            (and (stringp status-text)
                 (find-symbol (string-upcase status-text) :keyword)))
           (text (tool-argument arguments "text"))
           (data (tool-argument arguments "data"))
           (failure (tool-argument arguments "error"))
           (label (tool-argument arguments "label"))
           (output-definition
            (task-agent-definition-output (task-child-agent-definition agent))))
      (when (task-completion-called-p completion)
        (error 'task-yield-error :message
               "This child already submitted its terminal yield." :tool-name
               "yield.submit" :task-id
               (getf (task-child-agent-identity agent) :id)))
      (unless (member status '(:success :failed :aborted) :test #'eq)
        (error 'task-yield-error :message
               "Yield status must be success, failed, or aborted." :tool-name
               "yield.submit" :task-id
               (getf (task-child-agent-identity agent) :id)))
      (when (and output-definition (null data))
        (error 'task-yield-error :message
               "This agent definition requires structured yield data."
               :tool-name "yield.submit" :task-id
               (getf (task-child-agent-identity agent) :id)))
      (when
          (and (json-object-p output-definition)
               (not (task-json-schema-valid-p data output-definition)))
        (error 'task-yield-error :message
               "The supplied yield data does not satisfy the agent output schema."
               :tool-name "yield.submit" :task-id
               (getf (task-child-agent-identity agent) :id)))
      (setf (task-completion-called-p completion) t
            (task-completion-status completion) status
            (task-completion-text completion) (and (stringp text) text)
            (task-completion-data completion) (agent--portable-value data)
            (task-completion-error completion) (and (stringp failure) failure)
            (task-completion-label completion) (and (stringp label) label))
      (tool-success
       "Terminal yield accepted. The child session will now stop."))))

(defun task-child--yield-only-schemas (agent)
  "Return only AGENT's yield namespace schema for hard finalization."
  (let ((tool
         (tool-registry-find (agent-tool-registry agent) "yield" "submit")))
    (if tool
        (vector
         (json-object "type" "namespace" "name" "yield" "description"
                      "Required child completion protocol." "tools"
                      (vector (tool-provider-schema tool))))
        #())))

(defun task-child--execute-tool-calls (agent calls observer tool-round)
  "Execute child CALLS, rejecting calls after the first accepted yield."
  (loop for remaining on calls
        for call = (first remaining)
        for call-id = (json-get call "call_id")
        for tool-name = (function-call-canonical-name call)
        do (let ((context
                  (make-instance 'tool-context :configuration
                                 (agent-configuration agent) :worker
                                 (agent-worker agent) :conversation
                                 (agent-conversation agent) :agent agent
                                 :observer observer :call-id call-id)))
             (agent-observer-status observer :tool-call-started
                                    (list :tool-round tool-round :call-id
                                          call-id :tool tool-name))
             (let ((result
                    (tool-registry-execute-call (agent-tool-registry agent)
                                                call context)))
               (conversation-append-tool-result (agent-conversation agent)
                                                call-id :tool-name tool-name
                                                :output (tool-result-content result)
                                                :success-p (tool-result-success-p result))
               (agent-observer-status observer :tool-call-completed
                                      (list :tool-round tool-round :call-id
                                            call-id :tool tool-name :success-p
                                            (tool-result-success-p result)
                                            :output
                                            (tool-result-content result)
                                            :details
                                            (and
                                             (typep result 'task-tool-result)
                                             (task-tool-result-details
                                              result))))))
        when (task-completion-called-p (task-child-agent-completion agent))
        do (when (rest remaining)
             (agent--reject-tool-calls agent (rest remaining) observer
                                       :tool-round tool-round :message
                                       "This call was not executed because the child already yielded.")) (return))
  nil)

(defun task-child--run-provider-loop (agent observer goal-context)
  "Run a bounded child turn until yield.submit or hard finalization."
  (let ((seen-call-identifiers (make-hash-table :test #'equal))
        (request-number 0)
        (tool-rounds 0)
        (tool-calls 0))
    (loop
     (when (agent--should-compact-p agent)
       (agent-compact-conversation agent observer))
     (incf request-number)
     (let ((budget-state (agent--budget-state agent request-number)))
       (when (eq budget-state :warning)
         (agent-observer-status observer :turn-budget-warning
                                (list :provider-step request-number
                                      :maximum-provider-steps
                                      (agent-maximum-provider-steps agent))))
       (agent-observer-status observer :provider-request-started
                              (list :request-number request-number :tool-rounds
                                    tool-rounds :turn-budget-state
                                    budget-state))
       (let* ((conversation (agent-conversation agent))
              (provider-tools
               (if (eq budget-state :finalization)
                   (task-child--yield-only-schemas agent)
                   (tool-registry-provider-schemas
                    (agent-tool-registry agent))))
              (result
               (provider-stream-turn (agent-provider agent) conversation
                                     provider-tools
                                     (agent--provider-event-callback observer)
                                     :turn-budget-state
                                     (if (eq budget-state :finalization)
                                         :normal
                                         budget-state)
                                     :goal-context goal-context))
              (calls (provider-result-tool-calls result)))
         (agent--validate-tool-call-identifiers agent calls
                                                :seen-call-identifiers seen-call-identifiers
                                                :request-number request-number)
         (agent--persist-provider-result agent result request-number)
         (setf (conversation-turn-state conversation)
               (provider-result-turn-state result))
         (agent-observer-status observer :provider-request-completed
                                (list :request-number request-number
                                      :response-id
                                      (provider-result-response-id result)
                                      :usage
                                      (agent--portable-value
                                       (provider-result-usage result))
                                      :output-item-count
                                      (length
                                       (provider-result-output-items result))
                                      :tool-call-count (length calls)
                                      :turn-completion
                                      (provider-result-turn-completion result)
                                      :turn-budget-state budget-state))
         (cond
           ((and calls
                 (> (+ tool-calls (length calls))
                    (agent-maximum-tool-calls agent)))
            (let ((message
                   (format nil
                           "This call was not executed because the child reached its ~:D-call tool budget."
                           (agent-maximum-tool-calls agent))))
              (agent--reject-tool-calls agent calls observer :tool-round
					(1+ tool-rounds) :message message)
              (agent--signal-budget-exhausted
                 agent :provider-step request-number :tool-calls tool-calls
                 :reason :tool-call-limit :message message)))
           (calls (incf tool-rounds) (incf tool-calls (length calls))
		  (task-child--execute-tool-calls agent calls observer tool-rounds)))
         (when (task-completion-called-p (task-child-agent-completion agent))
           (agent-observer-status observer :turn-completed
                                  (list :provider-requests request-number
                                        :tool-rounds tool-rounds :tool-calls
                                        tool-calls :yielded-p t :response-id
                                        (provider-result-response-id result)))
           (return result))
         (when (eq budget-state :finalization)
           (agent-observer-status observer :turn-completed
                                  (list :provider-requests request-number
                                        :tool-rounds tool-rounds :tool-calls
                                        tool-calls :yielded-p nil
                                        :budget-finalization-p t :response-id
                                        (provider-result-response-id result)))
           (return result))
         (when (null calls)
           (agent-observer-status observer :provider-follow-up
                                  (list :request-number request-number :reason
                                        :yield-required))))))))

(defmethod agent-run-user-turn
    ((agent task-child-agent) (content string)
     &key (observer (make-instance 'agent-observer)) goal-context)
  "Run one serialized child turn until its explicit terminal yield."
  (unless (non-empty-string-p content)
    (error 'agent-loop-error :message
           "A child assignment requires non-empty content." :conversation-id
           (conversation-identifier (agent-conversation agent)) :request-number
           nil))
  (with-lock-held ((agent-turn-lock agent))
    (let ((conversation (agent-conversation agent)))
      (when (agent--should-compact-p agent)
        (agent-compact-conversation agent observer))
      (conversation-append-user-message conversation content)
      (unwind-protect
           (task-child--run-provider-loop agent observer goal-context)
        (setf (conversation-turn-state conversation) nil)))))

(defun task--utf8-length (text)
  "Return the UTF-8 byte length of TEXT on the supported SBCL runtime."
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun task--bounded-output (text)
  "Bound TEXT by configured UTF-8 bytes and lines, marking truncation."
  (let* ((maximum-bytes
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_BYTES"
                                     +task-default-maximum-output-bytes+
                                     :minimum 1))
         (maximum-lines
          (task--environment-integer "AUTOLITH_TASK_MAX_OUTPUT_LINES"
                                     +task-default-maximum-output-lines+
                                     :minimum 1))
         (lines (task--split-lines (or text "")))
         (line-bounded-p (> (length lines) maximum-lines))
         (line-text
          (format nil "~{~A~^~%~}"
                  (subseq lines 0 (min maximum-lines (length lines)))))
         (byte-bounded-p (> (task--utf8-length line-text) maximum-bytes))
         (bounded
          (if byte-bounded-p
              (let ((low 0) (high (length line-text)))
                (loop while (< low high)
                      for middle = (ceiling (+ low high) 2)
                      if (<= (task--utf8-length (subseq line-text 0 middle))
                             maximum-bytes)
                      do (setf low middle) else
                      do (setf high (1- middle)))
                (subseq line-text 0 low))
              line-text)))
    (if (or line-bounded-p byte-bounded-p)
        (format nil "~A~%... [task output truncated]" bounded)
        bounded)))

(defun task--artifact-root (configuration parent-conversation)
  "Return the artifact directory for PARENT-CONVERSATION's task children."
  (merge-pathnames
   (format nil "tasks/~A/" (conversation-identifier parent-conversation))
   (configuration-data-root configuration)))

(defun task--write-result-artifact (job result)
  "Atomically persist portable RESULT and return its pathname."
  (let* ((configuration (agent-configuration (task-job-parent-agent job)))
         (root
          (task--artifact-root configuration
                               (agent-conversation
                                (task-job-parent-agent job))))
         (identifier (getf (task-job-identity job) :id))
         (target
          (merge-pathnames (make-pathname :name identifier :type "sexp") root))
         (temporary
          (merge-pathnames
           (make-pathname :name
                          (format nil ".~A.~A" identifier (make-identifier))
                          :type "tmp")
           root)))
    (ensure-directories-exist target)
    (unwind-protect
         (progn
           (with-open-file
               (stream temporary :direction :output :if-exists :supersede
		       :if-does-not-exist :create :external-format :utf-8)
             (let ((*print-readably* t) (*print-pretty* t) (*print-circle* t))
               (prin1 result stream)
               (terpri stream)
               (finish-output stream)))
           (uiop/filesystem:rename-file-overwriting-target temporary target)
           target)
      (when (probe-file temporary) (delete-file temporary)))))

(defun task--result-output (completion progress result)
  "Select bounded child output from COMPLETION, PROGRESS, and provider RESULT."
  (let ((text
         (or (task-completion-text completion)
             (provider-result-assistant-text result)
             (let ((tail (task-progress-output-tail progress)))
               (and (non-empty-string-p tail) tail)))))
    (cond ((non-empty-string-p text) (task--bounded-output text))
          ((task-completion-data completion)
           (task--bounded-output
            (json-encode (task-completion-data completion))))
          ((plusp (task-progress-request-count progress))
           (format nil "(no output) after ~D req"
                   (task-progress-request-count progress)))
          (t "(no output)"))))

(defun task--assemble-child-result
    (job provider-result child conversation completion)
  "Assemble and persist one SingleResult-style plist for a completed child."
  (let* ((progress (task-job-progress job))
         (status
          (if (task-completion-called-p completion)
              (task-completion-status completion)
              :failed))
         (output (task--result-output completion progress provider-result))
         (duration
          (task--milliseconds-between
           (or (task-job-started-at job) (task-job-created-at job))
           (get-internal-real-time)))
         (base
          (list :id (getf (task-job-identity job) :id) :name
                (getf (task-job-identity job) :display-name) :agent
                (task-agent-definition-name (task-job-definition job))
                :agent-source
                (task-agent-definition-source (task-job-definition job))
                :assignment (getf (task-job-item job) :task) :status status
                :output output :error
                (or (task-completion-error completion)
                    (and (not (task-completion-called-p completion))
                         "Child stopped without calling yield.submit."))
                :yielded-p (task-completion-called-p completion)
                :structured-output (task-completion-data completion) :label
                (task-completion-label completion) :request-count
                (task-progress-request-count progress) :usage
                (task-progress-usage progress) :duration-ms duration :model
                (configuration-model (agent-configuration child))
                :conversation-file
                (namestring (conversation-pathname conversation)) :detached
                (task-job-detached-p job)))
         (artifact (task--write-result-artifact job base)))
    (append base (list :output-path (namestring artifact)))))

(defun task--failed-result (job status message)
  "Return a portable terminal failure result for JOB."
  (let* ((progress (task-job-progress job))
         (tail (task-progress-output-tail progress))
         (base
          (list :id (getf (task-job-identity job) :id) :name
                (getf (task-job-identity job) :display-name) :agent
                (task-agent-definition-name (task-job-definition job))
                :agent-source
                (task-agent-definition-source (task-job-definition job))
                :assignment (getf (task-job-item job) :task) :status status
                :output
                (if (non-empty-string-p tail)
                    (task--bounded-output tail)
                    "(no output)")
                :error message :yielded-p nil :request-count
                (task-progress-request-count progress) :usage
                (task-progress-usage progress) :duration-ms
                (task--milliseconds-between
                 (or (task-job-started-at job) (task-job-created-at job))
                 (get-internal-real-time))
                :model
                (configuration-model
                 (agent-configuration (task-job-parent-agent job)))
                :detached (task-job-detached-p job))))
    (handler-case
	(append base
		(list :output-path
                      (namestring (task--write-result-artifact job base))))
      (error nil base))))

(defun task-run-child (job)
  "Create and run JOB's real in-process child session through terminal yield."
  (let* ((parent (task-job-parent-agent job))
         (definition (task-job-definition job))
         (orchestrator (task-job-orchestrator job))
         (depth (1+ (task-parent-depth parent)))
         (configuration
          (task-configuration-for-definition (agent-configuration parent)
                                             definition))
         (conversation
          (conversation-create configuration :identifier
                               (format nil "task-~A-~A"
                                       (getf (task-job-identity job) :id)
                                       (make-identifier))))
         (worker (lisp-worker-pool-create configuration))
         (completion (make-instance 'task-completion))
         (registry
          (task-child-tool-registry (agent-tool-registry parent) definition
                                    orchestrator depth))
         (budget (task-definition-request-budget definition))
         (hard-limit (+ (ceiling (* 3 budget) 2) 5))
         (provider
          (provider-with-configuration (agent-provider parent) configuration))
         (child
          (make-instance 'task-child-agent :configuration configuration
                         :provider provider :conversation conversation
                         :tool-registry registry :worker worker
                         :maximum-provider-steps hard-limit
                         :provider-step-warning budget :maximum-tool-calls
                         +default-maximum-tool-calls+ :definition definition
                         :identity (task-job-identity job) :depth depth
                         :completion completion :orchestrator orchestrator :job
                         job))
         (progress (task-job-progress job))
         (observer
          (callback-agent-observer-create :text-callback
                                          (lambda (text)
                                            (task-progress-append-output
                                             progress text))
                                          :reasoning-callback
                                          (lambda (text)
                                            (declare (ignore text))
                                            nil)
                                          :status-callback
                                          (lambda (status details)
                                            (task-progress-note-status job
                                                                       status
                                                                       details)))))
    (unwind-protect
         (let ((result
		(agent-run-user-turn child (getf (task-job-item job) :task)
                                     :observer observer :goal-context
                                     (task-child-goal-context job
                                                              configuration))))
           (task--assemble-child-result job result child conversation
					completion))
      (ignore-errors (lisp-worker-pool-stop-all worker)))))

(defun task-job--set-progress-state (job state)
  "Set JOB's normalized progress STATE."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (setf (task-progress-status progress) state
            (task-progress-updated-at progress) (get-internal-real-time))
      (when (eq state :running)
        (setf (task-progress-started-at progress) (get-internal-real-time)))))
  nil)

(defun task-job--finish (job state result &optional condition)
  "Transition JOB once to terminal STATE with RESULT and optional CONDITION."
  (with-lock-held ((task-job-lock job))
    (unless (task-job-terminal-p job)
      (setf (task-job-state job) state
            (task-job-result job) result
            (task-job-condition job) condition
            (task-job-ended-at job) (get-internal-real-time))))
  (task-job--set-progress-state job state)
  (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-lifecycle
                          (list :id (getf (task-job-identity job) :id) :agent
                                (task-agent-definition-name
                                 (task-job-definition job))
                                :agent-source
                                (task-agent-definition-source
                                 (task-job-definition job))
                                :status state :session-file
                                (getf result :conversation-file)
                                :parent-tool-call-id
                                (task-job-parent-call-id job) :index
                                (getf (task-job-identity job) :index) :detached
                                (task-job-detached-p job)))
  nil)

(defun task-job--watchdog (job)
  "Enforce JOB's configured wall-clock cap without retaining a sleeping timer."
  (let ((milliseconds
         (task-orchestrator-maximum-runtime-milliseconds
          (task-job-orchestrator job))))
    (when (plusp milliseconds)
      (let ((deadline
             (+ (get-internal-real-time)
                (round (* milliseconds internal-time-units-per-second) 1000))))
        (loop until (with-lock-held ((task-job-lock job))
                      (task-job-terminal-p job))
              when (>= (get-internal-real-time) deadline)
              do (task-job-cancel job :timeout) (return)
              do (sleep 0.1)))))
  nil)

(defun task-job--run (job)
  "Acquire shared capacity, run JOB, and publish exactly one terminal result."
  (let ((orchestrator (task-job-orchestrator job)) (acquired-p nil))
    (setf (task-job-thread job) (current-thread))
    (handler-case
	(unwind-protect
             (progn
               (task-orchestrator-acquire orchestrator)
               (setf acquired-p t)
               (with-lock-held ((task-job-lock job))
		 (when (task-job-cancellation-reason job)
		   (error 'task-aborted :message
			  "Task was cancelled before it started." :reason
			  (task-job-cancellation-reason job)))
		 (setf (task-job-state job) :running
                       (task-job-started-at job) (get-internal-real-time)))
               (task-job--set-progress-state job :running)
               (task-orchestrator-emit orchestrator :task-subagent-lifecycle
                                       (list :id (getf (task-job-identity job) :id)
                                             :agent
                                             (task-agent-definition-name
                                              (task-job-definition job))
                                             :agent-source
                                             (task-agent-definition-source
                                              (task-job-definition job))
                                             :status :started :parent-tool-call-id
                                             (task-job-parent-call-id job) :index
                                             (getf (task-job-identity job) :index)
                                             :detached (task-job-detached-p job)))
               (when
		   (plusp
		    (task-orchestrator-maximum-runtime-milliseconds orchestrator))
		 (make-thread (lambda () (task-job--watchdog job)) :name
                              (format nil "Autolith task watchdog ~A"
                                      (getf (task-job-identity job) :id))))
               (let* ((result (task-run-child job))
                      (status (getf result :status))
                      (state
                       (cond ((eq status :success) :completed)
                             ((eq status :aborted) :aborted) (t :failed))))
		 (task-job--finish job state result)))
	  (when acquired-p (task-orchestrator-release orchestrator)))
      (task-aborted (condition)
	(task-job--finish job :aborted
                          (task--failed-result job :aborted
                                               (princ-to-string condition))
                          condition))
      (error (condition)
        (task-job--finish job :failed
                          (task--failed-result job :failed
                                               (princ-to-string condition))
                          condition))))
  nil)

(defun task-orchestrator-start-job
    (orchestrator parent-agent definition item detached-p parent-call-id)
  "Create, register, and start one child JOB."
  (let* ((identity
          (task-orchestrator-create-identity orchestrator (getf item :name)
                                             (task-agent-definition-name
                                              definition)))
         (job
          (make-instance 'task-job :orchestrator orchestrator :identity
                         identity :definition definition :item item
                         :parent-agent parent-agent :parent-call-id
                         parent-call-id :detached-p detached-p)))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (setf (gethash (getf identity :id) (task-orchestrator-jobs orchestrator))
            job))
    (setf (task-job-thread job)
          (make-thread (lambda () (task-job--run job)) :name
                       (format nil "Autolith task ~A" (getf identity :id))))
    job))

(defun task--repair-prose (value)
  "Repair a provider string that was JSON-encoded one extra time."
  (if (not (stringp value))
      value
      (loop with current = value
            repeat 2
            for trimmed = (task--trim current)
            if (and (> (length trimmed) 1)
                    (char= (char trimmed 0) #\QUOTATION_MARK)
                    (char= (char trimmed (1- (length trimmed)))
                           #\QUOTATION_MARK))
            do (handler-case
                   (let ((decoded (json-decode trimmed)))
                     (if (stringp decoded)
			 (setf current decoded)
			 (return current)))
                 (error nil (return current))) else return current
            finally (return current))))

(defun task--combine-context (shared item)
  "Combine optional SHARED and ITEM context without manufacturing instructions."
  (cond
    ((and (non-empty-string-p shared) (non-empty-string-p item))
     (format nil "~A~2%~A" shared item))
    ((non-empty-string-p shared) shared) ((non-empty-string-p item) item)
    (t nil)))

(defun task--normalize-item (object shared-context top-async)
  "Validate and normalize one flat task OBJECT."
  (unless (json-object-p object)
    (error 'task-error :message "Every tasks item must be a JSON object."
           :tool-name "task.run"))
  (let ((task (task--repair-prose (json-get object "task")))
        (name (json-get object "name"))
        (agent (or (json-get object "agent") "task"))
        (context (task--repair-prose (json-get object "context")))
        (isolated (json-get object "isolated"))
        (async
         (multiple-value-bind (value present-p)
             (gethash "async" object)
           (if present-p
               value
               top-async))))
    (unless (non-empty-string-p task)
      (error 'task-error :message
             "Every child requires a non-empty task assignment." :tool-name
             "task.run"))
    (when (and name (not (non-empty-string-p name)))
      (error 'task-error :message
             "A supplied task name must be a non-empty string." :tool-name
             "task.run"))
    (unless (non-empty-string-p agent)
      (error 'task-error :message "A task agent must be a non-empty string."
             :tool-name "task.run"))
    (when isolated
      (error 'task-error :message
             "Worktree isolation is not available in this live-image task port."
             :tool-name "task.run"))
    (list :name name :agent (string-downcase agent) :task task :context
          (task--combine-context shared-context context) :async (and async t)
          :isolated nil)))

(defun task-normalize-arguments (arguments)
  "Validate TASK.RUN ARGUMENTS and return ordinary normalized item plists."
  (when (nth-value 1 (gethash "schema" arguments))
    (error 'task-error :message
           "task.run does not accept a schema field; use the agent output definition."
           :tool-name "task.run"))
  (let* ((tasks (json-get arguments "tasks"))
         (flat-task (json-get arguments "task"))
         (shared-context (task--repair-prose (json-get arguments "context")))
         (top-async (json-get arguments "async"))
         (items
          (cond
            (tasks
             (when flat-task
               (error 'task-error :message
                      "A batch task call cannot also contain top-level task."
                      :tool-name "task.run"))
             (unless (and (vectorp tasks) (plusp (length tasks)))
               (error 'task-error :message
                      "A batch task call requires a non-empty tasks array."
                      :tool-name "task.run"))
             (unless (non-empty-string-p shared-context)
               (error 'task-error :message
                      "A batch task call requires non-empty shared context."
                      :tool-name "task.run"))
             (loop for item across tasks
                   collect (task--normalize-item item shared-context
                                                 top-async)))
            (t (list (task--normalize-item arguments nil top-async))))))
    (let ((names (make-hash-table :test #'equal)))
      (dolist (item items)
        (let ((name (getf item :name)))
          (when name
            (let ((key (string-downcase name)))
              (when (gethash key names)
                (error 'task-error :message
                       (format nil "Task name ~S is duplicated in this batch."
                               name)
                       :tool-name "task.run"))
              (setf (gethash key names) t))))))
    items))

(defun task--resolve-items (parent orchestrator definitions items)
  "Resolve ITEMS to definitions after enforcing parent policy and names."
  (mapcar
   (lambda (item)
     (let* ((name (getf item :agent))
            (definition (task-find-agent-definition definitions name)))
       (unless definition
         (error 'task-error :message
                (format nil
                        "Unknown task agent ~S. Available agents: ~{~A~^, ~}."
                        name (mapcar #'task-agent-definition-name definitions))
                :tool-name "task.run"))
       (unless (task-parent-can-spawn-p parent name orchestrator)
         (error 'task-error :message
                (format nil
                        "Agent ~A may not spawn ~A at depth ~D of maximum ~D."
                        (if (typep parent 'task-child-agent)
                            (task-agent-definition-name
                             (task-child-agent-definition parent))
                            "primary")
                        name (task-parent-depth parent)
                        (task-orchestrator-maximum-depth orchestrator))
                :tool-name "task.run"))
       (list :item item :definition definition :detached
             (and (getf item :async)
                  (not (task-agent-definition-blocking-p definition))))))
   items))

(defun task--result-preview (result)
  "Return a concise parent-facing rendering of one child RESULT."
  (let* ((identifier (getf result :id))
         (agent (getf result :agent))
         (status (getf result :status))
         (output (or (getf result :output) "(no output)"))
         (preview
          (if (> (length output) +task-result-preview-limit+)
              (format nil "~A~%... [see artifact]"
                      (subseq output 0 +task-result-preview-limit+))
              output)))
    (format nil "~A [~A] ~A (~,2Fs)~%~A~@[~%artifact: ~A~]" identifier agent
            status (/ (or (getf result :duration-ms) 0) 1000.0) preview
            (getf result :output-path))))

(defun task--job-start-preview (job)
  "Return a concise detached JOB launch line."
  (format nil "~A [~A] running: ~A" (getf (task-job-identity job) :id)
          (task-agent-definition-name (task-job-definition job))
          (getf (task-job-item job) :task)))

(defun task-tool-result (content details &optional (success-p t))
  "Return bounded CONTENT and portable DETAILS as a task-aware tool result."
  (make-instance 'task-tool-result :content (bounded-string content) :success-p
                 (and success-p t) :details details))

(defmethod tool-execute ((tool task-run-tool) (context tool-context) arguments)
  "Validate, fan out, and aggregate synchronous and detached child agents."
  (let* ((parent (tool-context-agent context))
         (orchestrator
          (task-orchestrator-refresh (task-run-tool-orchestrator tool))))
    (unless (typep parent 'agent)
      (error 'task-error :message
             "task.run requires an executing parent agent context." :tool-name
             "task.run"))
    (let* ((items (task-normalize-arguments arguments))
           (definitions (task-discover-agents (agent-configuration parent)))
           (resolved
            (task--resolve-items parent orchestrator definitions items))
           (ordered
            (append
             (remove-if-not (lambda (entry) (getf entry :detached)) resolved)
             (remove-if (lambda (entry) (getf entry :detached)) resolved)))
           (jobs nil)
           (synchronous nil)
           (detached nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (dolist (entry ordered)
               (let ((job
                      (task-orchestrator-start-job orchestrator parent
                                                   (getf entry :definition)
                                                   (getf entry :item)
                                                   (getf entry :detached)
                                                   (tool-context-call-id
                                                    context))))
		 (push job jobs)
		 (if (getf entry :detached)
                     (push job detached)
                     (push job synchronous))))
             (setf jobs (nreverse jobs)
                   synchronous (nreverse synchronous)
                   detached (nreverse detached))
             (dolist (job synchronous) (task-job-await job nil))
             (let* ((results (mapcar #'task-job-result synchronous))
                    (duration
                     (if jobs
			 (task--milliseconds-between
                          (reduce #'min jobs :key #'task-job-created-at)
                          (get-internal-real-time))
			 0))
                    (content
                     (format nil
                             "~{~A~^~2%~}~@[~2%Detached jobs started:~%~{~A~^~%~}~]"
                             (mapcar #'task--result-preview results)
                             (and detached
                                  (mapcar #'task--job-start-preview detached))))
                    (details
                     (list :project-agents-dir
                           (let ((directory
                                  (task--project-agents-directory
                                   (agent-configuration parent))))
                             (and directory (namestring directory)))
                           :results results :total-duration-ms duration
                           :output-paths
                           (remove nil
                                   (mapcar
                                    (lambda (result) (getf result :output-path))
                                    results))
                           :progress (mapcar #'task-progress-snapshot jobs)
                           :async
                           (and detached
				(list :state :running :job-ids
                                      (mapcar
                                       (lambda (job)
					 (getf (task-job-identity job) :id))
                                       detached)
                                      :type :task)))))
               (setf completed-p t)
               (task-tool-result
		(if (non-empty-string-p content)
                    content
                    "No task results were produced.")
		details)))
        (unless completed-p
          (dolist (job synchronous) (task-job-cancel job :signal)))))))

(defmethod tool-execute ((tool task-job-tool) (context tool-context) arguments)
  "Execute the job operation named by TOOL."
  (declare (ignore context))
  (let* ((orchestrator (task-job-tool-orchestrator tool))
         (operation (tool-name tool)))
    (cond
      ((string= operation "list")
       (let* ((jobs (task-orchestrator-list-jobs orchestrator))
              (snapshots (mapcar #'task-job-snapshot jobs)))
         (task-tool-result
          (if jobs
              (format nil "~{~A~^~%~}"
                      (mapcar
                       (lambda (job)
                         (format nil "~A [~A] ~A"
                                 (getf (task-job-identity job) :id)
                                 (task-agent-definition-name
                                  (task-job-definition job))
                                 (task-job-state job)))
                       jobs))
              "No task jobs exist in this session.")
          (list :jobs snapshots))))
      ((member operation '("get" "wait" "cancel") :test #'string=)
       (let* ((identifier (tool-argument arguments "id" :required t))
              (job (task-orchestrator-find-job orchestrator identifier)))
         (cond
           ((string= operation "cancel")
            (let ((cancelled-p (task-job-cancel job :terminate)))
              (task-tool-result
               (if cancelled-p
                   (format nil "Cancellation requested for ~A." identifier)
                   (format nil "Task ~A is already terminal." identifier))
               (task-job-snapshot job))))
           ((string= operation "wait")
            (let* ((timeout (tool-argument arguments "timeout-seconds"))
                   (snapshot (task-job-await job (or timeout 60))))
              (task-tool-result
               (if (task-job-terminal-p job)
                   (let ((result (task-job-result job)))
                     (task--result-preview result))
                   (format nil "Task ~A is still ~A." identifier
                           (task-job-state job)))
               snapshot)))
           (t
            (let ((snapshot (task-job-snapshot job)))
              (task-tool-result
               (if (task-job-terminal-p job)
                   (task--result-preview (task-job-result job))
                   (format nil "Task ~A is ~A.~%~A" identifier
                           (task-job-state job)
                           (or (getf (getf snapshot :progress) :recent-output)
                               "(no output yet)")))
               snapshot))))))
      (t
       (error 'task-error :message
              (format nil "Unknown job operation ~A." operation) :tool-name
              (tool-canonical-name tool))))))

(defun task-run-parameters-schema ()
  "Return the permissive flat-or-batch schema advertised by task.run."
  (let* ((item-properties
          (json-object "name"
                       (tool-string-property "Optional stable child name.")
                       "agent"
                       (tool-string-property
                        "Agent type, including scout, designer, reviewer, librarian, task, or sonic.")
                       "task"
                       (tool-string-property
                        "Self-contained child assignment.")
                       "context"
                       (tool-string-property
                        "Optional item-specific background.")
                       "isolated"
                       (tool-boolean-property
                        "Request worktree isolation when available.")
                       "async"
                       (tool-boolean-property
                        "Detach this non-blocking child as a background job.")))
         (item-schema (tool-object-schema item-properties '("task")))
         (properties
          (json-object "name"
                       (tool-string-property
                        "Optional stable child name for a flat call.")
                       "agent"
                       (tool-string-property
                        "Agent type for a flat call; defaults to task.")
                       "task"
                       (tool-string-property
                        "Self-contained assignment for a flat call.")
                       "context"
                       (tool-string-property
                        "Shared non-empty background required for batch calls.")
                       "isolated"
                       (tool-boolean-property
                        "Request worktree isolation when available.")
                       "async"
                       (tool-boolean-property
                        "Detach non-blocking children as background jobs.")
                       "tasks"
                       (json-object "type" "array" "description"
                                    "Child assignments executed with shared context."
                                    "items" item-schema "minItems" 1))))
    (tool-object-schema properties nil)))

(defun task-augment-tool-registry (registry)
  "Register one session-scoped task orchestrator and its task/job tools."
  (when (tool-registry-find registry "task" "run")
    (return-from task-augment-tool-registry registry))
  (let* ((orchestrator (task-orchestrator-create))
         (identifier-schema
          (tool-object-schema
           (json-object "id" (tool-string-property "The task job identifier."))
           '("id"))))
    (tool-registry-register registry
                            (make-instance 'task-run-tool :orchestrator
                                           orchestrator :namespace "task" :name
                                           "run" :description
                                           "Spawn a real in-process child agent or a concurrency-limited batch. Children have explicit identities, restricted tools, recursion policy, progress, artifacts, and a required yield protocol."
                                           :parameters
                                           (task-run-parameters-schema)))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "list" :description
                                           "List synchronous and detached task jobs in this session."
                                           :parameters
                                           (tool-object-schema (json-object)
                                                               nil)))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "get" :description
                                           "Inspect one task job's lifecycle, progress, and result."
                                           :parameters identifier-schema))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "wait" :description
                                           "Wait briefly for one task job and return its current or terminal result."
                                           :parameters
                                           (tool-object-schema
                                            (json-object "id"
                                                         (tool-string-property
                                                          "The task job identifier.")
                                                         "timeout-seconds"
                                                         (tool-integer-property
                                                          "Maximum wait in seconds; defaults to 60."))
                                            '("id"))))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "cancel" :description
                                           "Request interruption of one queued or running task job."
                                           :parameters identifier-schema))
    registry))
