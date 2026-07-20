(in-package #:autolith)

;;;; -- In-Process Task Orchestration --

(define-constant +task-default-maximum-concurrency+ 8 :documentation
                 "The default number of child agents that may run concurrently.")

(define-constant +task-maximum-concurrency+ 32 :documentation
                 "The largest supported child-agent worker pool.")

(define-constant +task-maximum-batch-size+ 16 :documentation
                 "The largest task batch accepted atomically.")

(define-constant +task-maximum-live-jobs+ 64 :documentation
                 "The maximum combined queued and running task jobs.")

(define-constant +task-terminal-retention-limit+ 64 :documentation
                 "The maximum terminal task summaries retained in one session.")

(define-constant +task-shutdown-timeout-seconds+ 10 :documentation
                 "The maximum time allowed for task worker shutdown.")

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

(define-constant +task-identifier-maximum-characters+ 64 :documentation
                 "The maximum friendly task identifier fragment retained by the scheduler.")

(define-constant +task-retained-assignment-limit+ 1000 :documentation
                 "The assignment characters retained after a task becomes terminal.")

(define-constant +task-retained-output-limit+ 2000 :documentation
                 "The result output characters retained after artifact publication.")

(define-constant +task-retained-progress-output-limit+ 1000 :documentation
                 "The streamed output characters retained for a terminal job.")

(define-constant +task-retained-structured-output-limit+ 2000 :documentation
                 "The readable structured-result characters retained outside its artifact.")

(define-constant +task-retained-usage-limit+ 1000 :documentation
                 "The provider-usage characters retained after a task becomes terminal.")

(define-constant +task-tool-content-limit+ 16000 :documentation
                 "The maximum provider-visible characters returned by task and job tools.")

(define-constant +task-agent-page-default+ 16 :documentation
                 "The default number of task-agent discovery records returned at once.")

(define-constant +task-agent-page-maximum+ 32 :documentation
                 "The largest task-agent discovery page accepted by the provider tool.")

(define-constant +task-job-wait-maximum-seconds+ 3600 :documentation
                 "The longest blocking wait accepted by job.wait.")

(define-constant +task-job-page-default+ 32 :documentation
                 "The default number of job.list records returned at once.")

(define-constant +task-job-page-maximum+ 64 :documentation
                 "The largest job.list page accepted by the provider tool.")

(define-constant +task-result-label-maximum-characters+ 256 :documentation
                 "The maximum child yield label length accepted and retained.")


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
         :documentation "The raw validated provider JSON yield value.")
   (data-present-p :initform nil :accessor task-completion-data-present-p
                   :type boolean :documentation
                   "True when the child explicitly supplied yield data, including null.")
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
  ((lock
    :initform (make-lock "Autolith task orchestrator")
    :accessor task-orchestrator-lock
    :documentation "The lock protecting scheduler, jobs, and listeners.")
   (condition-variable
    :initform (make-condition-variable)
    :accessor task-orchestrator-condition-variable
    :documentation "The condition waking reusable workers and shutdown waiters.")
   (maximum-concurrency
    :initarg :maximum-concurrency
    :accessor task-orchestrator-maximum-concurrency
    :type (integer 1)
    :documentation "The maximum child jobs that may execute concurrently.")
   (maximum-depth
    :initarg :maximum-depth
    :accessor task-orchestrator-maximum-depth
    :type (integer 1)
    :documentation "The maximum child depth below the primary agent.")
   (maximum-runtime-milliseconds
    :initarg :maximum-runtime-milliseconds
    :accessor task-orchestrator-maximum-runtime-milliseconds
    :type (integer 0)
    :documentation "The wall-clock cap for one child, or zero when disabled.")
   (queue
    :initform nil
    :accessor task-orchestrator-queue
    :type list
    :documentation "The bounded FIFO of jobs awaiting a reusable worker.")
   (worker-threads
    :initform nil
    :accessor task-orchestrator-worker-threads
    :type list
    :documentation "The reusable scheduler worker threads.")
   (monitor-thread
    :initform nil
    :accessor task-orchestrator-monitor-thread
    :type t
    :documentation "The optional single runtime-deadline monitor thread.")
   (shutdown-p
    :initform nil
    :accessor task-orchestrator-shutdown-p
    :type boolean
    :documentation "True while admission is closed and workers must exit.")
   (lifecycle-state
    :initform :open
    :accessor task-orchestrator-lifecycle-state
    :type keyword
    :documentation "The :OPEN, :CLOSING, or :CLOSED scheduler lifecycle state.")
   (close-owner
    :initform nil
    :accessor task-orchestrator-close-owner
    :type t
    :documentation "The thread coordinating shutdown, or NIL between attempts.")
   (active-count
    :initform 0
    :accessor task-orchestrator-active-count
    :type (integer 0)
    :documentation "The jobs currently executing on reusable workers.")
   (live-count
    :initform 0
    :accessor task-orchestrator-live-count
    :type (integer 0)
    :documentation "The admitted queued, running, and finalizing jobs.")
   (next-index
    :initform 0
    :accessor task-orchestrator-next-index
    :type (integer 0)
    :documentation "The monotonically increasing friendly-name source.")
   (names
    :initform (make-hash-table :test #'equal)
    :accessor task-orchestrator-names
    :type hash-table
    :documentation "Lowercase child identifiers reserved by retained jobs.")
   (jobs
    :initform (make-hash-table :test #'equal)
    :accessor task-orchestrator-jobs
    :type hash-table
    :documentation "Task identifiers mapped to bounded live or terminal jobs.")
   (terminal-identifiers
    :initform nil
    :accessor task-orchestrator-terminal-identifiers
    :type list
    :documentation "Terminal job identifiers ordered from oldest to newest.")
   (listeners
    :initform nil
    :accessor task-orchestrator-listeners
    :type list
    :documentation "Callbacks receiving portable task lifecycle and progress events."))
  (:documentation
   "Session-scoped child identity, concurrency, event, and job state."))

(defclass task-job nil
  ((orchestrator :initarg :orchestrator :reader task-job-orchestrator
		 :type task-orchestrator :documentation
		 "The session orchestrator owning this job.")
   (identity
    :initarg :identity
    :reader task-job-identity
    :type list
    :documentation "The stable child identity plist.")
   (execution-identifier
    :initarg :execution-identifier
    :reader task-job-execution-identifier
    :type non-empty-string
    :documentation "The process-independent identity used for private artifacts.")
   (definition :initarg :definition :accessor task-job-definition :type
               (option task-agent-definition) :documentation
               "The full child role while this job remains live.")
   (definition-summary
    :initform nil
    :accessor task-job-definition-summary
    :type (option list)
    :documentation "Compact non-instruction role metadata retained at terminal state.")
   (item :initarg :item :accessor task-job-item :type list :documentation
         "The normalized assignment plist.")
   (parent-agent
    :initarg :parent-agent
    :accessor task-job-parent-agent
    :type (option agent)
    :documentation "The parent session while this job remains live.")
   (root-conversation-identifier
    :initarg :root-conversation-identifier
    :reader task-job-root-conversation-identifier
    :type non-empty-string
    :documentation "The primary conversation that owns this task tree.")
   (owner-identifiers
    :initarg :owner-identifiers
    :reader task-job-owner-identifiers
    :type list
    :documentation "The ancestor task identifiers authorized to inspect this job.")
   (parent-call-id :initarg :parent-call-id :initform nil :reader
			   task-job-parent-call-id :type (option string) :documentation
			   "The task.run function call that created this child.")
   (command-authorization-function
    :initarg :command-authorization-function
    :initform nil
    :accessor task-job-command-authorization-function
    :type (option function)
    :documentation "The parent capability used to authorize child shell commands.")
   (detached-p :initarg :detached-p :reader task-job-detached-p :type
               boolean :documentation
               "True when the parent did not wait for this child.")
   (lock
    :initform (make-lock "Autolith task job")
    :reader task-job-lock
    :documentation "The lock protecting mutable job lifecycle fields.")
   (condition-variable
    :initform (make-condition-variable)
    :reader task-job-condition-variable
    :documentation "The condition waking waiters after lifecycle transitions.")
   (state :initform :queued :accessor task-job-state :type keyword
          :documentation
          "The queued, running, completed, failed, or aborted state.")
   (publication-claimed-p
    :initform nil
    :accessor task-job-publication-claimed-p
    :type boolean
    :documentation "True while one writer prepares the terminal publication.")
   (thread
    :initform nil
    :accessor task-job-thread
    :type t
    :documentation "The reusable worker currently executing this job.")
   (run-token
    :initform nil
    :accessor task-job-run-token
    :type (option string)
    :documentation "The token preventing delayed interrupts from striking another job.")
   (result :initform nil :accessor task-job-result :type t
           :documentation "The final portable SingleResult-style plist.")
   (condition-report
    :initform nil
    :accessor task-job-condition-report
    :type (option string)
    :documentation "The bounded report for an unexpected job failure.")
   (cancellation-reason :initform nil :accessor
			task-job-cancellation-reason :type (option keyword) :documentation
			"The structured cancellation reason requested by a controller.")
   (retained-p
    :initform nil
    :accessor task-job-retained-p
    :type boolean
    :documentation "True after terminal retention accounts for this job.")
   (progress :initform (make-instance 'task-progress) :reader
             task-job-progress :type task-progress :documentation
             "The normalized progress visible to job inspection.")
   (created-at :initform (get-internal-real-time) :reader
               task-job-created-at :type integer :documentation
               "The internal real time at job creation.")
   (started-at :initform nil :accessor task-job-started-at :type t
               :documentation "The internal real time at child execution start.")
   (deadline
    :initform nil
    :accessor task-job-deadline
    :type (option integer)
    :documentation "The internal real time at which the runtime monitor cancels this job.")
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

(defvar *task-current-job* nil
  "The task job dynamically owned by the current reusable worker.")

(defvar *task-current-run-token* nil
  "The run token guarding cancellation interrupts on a reusable worker.")

(defvar *task-admission-parent-locked-p* nil
  "True while nested task admission holds its parent job lifecycle lock.")

(defvar *task-terminal-publication-job* nil
  "The job protected from delayed cancellation while publishing terminal state.")

(-> task--condition-broadcast (t) null)
(defun task--condition-broadcast (condition-variable)
  "Wake every waiter on CONDITION-VARIABLE through the narrow SBCL adapter."
  #+sbcl
  (sb-thread:condition-broadcast condition-variable)
  #-sbcl
  (condition-notify condition-variable)
  nil)

(defmethod agent-turn-complete-p
    ((agent task-child-agent) (result provider-result))
  "Return true after AGENT yields or stops without requesting continuation."
  (or (task-completion-called-p (task-child-agent-completion agent))
      (call-next-method)))

(defmethod agent-turn-completion-details ((agent task-child-agent))
  "Identify whether AGENT completed through its explicit yield protocol."
  (list :yielded-p
        (task-completion-called-p (task-child-agent-completion agent))))

(defclass task-tool-result (tool-result)
  ((details :initarg :details :reader task-tool-result-details
            :reader tool-result-details :type t
            :documentation
            "Portable machine-readable task or job orchestration details."))
  (:documentation
   "A normal tool result carrying structured orchestration metadata."))

(defclass task-orchestrator-tool (tool)
  ((orchestrator
    :initarg :orchestrator
    :reader task-orchestrator-tool-orchestrator
    :type task-orchestrator
    :documentation "The session-scoped scheduler shared by task and job tools."))
  (:documentation "A provider tool backed by one shared task orchestrator."))

(defclass task-run-tool (task-orchestrator-tool) nil
  (:documentation
   "Spawn one child agent or a concurrency-limited batch."))

(defclass task-agents-tool (task-orchestrator-tool) nil
  (:documentation "Discover effective child roles and rejected role files."))

(defclass task-job-tool (task-orchestrator-tool) nil
  (:documentation "Inspect, wait for, or cancel detached task jobs."))

(-> task-run-tool-orchestrator (task-run-tool) task-orchestrator)
(defun task-run-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-agents-tool-orchestrator (task-agents-tool) task-orchestrator)
(defun task-agents-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-job-tool-orchestrator (task-job-tool) task-orchestrator)
(defun task-job-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(defclass task-yield-tool (tool) nil
  (:documentation
   "Submit the required terminal result from a child agent."))

(defmethod tool-decode-arguments ((tool task-run-tool) source)
  "Decode task.run booleans without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.run"))

(defmethod tool-decode-arguments ((tool task-yield-tool) source)
  "Decode yield values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "yield.submit"))

(defmethod tool-decode-arguments ((tool task-agents-tool) source)
  "Decode task.agents values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.agents"))

(defmethod tool-decode-arguments ((tool task-job-tool) source)
  "Decode job tool values without conflating JSON false and null."
  (task-json-decode source :tool-name (tool-canonical-name tool)))

(-> task--environment-integer
    (string integer &key (:minimum (option integer)) (:maximum (option integer)))
    integer)
(defun task--environment-integer (name fallback &key minimum maximum)
  "Return bounded integer environment NAME or FALLBACK."
  (let ((value (uiop/os:getenv name)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (if (and (integerp parsed)
                       (or (null minimum) (>= parsed minimum)))
                  (if maximum (min parsed maximum) parsed)
                  fallback))
          (error nil fallback))
        fallback)))

(-> task-orchestrator-create () task-orchestrator)
(defun task-orchestrator-create ()
  "Create an orchestrator from the current task environment settings."
  (make-instance 'task-orchestrator :maximum-concurrency
                 (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                            +task-default-maximum-concurrency+
                                            :minimum 1
                                            :maximum
                                            +task-maximum-concurrency+)
                 :maximum-depth
                 (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                            +task-default-maximum-depth+
                                            :minimum 1)
                 :maximum-runtime-milliseconds
                 (task--environment-integer "AUTOLITH_TASK_MAX_RUNTIME_MS" 0
                                            :minimum 0)))

(-> task-orchestrator--reap-dead-threads-locked
    (task-orchestrator)
    null)
(defun task-orchestrator--reap-dead-threads-locked (orchestrator)
  "Forget dead runtime threads and complete an ownerless shutdown."
  (let ((owner (task-orchestrator-close-owner orchestrator)))
    (when (and owner (not (thread-alive-p owner)))
      (setf (task-orchestrator-close-owner orchestrator) nil)))
  (setf (task-orchestrator-worker-threads orchestrator)
        (remove-if-not #'thread-alive-p
                       (task-orchestrator-worker-threads orchestrator)))
  (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
    (when (and monitor (not (thread-alive-p monitor)))
      (setf (task-orchestrator-monitor-thread orchestrator) nil)))
  (when (and (eq (task-orchestrator-lifecycle-state orchestrator) :closing)
             (null (task-orchestrator-close-owner orchestrator))
             (null (task-orchestrator-worker-threads orchestrator))
             (null (task-orchestrator-monitor-thread orchestrator)))
    (setf (task-orchestrator-lifecycle-state orchestrator) :closed
          (task-orchestrator-active-count orchestrator) 0)
    (task--condition-broadcast
     (task-orchestrator-condition-variable orchestrator)))
  nil)

(-> task-orchestrator-refresh (task-orchestrator) task-orchestrator)
(defun task-orchestrator-refresh (orchestrator)
  "Apply current limits to ORCHESTRATOR and ensure its reusable workers."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (task-orchestrator--reap-dead-threads-locked orchestrator)
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :closing)
      (error 'task-error
             :message "The task runtime is still shutting down."
             :tool-name "task.run"))
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :closed)
      (setf (task-orchestrator-lifecycle-state orchestrator) :open))
    (setf (task-orchestrator-maximum-concurrency orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                     +task-default-maximum-concurrency+
                                     :minimum 1
                                     :maximum +task-maximum-concurrency+)
          (task-orchestrator-maximum-depth orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                     +task-default-maximum-depth+ :minimum 1)
          (task-orchestrator-maximum-runtime-milliseconds orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_RUNTIME_MS" 0
                                     :minimum 0)
          (task-orchestrator-shutdown-p orchestrator) nil)
    (task--condition-broadcast
     (task-orchestrator-condition-variable orchestrator)))
  (task-orchestrator--ensure-workers orchestrator)
  (task-orchestrator--ensure-monitor orchestrator)
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
      (handler-case
          (funcall listener channel payload)
        (serious-condition ()
          nil))))
  nil)

(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((unbounded (string-downcase (task--trim (or value ""))))
         (text (subseq unbounded
                       0
                       (min (length unbounded)
                            +task-identifier-maximum-characters+)))
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

(-> task-orchestrator--create-identity
    (task-orchestrator (option string) string)
    list)
(defun task-orchestrator--create-identity
    (orchestrator requested-name agent-type)
  "Reserve a child identity while ORCHESTRATOR's lock is held."
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
         (suffix-text (format nil "-~D" index))
         (base-limit
           (max 0
                (- +task-identifier-maximum-characters+
                   (length suffix-text))))
         (candidate
           (concatenate 'string
                        (subseq base 0 (min (length base) base-limit))
                        suffix-text)))
    (setf (gethash candidate (task-orchestrator-names orchestrator)) t)
    (list :id candidate
          :display-name
          (if requested-name
              (subseq requested-name
                      0
                      (min (length requested-name)
                           +task-identifier-maximum-characters+))
              candidate)
          :agent-type agent-type
          :index index)))

(-> task-orchestrator-create-identity
    (task-orchestrator (option string) string)
    list)
(defun task-orchestrator-create-identity
    (orchestrator requested-name agent-type)
  "Reserve and return a stable unique child identity plist."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (task-orchestrator--create-identity orchestrator requested-name agent-type)))

(-> task-orchestrator--worker-loop (task-orchestrator) null)
(defun task-orchestrator--worker-loop (orchestrator)
  "Run queued jobs on one reusable worker until ORCHESTRATOR closes."
  (loop
    (let ((job nil))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (loop
          (when (task-orchestrator-shutdown-p orchestrator)
            (return-from task-orchestrator--worker-loop nil))
          (when (and (task-orchestrator-queue orchestrator)
                     (< (task-orchestrator-active-count orchestrator)
                        (task-orchestrator-maximum-concurrency orchestrator)))
            (setf job (pop (task-orchestrator-queue orchestrator)))
            (incf (task-orchestrator-active-count orchestrator))
            (return))
          (condition-wait
           (task-orchestrator-condition-variable orchestrator)
           (task-orchestrator-lock orchestrator))))
      (unwind-protect
           (handler-case
               (task-job--execute job)
             (serious-condition (condition)
               (handler-case
                   (unless (task-job-terminal-p job)
                     (unless
                         (task-job--publish-terminal
                          job
                          :failed
                          (task--failed-result job :failed
                                               (princ-to-string condition))
                          :report
                          (bounded-string (princ-to-string condition)))
                       (unless (task-job-terminal-p job)
                         (task-job--force-terminal-failure
                          job condition condition))))
                 (serious-condition (publication-condition)
                   (task-job--force-terminal-failure
                    job condition publication-condition)))))
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (decf (task-orchestrator-active-count orchestrator))
          (task--condition-broadcast
           (task-orchestrator-condition-variable orchestrator)))))))

(-> task-orchestrator--ensure-workers (task-orchestrator) null)
(defun task-orchestrator--ensure-workers (orchestrator)
  "Ensure ORCHESTRATOR has enough reusable scheduler workers."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-worker-threads orchestrator)
          (remove-if-not #'thread-alive-p
                         (task-orchestrator-worker-threads orchestrator)))
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :open)
      (loop repeat (max 0
                        (- (task-orchestrator-maximum-concurrency orchestrator)
                           (length
                            (task-orchestrator-worker-threads orchestrator))))
            for index from
              (length (task-orchestrator-worker-threads orchestrator))
            for thread =
              (make-thread
               (lambda () (task-orchestrator--worker-loop orchestrator))
               :name (format nil "Autolith task worker ~D" (1+ index)))
            do (push thread (task-orchestrator-worker-threads orchestrator)))
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator))))
  nil)

(-> task-orchestrator--monitor-loop (task-orchestrator) null)
(defun task-orchestrator--monitor-loop (orchestrator)
  "Cancel running jobs whose runtime deadlines have elapsed."
  (loop
    (let ((expired nil)
          (jobs nil))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (when (task-orchestrator-shutdown-p orchestrator)
          (return-from task-orchestrator--monitor-loop nil))
        (setf jobs
              (loop for job being the hash-values of
                      (task-orchestrator-jobs orchestrator)
                    collect job)))
      (let ((now (get-internal-real-time)))
        (dolist (job jobs)
          (with-lock-held ((task-job-lock job))
            (when (and (eq (task-job-state job) :running)
                       (task-job-deadline job)
                       (>= now (task-job-deadline job)))
              (push job expired)))))
      (dolist (job expired)
        (task-job-cancel job :timeout))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (unless (task-orchestrator-shutdown-p orchestrator)
          (condition-wait
           (task-orchestrator-condition-variable orchestrator)
           (task-orchestrator-lock orchestrator)
           :timeout 0.1))))))

(-> task-orchestrator--ensure-monitor (task-orchestrator) null)
(defun task-orchestrator--ensure-monitor (orchestrator)
  "Ensure ORCHESTRATOR has one deadline monitor when runtime caps are enabled."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
      (when (and (plusp
                  (task-orchestrator-maximum-runtime-milliseconds orchestrator))
                 (eq (task-orchestrator-lifecycle-state orchestrator) :open)
                 (not (and monitor (thread-alive-p monitor))))
        (setf (task-orchestrator-monitor-thread orchestrator)
              (make-thread
               (lambda () (task-orchestrator--monitor-loop orchestrator))
               :name "Autolith task deadline monitor")))))
  nil)

(-> task-orchestrator-close (task-orchestrator) boolean)
(defun task-orchestrator-close (orchestrator)
  "Cancel all jobs, stop reusable threads, and report complete shutdown."
  (let ((owner-p nil)
        (jobs nil)
        (threads nil)
        (deadline (+ (get-internal-real-time)
                     (* +task-shutdown-timeout-seconds+
                        internal-time-units-per-second))))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (task-orchestrator--reap-dead-threads-locked orchestrator)
      (case (task-orchestrator-lifecycle-state orchestrator)
        (:closed
         (return-from task-orchestrator-close t))
        (:closing
         (let ((owner (task-orchestrator-close-owner orchestrator)))
           (unless (and owner
                        (not (eq owner (current-thread)))
                        (thread-alive-p owner))
             (setf owner-p t
                   (task-orchestrator-close-owner orchestrator)
                   (current-thread)
                   jobs
                   (loop for job being the hash-values of
                           (task-orchestrator-jobs orchestrator)
                         collect job)
                   threads
                   (remove nil
                           (cons
                            (task-orchestrator-monitor-thread orchestrator)
                            (copy-list
                             (task-orchestrator-worker-threads
                              orchestrator))))))))
        (otherwise
         (setf owner-p t
               (task-orchestrator-close-owner orchestrator) (current-thread)
               (task-orchestrator-lifecycle-state orchestrator) :closing
               (task-orchestrator-shutdown-p orchestrator) t
               (task-orchestrator-queue orchestrator) nil
               jobs
               (loop for job being the hash-values of
                       (task-orchestrator-jobs orchestrator)
                     collect job)
               threads
               (remove nil
                       (cons (task-orchestrator-monitor-thread orchestrator)
                             (copy-list
                              (task-orchestrator-worker-threads
                               orchestrator)))))
         (task--condition-broadcast
          (task-orchestrator-condition-variable orchestrator)))))
    (unless owner-p
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (loop while (and (eq (task-orchestrator-lifecycle-state orchestrator)
                             :closing)
                         (task-orchestrator-close-owner orchestrator)
                         (< (get-internal-real-time) deadline))
              for remaining =
                (/ (max 0 (- deadline (get-internal-real-time)))
                   internal-time-units-per-second)
              do (condition-wait
                  (task-orchestrator-condition-variable orchestrator)
                  (task-orchestrator-lock orchestrator)
                  :timeout remaining))
        (return-from task-orchestrator-close
          (eq (task-orchestrator-lifecycle-state orchestrator) :closed))))
    (dolist (job jobs)
      (task-job-cancel job :shutdown))
    (loop
      for live = (remove-if-not
                  (lambda (thread)
                    (and (not (eq thread (current-thread)))
                         (thread-alive-p thread)))
                  threads)
      until (or (null live)
                (>= (get-internal-real-time) deadline))
      do (sleep 0.01))
    (dolist (thread threads)
      (when (and (not (eq thread (current-thread)))
                 (not (thread-alive-p thread)))
        (join-thread thread)))
    (let ((live
            (remove-if-not
             (lambda (thread)
               (and (not (eq thread (current-thread)))
                    (thread-alive-p thread)))
             threads)))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (setf (task-orchestrator-worker-threads orchestrator)
              (intersection live
                            (task-orchestrator-worker-threads orchestrator)
                            :test #'eq)
              (task-orchestrator-monitor-thread orchestrator)
              (and (member (task-orchestrator-monitor-thread orchestrator)
                           live
                           :test #'eq)
                   (task-orchestrator-monitor-thread orchestrator))
              (task-orchestrator-lifecycle-state orchestrator)
              (if live :closing :closed)
              (task-orchestrator-close-owner orchestrator) nil
              (task-orchestrator-active-count orchestrator)
              (if live (task-orchestrator-active-count orchestrator) 0))
        (task--condition-broadcast
         (task-orchestrator-condition-variable orchestrator)))
      (null live))))

(-> task-orchestrator-detach (task-orchestrator) null)
(defun task-orchestrator-detach (orchestrator)
  "Remove closed runtime state before an image save or registry replacement."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (unless (eq (task-orchestrator-lifecycle-state orchestrator) :closed)
      (error 'task-error
             :message "Task runtime must close before it can detach."
             :tool-name "task.run"))
    (when (or (some #'thread-alive-p
                    (task-orchestrator-worker-threads orchestrator))
              (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
                (and monitor (thread-alive-p monitor))))
      (error 'task-error
             :message "Task runtime cannot detach while its threads are alive."
             :tool-name "task.run"))
    (setf (task-orchestrator-worker-threads orchestrator) nil
          (task-orchestrator-monitor-thread orchestrator) nil
          (task-orchestrator-queue orchestrator) nil
          (task-orchestrator-close-owner orchestrator) nil
          (task-orchestrator-active-count orchestrator) 0
          (task-orchestrator-live-count orchestrator) 0
          (task-orchestrator-terminal-identifiers orchestrator) nil
          (task-orchestrator-listeners orchestrator) nil)
    (clrhash (task-orchestrator-jobs orchestrator))
    (clrhash (task-orchestrator-names orchestrator)))
  nil)

(defmethod tool-runtime-identity ((tool task-orchestrator-tool))
  "Return the scheduler shared by task and job tools."
  (task-orchestrator-tool-orchestrator tool))

(defmethod tool-runtime-close ((tool task-orchestrator-tool))
  "Stop TOOL's shared jobs and reusable scheduler threads."
  (unless (task-orchestrator-close
           (task-orchestrator-tool-orchestrator tool))
    (error 'task-error
           :message "Task workers did not stop before the shutdown deadline."
           :tool-name (tool-canonical-name tool)))
  nil)

(defmethod tool-runtime-detach ((tool task-orchestrator-tool))
  "Remove TOOL's closed shared scheduler graph before image saving."
  (task-orchestrator-detach (task-orchestrator-tool-orchestrator tool)))

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
  (let ((progress (task-job-progress job))
        (event nil))
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
      (setf (task-progress-updated-at progress) (get-internal-real-time)
            event
            (list :id (getf (task-job-identity job) :id)
                  :status (task-progress-status progress)
                  :current-tool (task-progress-current-tool progress)
                  :request-count (task-progress-request-count progress))))
    (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-progress
                            event)
    (let ((reason
            (with-lock-held ((task-job-lock job))
              (and (not (task-job--terminal-state-p (task-job-state job)))
                   (task-job-cancellation-reason job)))))
      (when reason
        (error 'task-aborted
               :message
               (format nil "Task ~A was ~A."
                       (getf (task-job-identity job) :id)
                       reason)
               :reason reason))))
  nil)

(-> task-job--terminal-state-p (keyword) boolean)
(defun task-job--terminal-state-p (state)
  "Return true when STATE is a published terminal task state."
  (not (null (member state '(:completed :failed :aborted) :test #'eq))))

(-> task-job-agent-name (task-job) non-empty-string)
(defun task-job-agent-name (job)
  "Return JOB's live or retained child role name."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-name definition)
        (getf (task-job-definition-summary job) :name))))

(-> task-job-agent-source (task-job) keyword)
(defun task-job-agent-source (job)
  "Return JOB's live or retained child role source."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-source definition)
        (getf (task-job-definition-summary job) :source))))

(-> task-progress--snapshot
    (task-job &key (:parent t) (:result t) (:ended-at t))
    list)
(defun task-progress--snapshot (job &key parent result ended-at)
  "Return JOB progress using lifecycle values captured under the job lock."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (list :id (getf (task-job-identity job) :id)
            :agent (task-job-agent-name job)
            :status (task-progress-status progress)
            :current-tool (task-progress-current-tool progress)
            :recent-tools
            (reverse (copy-list (task-progress-recent-tools progress)))
            :recent-output (task-progress-output-tail progress)
            :request-count (task-progress-request-count progress)
            :usage (copy-tree (task-progress-usage progress))
            :duration-ms
            (and (task-progress-started-at progress)
                 (task--milliseconds-between
                  (task-progress-started-at progress)
                  (or ended-at (get-internal-real-time))))
            :model
            (or (getf result :model)
                (and parent
                     (configuration-model
                      (task-configuration-for-definition
                       (agent-configuration parent)
                       (task-job-definition job)))))))))

(-> task-progress-snapshot (task-job) list)
(defun task-progress-snapshot (job)
  "Return a coherent portable snapshot of JOB's current progress."
  (with-lock-held ((task-job-lock job))
    (task-progress--snapshot job
                             :parent (task-job-parent-agent job)
                             :result (task-job-result job)
                             :ended-at (task-job-ended-at job))))

(-> task-job-terminal-p (task-job) boolean)
(defun task-job-terminal-p (job)
  "Return true when JOB cannot make another state transition."
  (with-lock-held ((task-job-lock job))
    (task-job--terminal-state-p (task-job-state job))))

(-> task-job--snapshot-locked (task-job) list)
(defun task-job--snapshot-locked (job)
  "Return JOB's snapshot while its lifecycle lock is held."
  (let ((result (copy-tree (task-job-result job))))
    (list :job-id (getf (task-job-identity job) :id)
          :execution-id (task-job-execution-identifier job)
          :type :task
          :state (task-job-state job)
          :detached (task-job-detached-p job)
          :agent (task-job-agent-name job)
          :assignment
          (bounded-string (getf (task-job-item job) :task)
                          :limit +task-retained-assignment-limit+)
          :progress
          (task-progress--snapshot job
                                   :parent (task-job-parent-agent job)
                                   :result result
                                   :ended-at (task-job-ended-at job))
          :result result
          :cancellation-reason (task-job-cancellation-reason job)
          :condition-report (task-job-condition-report job))))

(-> task-job-snapshot (task-job) list)
(defun task-job-snapshot (job)
  "Return JOB's coherent portable lifecycle, progress, and result snapshot."
  (with-lock-held ((task-job-lock job))
    (task-job--snapshot-locked job)))

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

(-> task-job-visible-to-agent-p (task-job agent) boolean)
(defun task-job-visible-to-agent-p (job viewer)
  "Return true when VIEWER owns JOB through conversation or task ancestry."
  (not
   (null
    (if (typep viewer 'task-child-agent)
        (member (getf (task-job-identity
                       (task-child-agent-job viewer))
                      :id)
                (task-job-owner-identifiers job)
                :test #'string=)
        (string=
         (task-job-root-conversation-identifier job)
         (conversation-identifier (agent-conversation viewer)))))))

(-> task-orchestrator-list-visible-jobs
    (task-orchestrator agent)
    list)
(defun task-orchestrator-list-visible-jobs (orchestrator viewer)
  "Return jobs VIEWER may inspect, ordered by child index."
  (remove-if-not
   (lambda (job) (task-job-visible-to-agent-p job viewer))
   (task-orchestrator-list-jobs orchestrator)))

(-> task-orchestrator-find-visible-job
    (task-orchestrator string agent string)
    task-job)
(defun task-orchestrator-find-visible-job
    (orchestrator identifier viewer tool-name)
  "Return VIEWER's visible IDENTIFIER or signal a non-disclosing task error."
  (let ((job
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (gethash identifier (task-orchestrator-jobs orchestrator)))))
    (if (and job (task-job-visible-to-agent-p job viewer))
        job
        (error 'task-error
               :message (format nil "No visible task job named ~A exists."
                                identifier)
               :tool-name tool-name
               :task-id identifier))))

(-> task-job--request-cancellation (task-job keyword) boolean)
(defun task-job--request-cancellation (job reason)
  "Request first-writer cancellation REASON for JOB without walking descendants."
  (let ((thread nil)
        (run-token nil)
        (queued-p nil)
        (cancel-p nil)
        (orchestrator (task-job-orchestrator job)))
    (with-lock-held ((task-job-lock job))
      (unless (or (task-job--terminal-state-p (task-job-state job))
                  (task-job-publication-claimed-p job)
                  (task-job-cancellation-reason job))
        (setf (task-job-cancellation-reason job) reason
              thread (task-job-thread job)
              run-token (task-job-run-token job)
              queued-p (eq (task-job-state job) :queued)
              cancel-p t)))
    (when cancel-p
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (setf (task-orchestrator-queue orchestrator)
              (remove job (task-orchestrator-queue orchestrator) :test #'eq))
        (task--condition-broadcast
         (task-orchestrator-condition-variable orchestrator)))
      (when queued-p
        (task-job--publish-terminal
         job
         :aborted
         (task--failed-result
          job
          :aborted
          (format nil "Task ~A was ~A before it started."
                  (getf (task-job-identity job) :id)
                  reason)))))
    (when (and cancel-p thread run-token (thread-alive-p thread))
      (interrupt-thread thread
                        (lambda ()
                          (when (and (eq *task-current-job* job)
                                     (not (eq *task-terminal-publication-job*
                                              job))
                                     (stringp *task-current-run-token*)
                                     (string=
                                      *task-current-run-token*
                                      run-token))
                            (error 'task-aborted
                                   :message
                                   (format nil "Task ~A was ~A."
                                           (getf (task-job-identity job) :id)
                                           reason)
                                   :reason reason)))))
    cancel-p))

(-> task-job-cancel (task-job keyword) (values boolean list))
(defun task-job-cancel (job reason)
  "Cancel JOB and every retained live descendant, returning accepted identities."
  (let* ((orchestrator (task-job-orchestrator job))
         (identifier (getf (task-job-identity job) :id))
         (accepted-p (task-job--request-cancellation job reason))
         (accepted-descendants nil))
    (loop
      with accepted-this-pass = nil
      do
         (setf accepted-this-pass nil)
         (let ((descendants
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (sort
                    (loop for candidate being the hash-values of
                            (task-orchestrator-jobs orchestrator)
                          when (member identifier
                                       (task-job-owner-identifiers candidate)
                                       :test #'string=)
                            collect candidate)
                    #'<
                    :key (lambda (candidate)
                           (getf (task-job-identity candidate) :index))))))
           (dolist (descendant descendants)
             (when (task-job--request-cancellation descendant reason)
               (let ((descendant-identifier
                       (getf (task-job-identity descendant) :id)))
                 (pushnew descendant-identifier accepted-descendants
                          :test #'string=)
                 (setf accepted-this-pass t)))))
      while accepted-this-pass)
    (values accepted-p
            (sort accepted-descendants #'string<))))

(-> task-job-help-join (task-job) boolean)
(defun task-job-help-join (job)
  "Run queued JOB inline when a child waiter would otherwise occupy a worker."
  (let ((claimed-p nil)
        (orchestrator (task-job-orchestrator job)))
    (with-lock-held ((task-job-lock job))
      (when (and (eq (task-job-state job) :queued)
                 (null (task-job-cancellation-reason job))
                 (not (task-job-publication-claimed-p job)))
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (when (member job (task-orchestrator-queue orchestrator) :test #'eq)
            (setf (task-orchestrator-queue orchestrator)
                  (remove job
                          (task-orchestrator-queue orchestrator)
                          :test #'eq)
                  claimed-p t)
            (task--condition-broadcast
             (task-orchestrator-condition-variable orchestrator))))))
    (when claimed-p
      (task-job--execute job))
    claimed-p))

(-> task-job-await
    (task-job (option (real 0)))
    (values list boolean))
(defun task-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return a snapshot plus terminal flag."
  (let ((deadline
         (and timeout-seconds
              (+ (get-internal-real-time)
                 (* timeout-seconds internal-time-units-per-second)))))
    (with-lock-held ((task-job-lock job))
      (loop until (task-job--terminal-state-p (task-job-state job))
            for now = (get-internal-real-time)
            for remaining =
              (and deadline
                   (/ (max 0 (- deadline now))
                      internal-time-units-per-second))
            when (and deadline (<= remaining 0))
              return nil
            do (condition-wait
                (task-job-condition-variable job)
                (task-job-lock job)
                :timeout remaining))))
  (let ((snapshot (task-job-snapshot job)))
    (values snapshot
            (task-job--terminal-state-p (getf snapshot :state)))))

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
    (or (string= normalized canonical)
        (string= normalized (format nil "~A.*" namespace)))))

(defun task--definition-allows-tool-p (definition tool)
  "Return true when DEFINITION permits ordinary TOOL."
  (let ((specs (task-agent-definition-tools definition)))
    (and (tool-child-safe-p tool)
         (or (eq specs :all)
             (and (listp specs)
                  (some (lambda (spec)
                          (task--tool-spec-matches-p spec tool))
                        specs))))))

(-> task-agent-definition-validate-tools-available
    (task-agent-definition tool-registry)
    null)
(defun task-agent-definition-validate-tools-available (definition registry)
  "Reject explicit grants in DEFINITION that REGISTRY cannot provide."
  (let ((specifications (task-agent-definition-tools definition)))
    (when (listp specifications)
      (dolist (specification specifications)
        (unless
            (or (string= specification "web_search")
                (some (lambda (tool)
                        (and (tool-child-safe-p tool)
                             (task--tool-spec-matches-p specification tool)))
                      (tool-registry-tools registry)))
          (task-agent-definition--error
           :pathname (task-agent-definition-pathname definition)
           :source (task-agent-definition-source definition)
           :field :tools
           :cause (format nil "Tool grant ~S is unavailable in this session."
                          specification)
           :definition-name (task-agent-definition-name definition)))))
  nil))

(defun task-child-tool-registry (parent-registry definition orchestrator depth)
  "Build a restricted child registry with yield and structurally bounded spawning."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools parent-registry))
      (when (task--definition-allows-tool-p definition tool)
        (tool-registry-register registry tool)))
    (when
        (and (task-agent-definition-spawns definition)
             (< depth (task-orchestrator-maximum-depth orchestrator)))
      (dolist (name '("run" "agents"))
        (let ((task-tool (tool-registry-find parent-registry "task" name)))
          (when task-tool
            (tool-registry-register registry task-tool))))
      (dolist (tool (tool-registry-tools parent-registry))
        (when (string= (tool-namespace tool) "job")
          (tool-registry-register registry tool))))
    (let ((output (task-agent-definition-output definition)))
      (tool-registry-register
       registry
       (make-instance
        'task-yield-tool
        :namespace "yield"
        :name "submit"
        :description
        "Submit the required terminal child result. Call exactly once when the assignment is complete or cannot continue."
        :parameters
        (tool-object-schema
         (json-object
          "status"
          (json-object "type" "string"
                       "description" "Terminal result status."
                       "enum" (json-array "success" "failed" "aborted"))
          "text"
          (tool-string-property
           "Human-readable result for the parent; include concrete findings or changes.")
          "data"
          (if output
              (task-output-schema->json output)
              (json-object
               "description"
               "Optional structured result when the agent has no output contract."))
          "error"
          (tool-string-property
           "Failure or abort explanation when status is not success.")
          "label"
          (tool-string-property "Optional short result label."))
         '("status")))))
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
  (let ((value (and level (string-downcase (symbol-name level)))))
    (cond
      ((or (null value) (string= value "auto"))
       parent-effort)
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
           (task-agent-definition-reasoning-effort definition)
           (configuration-reasoning-effort parent-configuration)))
         (web-enabled-p
          (or (eq (task-agent-definition-tools definition) :all)
              (member "web_search" (task-agent-definition-tools definition)
                      :test #'string-equal))))
    (configuration--clone
     parent-configuration
     :model model
     :reasoning-effort effort
     :web-search-mode
     (if web-enabled-p
         (configuration-web-search-mode parent-configuration)
         "disabled"))))

(defun task-output-definition-text (definition)
  "Return DEFINITION's output contract as prompt text, or NIL."
  (let ((output (task-agent-definition-output definition)))
    (and output
         (task--write-readable-sexp output :pretty-p t))))

(defun task-child-goal-context (job child-configuration)
  "Build the transient developer instructions for JOB's child session."
  (let* ((definition (task-job-definition job))
         (identity (task-job-identity job))
         (item (task-job-item job))
         (context (getf item :context))
         (output (task-output-definition-text definition)))
    (format nil
            "You are child agent ~A of type ~A, depth ~D. Your specialized role follows.~2%~A~@[~2%Shared parent context:~%~A~]~@[~2%Your yield data must satisfy this native output contract:~%~A~]~2%You are not the primary Autolith session. self.* tools are deliberately unavailable. Work only in ~A. Complete the assignment in the user message. You MUST end by calling yield.submit exactly once. A normal assistant stop without yield is a failed child run. Put the useful parent-facing answer in yield.text and structured data in yield.data when requested."
            (getf identity :id) (task-agent-definition-name definition)
            (1+ (task-parent-depth (task-job-parent-agent job)))
            (task-agent-definition-instructions definition) context output
            (namestring
             (configuration-working-directory child-configuration)))))


(defmethod tool-execute
    ((tool task-yield-tool) (context tool-context) arguments)
  "Validate and record one exact terminal child yield."
  (declare (ignore tool))
  (let ((agent (tool-context-agent context)))
    (unless (typep agent 'task-child-agent)
      (error 'task-yield-error
             :message "yield.submit is available only inside a child agent."
             :tool-name "yield.submit"))
    (let* ((completion (task-child-agent-completion agent))
           (identifier (getf (task-child-agent-identity agent) :id))
           (output
             (task-agent-definition-output
              (task-child-agent-definition agent))))
      (labels ((yield-error (message)
                 (error 'task-yield-error
                        :message message
                        :tool-name "yield.submit"
                        :task-id identifier))

               (optional-string (name)
                 (multiple-value-bind (value present-p)
                     (gethash name arguments)
                   (when (and present-p (not (stringp value)))
                     (yield-error
                      (format nil "Yield field ~S must be a string when supplied."
                              name)))
                   (values value present-p))))
        (when (task-completion-called-p completion)
          (yield-error "This child already submitted its terminal yield."))
        (loop for field being the hash-keys of arguments
              unless (member field
                             '("status" "text" "data" "error" "label")
                             :test #'string=)
                do (yield-error
                    (format nil "Unknown yield field ~S." field)))
        (multiple-value-bind (status-text status-present-p)
            (gethash "status" arguments)
          (unless (and status-present-p (stringp status-text))
            (yield-error "Yield status must be a string."))
          (let ((status
                  (cond
                    ((string= status-text "success") :success)
                    ((string= status-text "failed") :failed)
                    ((string= status-text "aborted") :aborted)
                    (t
                     (yield-error
                      "Yield status must be exactly success, failed, or aborted.")))))
            (multiple-value-bind (text text-present-p)
                (optional-string "text")
              (declare (ignore text-present-p))
              (multiple-value-bind (failure failure-present-p)
                  (optional-string "error")
                (multiple-value-bind (label label-present-p)
                    (optional-string "label")
                  (declare (ignore label-present-p))
                  (when (and label
                             (> (length label)
                                +task-result-label-maximum-characters+))
                    (yield-error
                     (format nil
                             "Yield label may contain at most ~D characters."
                             +task-result-label-maximum-characters+)))
                  (multiple-value-bind (data data-present-p)
                      (gethash "data" arguments)
                    (case status
                      (:success
                       (when failure-present-p
                         (yield-error
                          "A successful yield must not contain an error field."))
                       (unless (or data-present-p
                                   (non-empty-string-p (task--trim (or text ""))))
                         (yield-error
                          "A successful yield requires non-empty text or explicit data."))
                       (when (and output (not data-present-p))
                         (yield-error
                          "This role requires an explicit structured yield value."))
                       (when (and output
                                  (not (task-output-schema-valid-p data output)))
                         (yield-error
                          "The supplied yield data does not satisfy the role output contract.")))
                      ((:failed :aborted)
                       (unless (and failure-present-p
                                    (non-empty-string-p
                                     (task--trim (or failure ""))))
                         (yield-error
                          "A failed or aborted yield requires a non-empty error string."))
                       (when data-present-p
                         (yield-error
                          "A failed or aborted yield must not contain structured data."))))
                    (when data-present-p
                      (task-json->sexp data))
                    (setf (task-completion-called-p completion) t
                          (task-completion-status completion) status
                          (task-completion-text completion) text
                          (task-completion-data completion) data
                          (task-completion-data-present-p completion)
                          data-present-p
                          (task-completion-error completion) failure
                          (task-completion-label completion) label)
                    (tool-success
                     (task--write-readable-sexp
                      '(:yield-submit :accepted-p t)))))))))))))

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

(-> task--artifact-root (configuration task-job) pathname)
(defun task--artifact-root (configuration job)
  "Return JOB's private transcript and artifact directory."
  (merge-pathnames
   (format nil "tasks/~A/~A/"
           (or (task--identifier-fragment
                (task-job-root-conversation-identifier job))
               "conversation")
           (task-job-execution-identifier job))
   (configuration-data-root configuration)))

(-> task--write-result-artifact (task-job list) pathname)
(defun task--write-result-artifact (job result)
  "Publish portable RESULT once at JOB's unique artifact pathname."
  (let* ((configuration (agent-configuration (task-job-parent-agent job)))
         (root (task--artifact-root configuration job))
         (target (merge-pathnames "result.sexp" root))
         (temporary
          (merge-pathnames
           (make-pathname :name
                          (format nil ".result.~A" (make-identifier))
                          :type "tmp")
           root)))
    (ensure-directories-exist target)
    (when (probe-file target)
      (error 'task-error
             :message
             (format nil "Task artifact pathname is already occupied: ~A"
                     target)
             :tool-name "task.run"
             :task-id (getf (task-job-identity job) :id)))
    (unwind-protect
         (progn
           (with-open-file
               (stream temporary :direction :output :if-exists :supersede
		       :if-does-not-exist :create :external-format :utf-8)
             (with-standard-io-syntax
               (let ((*print-readably* t)
                     (*print-pretty* t)
                     (*print-circle* t))
                 (prin1 result stream)
                 (terpri stream)
                 (finish-output stream))))
           (when (probe-file target)
             (error 'task-error
                    :message
                    (format nil "Task artifact pathname became occupied: ~A"
                            target)
                    :tool-name "task.run"
                    :task-id (getf (task-job-identity job) :id)))
           (rename-file temporary target)
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
          ((task-completion-data-present-p completion)
           (task--bounded-output
            (task--write-readable-sexp
             (task-json->sexp (task-completion-data completion)))))
          ((plusp (task-progress-request-count progress))
           (format nil "(no output) after ~D req"
                   (task-progress-request-count progress)))
          (t "(no output)"))))

(defun task--assemble-child-result
    (job provider-result child conversation completion)
  "Assemble one portable SingleResult-style plist for a completed child."
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
                :structured-output-present-p
                (task-completion-data-present-p completion)
                :structured-output
                (and (task-completion-data-present-p completion)
                     (task-json->sexp (task-completion-data completion)))
                :label
                (task-completion-label completion) :request-count
                (task-progress-request-count progress) :usage
                (task-progress-usage progress) :duration-ms duration :model
                (configuration-model (agent-configuration child))
                :conversation-file
                (namestring (conversation-pathname conversation)) :detached
                (task-job-detached-p job))))
    base))

(defun task--failed-result (job status message)
  "Return a portable terminal failure result for JOB."
  (let* ((parent (task-job-parent-agent job))
         (progress (task-job-progress job))
         (tail (task-progress-output-tail progress))
         (model (and parent
                     (configuration-model (agent-configuration parent)))))
    (list :id (getf (task-job-identity job) :id)
          :name (getf (task-job-identity job) :display-name)
          :agent (task-agent-definition-name (task-job-definition job))
          :agent-source (task-agent-definition-source
                         (task-job-definition job))
          :assignment (getf (task-job-item job) :task)
          :status status
          :output (if (non-empty-string-p tail)
                      (task--bounded-output tail)
                      "(no output)")
          :error message
          :yielded-p nil
          :request-count (task-progress-request-count progress)
          :usage (task-progress-usage progress)
          :duration-ms
          (task--milliseconds-between
           (or (task-job-started-at job) (task-job-created-at job))
           (get-internal-real-time))
          :model model
          :detached (task-job-detached-p job))))

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
          (conversation-create
           configuration
           :identifier "conversation"
           :storage-root (task--artifact-root configuration job)))
         (worker (lisp-worker-pool-create configuration))
         (completion (make-instance 'task-completion))
         (registry
          (task-child-tool-registry (agent-tool-registry parent) definition
                                    orchestrator depth))
         (provider
          (provider-with-configuration (agent-provider parent) configuration))
         (child
          (make-instance 'task-child-agent :configuration configuration
                         :provider provider :conversation conversation
                         :tool-registry registry :worker worker
                         :definition definition
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
                                                                       details))
                                          :command-authorization-callback
                                          (task-job-command-authorization-function
                                           job))))
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

(-> task--retained-prefix (string integer) string)
(defun task--retained-prefix (text limit)
  "Return at most LIMIT leading characters from TEXT."
  (subseq text 0 (min limit (length text))))

(-> task-job--compact-result
    (list &key (:artifact-available-p boolean))
    list)
(defun task-job--compact-result (result &key artifact-available-p)
  "Return a bounded terminal summary of RESULT and its artifact availability."
  (let ((retained
          (loop for field in
                  '(:id :name :agent :agent-source :assignment :status
                    :output :error :yielded-p
                    :structured-output-present-p :structured-output :label
                    :request-count :usage :duration-ms :model
                    :conversation-file :detached :output-path
                    :agent-definition)
                append (list field (getf result field))))
        (storage (if artifact-available-p :artifact :omitted)))
    (flet ((compact-string
              (field limit &key storage-field characters-field)
             (let ((value (getf retained field)))
               (when (and (stringp value) (> (length value) limit))
                 (setf (getf retained field)
                       (task--retained-prefix value limit)
                       (getf retained storage-field) storage
                       (getf retained characters-field) (length value))))))
      (compact-string :assignment +task-retained-assignment-limit+
                      :storage-field :assignment-storage
                      :characters-field :assignment-characters)
      (compact-string :output +task-retained-output-limit+
                      :storage-field :output-storage
                      :characters-field :output-characters)
      (compact-string :error +task-retained-output-limit+
                      :storage-field :error-storage
                      :characters-field :error-characters)
      (compact-string :label +task-result-label-maximum-characters+
                      :storage-field :label-storage
                      :characters-field :label-characters))
    (when (getf retained :structured-output-present-p)
      (let* ((value (getf retained :structured-output))
             (serialized (task--write-readable-sexp value)))
        (when (> (length serialized)
                 +task-retained-structured-output-limit+)
          (setf (getf retained :structured-output) nil
                (getf retained :structured-output-storage) storage
                (getf retained :structured-output-characters)
                (length serialized)))))
    (let* ((usage (getf retained :usage))
           (serialized (and usage (task--write-readable-sexp usage))))
      (when (and serialized
                 (> (length serialized) +task-retained-usage-limit+))
        (setf (getf retained :usage) nil
              (getf retained :usage-storage) storage
              (getf retained :usage-characters) (length serialized))))
    retained))

(-> task-job--compact-progress (task-job keyword) null)
(defun task-job--compact-progress (job state)
  "Make JOB's progress terminal and release its large transient fields."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (let* ((output (task-progress-output-tail progress))
             (start
               (max 0
                    (- (length output)
                       +task-retained-progress-output-limit+))))
        (setf (task-progress-status progress) state
              (task-progress-current-tool progress) nil
              (task-progress-output-tail progress) (subseq output start)
              (task-progress-usage progress)
              (task--compact-native-value
               (task-progress-usage progress)
               +task-retained-usage-limit+)
              (task-progress-updated-at progress) (get-internal-real-time)))))
  nil)

(-> task-job--compact-item (task-job) list)
(defun task-job--compact-item (job)
  "Return the bounded assignment metadata retained for terminal JOB."
  (let ((item (task-job-item job)))
    (list :name (getf item :name)
          :agent (getf item :agent)
          :task (task--retained-prefix
                 (or (getf item :task) "")
                 +task-retained-assignment-limit+)
          :async (getf item :async))))

(-> task--compact-native-value (t integer) t)
(defun task--compact-native-value (value limit)
  "Return native VALUE or a descriptor when its readable form exceeds LIMIT."
  (let ((characters (length (task--write-readable-sexp value))))
    (if (<= characters limit)
        value
        (list :omitted :characters characters))))

(-> task--agent-definition-summary (task-agent-definition) list)
(defun task--agent-definition-summary (definition)
  "Return compact non-instruction metadata for DEFINITION."
  (let ((pathname (task-agent-definition-pathname definition))
        (output (task-agent-definition-output definition)))
    (list :name (task-agent-definition-name definition)
          :source (task-agent-definition-source definition)
          :pathname (and pathname (namestring pathname))
          :tools
          (task--compact-native-value
           (task-agent-definition-tools definition) 1000)
          :spawns
          (task--compact-native-value
           (task-agent-definition-spawns definition) 1000)
          :models
          (task--compact-native-value
           (task-agent-definition-models definition) 1000)
          :reasoning-effort
          (task-agent-definition-reasoning-effort definition)
          :output-contract-p (and output t)
          :blocking-p
          (and (task-agent-definition-blocking-p definition) t))))

(-> task-orchestrator--retain-terminal-locked
    (task-orchestrator task-job)
    null)
(defun task-orchestrator--retain-terminal-locked (orchestrator job)
  "Account for terminal JOB while its lifecycle lock is held."
  (unless (task-job-retained-p job)
    (setf (task-job-retained-p job) t)
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (when (plusp (task-orchestrator-live-count orchestrator))
        (decf (task-orchestrator-live-count orchestrator)))
      (let ((identifier (getf (task-job-identity job) :id)))
        (setf (task-orchestrator-terminal-identifiers orchestrator)
              (nconc (task-orchestrator-terminal-identifiers orchestrator)
                     (list identifier)))
        (loop while (> (length
                        (task-orchestrator-terminal-identifiers orchestrator))
                       +task-terminal-retention-limit+)
              for expired =
                (pop (task-orchestrator-terminal-identifiers orchestrator))
              do (remhash expired (task-orchestrator-jobs orchestrator))
                 (remhash expired (task-orchestrator-names orchestrator))))
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator))))
  nil)

(-> task-orchestrator--retain-terminal (task-orchestrator task-job) null)
(defun task-orchestrator--retain-terminal (orchestrator job)
  "Account for terminal JOB and evict the oldest excess summary."
  (with-lock-held ((task-job-lock job))
    (task-orchestrator--retain-terminal-locked orchestrator job))
  nil)

(-> task-job--lifecycle-event (task-job keyword list) list)
(defun task-job--lifecycle-event (job state result)
  "Return JOB's portable terminal lifecycle event."
  (list :id (getf (task-job-identity job) :id)
        :agent (task-job-agent-name job)
        :agent-source (task-job-agent-source job)
        :status state
        :session-file (getf result :conversation-file)
        :parent-tool-call-id (task-job-parent-call-id job)
        :index (getf (task-job-identity job) :index)
        :detached (task-job-detached-p job)))

(-> task-job--publish-terminal
    (task-job keyword list &key (:report (option string)))
    boolean)
(defun task-job--publish-terminal (job requested-state result &key report)
  "Claim and publish exactly one coherent terminal RESULT for JOB."
  (let ((*task-terminal-publication-job* job)
        (publish-p nil)
        (state requested-state)
        (final-result nil)
        (definition-summary nil)
        (event nil))
    (with-lock-held ((task-job-lock job))
      (unless (or (task-job--terminal-state-p (task-job-state job))
                  (task-job-publication-claimed-p job))
        (when (task-job-cancellation-reason job)
          (setf state :aborted))
        (setf (task-job-publication-claimed-p job) t
              publish-p t)))
    (when publish-p
      (let ((*task-terminal-publication-job* job))
        (handler-case
            (progn
              (setf final-result
                    (if (and (eq state :aborted)
                             (not (eq (getf result :status) :aborted)))
                        (task--failed-result
                         job
                         :aborted
                         (format nil "Task ~A was ~A."
                                 (getf (task-job-identity job) :id)
                                 (task-job-cancellation-reason job)))
                        (copy-list result))
                    (getf final-result :status)
                    (case state
                      (:completed :success)
                      (:aborted :aborted)
                      (otherwise :failed)))
              (setf definition-summary
                    (task--agent-definition-summary (task-job-definition job))
                    (getf final-result :agent-definition)
                    definition-summary)
              (handler-case
                  (setf final-result
                        (append
                         final-result
                         (list
                          :output-path
                          (namestring
                           (task--write-result-artifact job final-result)))))
                (error (condition)
                  (setf state :failed
                        (getf final-result :status) :failed
                        (getf final-result :error)
                        (format nil "Could not persist task artifact: ~A"
                                condition)
                        report
                        (or report
                            (bounded-string
                             (princ-to-string condition)
                             :limit +task-retained-output-limit+)))))
              (setf final-result
                    (task-job--compact-result
                     final-result
                     :artifact-available-p
                     (and (getf final-result :output-path) t))
                    report
                    (and report
                         (bounded-string
                          report :limit +task-retained-output-limit+)))
              (with-lock-held ((task-job-lock job))
                (task-job--compact-progress job state)
                (setf (task-job-state job) state
                      (task-job-publication-claimed-p job) nil
                      (task-job-result job) final-result
                      (task-job-condition-report job) report
                      (task-job-ended-at job) (get-internal-real-time)
                      (task-job-item job) (task-job--compact-item job)
                      (task-job-parent-agent job) nil
                      (task-job-command-authorization-function job) nil
                      (task-job-thread job) nil
                      (task-job-run-token job) nil
                      (task-job-deadline job) nil
                      event (task-job--lifecycle-event job state final-result)
                      (task-job-definition-summary job) definition-summary
                      (task-job-definition job) nil)
                (task-orchestrator--retain-terminal-locked
                 (task-job-orchestrator job) job)
                (task--condition-broadcast (task-job-condition-variable job)))
              (task-orchestrator-emit
               (task-job-orchestrator job) :task-subagent-lifecycle event))
          (serious-condition (condition)
            (task-job--force-terminal-failure job condition condition)))))
    publish-p))

(-> task-job--force-terminal-failure (task-job condition condition) null)
(defun task-job--force-terminal-failure
    (job execution-condition publication-condition)
  "Force JOB terminal when normal terminal publication itself fails."
  (let* ((cancellation-reason
           (with-lock-held ((task-job-lock job))
             (task-job-cancellation-reason job)))
         (state (if cancellation-reason :aborted :failed))
         (status (if cancellation-reason :aborted :failed))
         (report
           (bounded-string
            (format nil "Task failure: ~A; publication failure: ~A"
                    execution-condition publication-condition)
            :limit +task-retained-output-limit+))
         (definition-summary
           (or (task-job-definition-summary job)
               (task--agent-definition-summary (task-job-definition job))))
         (result
           (list :id (getf (task-job-identity job) :id)
                 :name (getf (task-job-identity job) :display-name)
                 :agent (task-job-agent-name job)
                 :status status
                 :output "(no retained output)"
                 :error report
                 :yielded-p nil
                 :structured-output-present-p nil
                 :agent-definition definition-summary
                 :detached (task-job-detached-p job)))
         (event nil))
    (with-lock-held ((task-job-lock job))
      (unless (task-job--terminal-state-p (task-job-state job))
        (task-job--compact-progress job state)
        (setf (task-job-state job) state
              (task-job-publication-claimed-p job) nil
              (task-job-result job) result
              (task-job-condition-report job) report
              (task-job-ended-at job) (get-internal-real-time)
              (task-job-item job) (task-job--compact-item job)
              (task-job-parent-agent job) nil
              (task-job-command-authorization-function job) nil
              (task-job-thread job) nil
              (task-job-run-token job) nil
              (task-job-deadline job) nil
              event (task-job--lifecycle-event job state result)
              (task-job-definition-summary job) definition-summary
              (task-job-definition job) nil)
        (task-orchestrator--retain-terminal-locked
         (task-job-orchestrator job) job)))
    (with-lock-held ((task-job-lock job))
      (task--condition-broadcast (task-job-condition-variable job)))
    (when event
      (task-orchestrator-emit
       (task-job-orchestrator job) :task-subagent-lifecycle event))
    nil))

(-> task-job--execute (task-job) null)
(defun task-job--execute (job)
  "Run JOB on the current reusable worker and publish one terminal result."
  (let* ((orchestrator (task-job-orchestrator job))
         (token (make-identifier))
         (started-p nil)
         (runtime-milliseconds
          (task-orchestrator-maximum-runtime-milliseconds orchestrator)))
    (with-lock-held ((task-job-lock job))
      (when (and (eq (task-job-state job) :queued)
                 (null (task-job-cancellation-reason job)))
        (let ((now (get-internal-real-time)))
          (setf (task-job-state job) :running
                (task-job-thread job) (current-thread)
                (task-job-run-token job) token
                (task-job-started-at job) now
                (task-job-deadline job)
                (and (plusp runtime-milliseconds)
                     (+ now
                        (round (* runtime-milliseconds
                                  internal-time-units-per-second)
                               1000)))
                started-p t)
          (task-job--set-progress-state job :running))))
    (when started-p
      (task-orchestrator-emit
       orchestrator
       :task-subagent-lifecycle
       (list :id (getf (task-job-identity job) :id)
             :agent (task-agent-definition-name (task-job-definition job))
             :agent-source
             (task-agent-definition-source (task-job-definition job))
             :status :started
             :parent-tool-call-id (task-job-parent-call-id job)
             :index (getf (task-job-identity job) :index)
             :detached (task-job-detached-p job)))
      (let ((*task-current-job* job)
            (*task-current-run-token* token))
        (handler-case
            (let* ((result (task-run-child job))
                   (status (getf result :status))
                   (state (cond
                            ((eq status :success) :completed)
                            ((eq status :aborted) :aborted)
                            (t :failed))))
              (task-job--publish-terminal job state result))
          (task-aborted (condition)
            (task-job--publish-terminal
             job
             :aborted
             (task--failed-result job :aborted (princ-to-string condition))
             :report (bounded-string (princ-to-string condition))))
          (error (condition)
            (task-job--publish-terminal
             job
             :failed
             (task--failed-result job :failed (princ-to-string condition))
             :report (bounded-string (princ-to-string condition))))))))
  nil)

(-> task-parent-root-conversation-identifier (agent) non-empty-string)
(defun task-parent-root-conversation-identifier (parent)
  "Return the primary conversation identifier owning PARENT's task tree."
  (if (typep parent 'task-child-agent)
      (task-job-root-conversation-identifier (task-child-agent-job parent))
      (conversation-identifier (agent-conversation parent))))

(-> task-parent-owner-identifiers (agent) list)
(defun task-parent-owner-identifiers (parent)
  "Return the task identifiers authorized to inspect PARENT's descendants."
  (if (typep parent 'task-child-agent)
      (let ((job (task-child-agent-job parent)))
        (append (task-job-owner-identifiers job)
                (list (getf (task-job-identity job) :id))))
      nil))

(-> task-orchestrator--live-job-count (task-orchestrator) (integer 0))
(defun task-orchestrator--live-job-count (orchestrator)
  "Return queued, running, and finalizing jobs while the lock is held."
  (task-orchestrator-live-count orchestrator))

(defun task-orchestrator-start-jobs
    (orchestrator parent-agent entries parent-call-id
     command-authorization-function)
  "Atomically admit ENTRIES and return jobs plus nested synchronous inline jobs."
  (when (and (typep parent-agent 'task-child-agent)
             (not *task-admission-parent-locked-p*))
    (let ((parent-job (task-child-agent-job parent-agent)))
      (with-lock-held ((task-job-lock parent-job))
        (when (or (task-job-cancellation-reason parent-job)
                  (task-job--terminal-state-p (task-job-state parent-job)))
          (error 'task-aborted
                 :message
                 (format nil "Task ~A was cancelled before child admission."
                         (getf (task-job-identity parent-job) :id))
                 :reason
                 (or (task-job-cancellation-reason parent-job) :shutdown)))
        (let ((*task-admission-parent-locked-p* t))
          (return-from task-orchestrator-start-jobs
            (task-orchestrator-start-jobs
             orchestrator parent-agent entries parent-call-id
             command-authorization-function))))))
  (let ((jobs nil)
        (inline nil)
        (queued nil)
        (reserved-identifiers nil)
        (count (length entries))
        (root-conversation-identifier
          (task-parent-root-conversation-identifier parent-agent))
        (owner-identifiers (task-parent-owner-identifiers parent-agent)))
    (when (> count +task-maximum-batch-size+)
      (error 'task-error
             :message
             (format nil "A task batch may contain at most ~D children."
                     +task-maximum-batch-size+)
             :tool-name "task.run"))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (when (or (task-orchestrator-shutdown-p orchestrator)
                (not (eq (task-orchestrator-lifecycle-state orchestrator)
                         :open)))
        (error 'task-error
               :message "The task runtime is shutting down."
               :tool-name "task.run"))
      (when (> (+ (task-orchestrator--live-job-count orchestrator) count)
               +task-maximum-live-jobs+)
        (error 'task-error
               :message
               (format nil "The task runtime admits at most ~D live jobs."
                       +task-maximum-live-jobs+)
               :tool-name "task.run"))
      (handler-case
          (dolist (entry entries)
            (let* ((definition (getf entry :definition))
                   (item (getf entry :item))
                   (detached-p (getf entry :detached))
                   (identity
                     (task-orchestrator--create-identity
                      orchestrator
                      (getf item :name)
                      (task-agent-definition-name definition))))
              (push (getf identity :id) reserved-identifiers)
              (let ((job
                      (make-instance
                       'task-job
                       :orchestrator orchestrator
                       :identity identity
                       :execution-identifier (make-identifier)
                       :definition definition
                       :item item
                       :parent-agent parent-agent
                       :root-conversation-identifier
                       root-conversation-identifier
                       :owner-identifiers owner-identifiers
                       :parent-call-id parent-call-id
                       :detached-p detached-p
                       :command-authorization-function
                       command-authorization-function)))
                (push job jobs)
                (if (and (typep parent-agent 'task-child-agent)
                         (not detached-p))
                    (push job inline)
                    (push job queued)))))
        (error (condition)
          (dolist (identifier reserved-identifiers)
            (remhash identifier (task-orchestrator-names orchestrator)))
          (error condition)))
      (setf jobs (nreverse jobs)
            inline (nreverse inline)
            queued (nreverse queued)
            (task-orchestrator-queue orchestrator)
            (nconc (task-orchestrator-queue orchestrator) queued))
      (dolist (job jobs)
        (setf (gethash (getf (task-job-identity job) :id)
                       (task-orchestrator-jobs orchestrator))
              job))
      (incf (task-orchestrator-live-count orchestrator) count)
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator)))
    (values jobs inline)))

(defun task-orchestrator-start-job
    (orchestrator
     &key parent-agent definition item detached-p parent-call-id
       command-authorization-function)
  "Admit one JOB through the atomic scheduler admission path."
  (multiple-value-bind (jobs inline)
      (task-orchestrator-start-jobs
       orchestrator
       parent-agent
       (list (list :definition definition
                   :item item
                   :detached detached-p))
       parent-call-id
       command-authorization-function)
    (dolist (job inline)
      (task-job--execute job))
    (first jobs)))

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

(-> task--json-boolean (t string) boolean)
(defun task--json-boolean (value field)
  "Return JSON boolean VALUE or reject FIELD's non-boolean value."
  (cond
    ((eq value t) t)
    ((eq value false) nil)
    (t
     (error 'task-error
            :message (format nil "Task field ~S must be a boolean." field)
            :tool-name "task.run"))))

(-> task--validate-json-fields (json-object list string) null)
(defun task--validate-json-fields (object allowed-fields location)
  "Reject fields outside ALLOWED-FIELDS in task JSON OBJECT at LOCATION."
  (loop for field being the hash-keys of object
        unless (member field allowed-fields :test #'string=)
          do (error 'task-error
                    :message
                    (format nil "Unknown task.run field ~S in ~A."
                            field location)
                    :tool-name "task.run"))
  nil)

(defun task--normalize-item (object shared-context top-async)
  "Validate and normalize one flat task OBJECT."
  (unless (json-object-p object)
    (error 'task-error :message "Every tasks item must be a JSON object."
           :tool-name "task.run"))
  (task--validate-json-fields
   object '("name" "agent" "task" "context" "async") "a task item")
  (let ((task (task--repair-prose (json-get object "task")))
        (name (json-get object "name"))
        (agent (or (json-get object "agent") "task"))
        (context (task--repair-prose (json-get object "context")))
        (async
         (multiple-value-bind (value present-p)
             (gethash "async" object)
           (if present-p
               (task--json-boolean value "async")
               top-async))))
    (unless (non-empty-string-p task)
      (error 'task-error :message
             "Every child requires a non-empty task assignment." :tool-name
             "task.run"))
    (when (and name (not (non-empty-string-p name)))
      (error 'task-error :message
             "A supplied task name must be a non-empty string." :tool-name
             "task.run"))
    (when (and name
               (> (length name) +task-identifier-maximum-characters+))
      (error 'task-error
             :message
             (format nil "A task name may contain at most ~D characters."
                     +task-identifier-maximum-characters+)
             :tool-name "task.run"))
    (unless (non-empty-string-p agent)
      (error 'task-error :message "A task agent must be a non-empty string."
             :tool-name "task.run"))
    (when (and (nth-value 1 (gethash "context" object))
               (not (stringp context)))
      (error 'task-error :message
             "A supplied task context must be a string."
             :tool-name "task.run"))
    (list :name name :agent (string-downcase agent) :task task :context
          (task--combine-context shared-context context) :async async)))

(defun task-normalize-arguments (arguments)
  "Validate TASK.RUN ARGUMENTS and return ordinary normalized item plists."
  (task--validate-json-fields
   arguments '("name" "agent" "task" "context" "async" "tasks")
   "the top-level call")
  (let* ((tasks nil)
         (tasks-present-p nil)
         (flat-task nil)
         (flat-task-present-p nil)
         (shared-context (task--repair-prose (json-get arguments "context")))
         (top-async
          (multiple-value-bind (value present-p)
              (gethash "async" arguments)
            (if present-p
                (task--json-boolean value "async")
                nil)))
         (items nil))
    (multiple-value-setq (tasks tasks-present-p)
      (gethash "tasks" arguments))
    (multiple-value-setq (flat-task flat-task-present-p)
      (gethash "task" arguments))
    (when (and (nth-value 1 (gethash "context" arguments))
               (not (stringp shared-context)))
      (error 'task-error :message
             "A supplied task context must be a string."
             :tool-name "task.run"))
    (setf items
          (cond
            (tasks-present-p
             (when flat-task-present-p
               (error 'task-error :message
                      "A batch task call cannot also contain top-level task."
                      :tool-name "task.run"))
             (dolist (field '("name" "agent"))
               (when (nth-value 1 (gethash field arguments))
                 (error 'task-error
                        :message
                        (format nil
                                "A batch task call cannot contain top-level field ~S."
                                field)
                        :tool-name "task.run")))
             (unless (and (vectorp tasks)
                          (not (stringp tasks))
                          (plusp (length tasks)))
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
            (t (list (task--normalize-item arguments nil top-async)))))
    (when (> (length items) +task-maximum-batch-size+)
      (error 'task-error
             :message
             (format nil "A task batch may contain at most ~D children."
                     +task-maximum-batch-size+)
             :tool-name "task.run"))
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

(defun task--resolve-items
    (parent orchestrator definitions &key items diagnostics registry)
  "Resolve ITEMS to definitions after enforcing parent policy and names."
  (let* ((selectable-definitions
           (remove-if-not
            (lambda (definition)
              (task-parent-can-spawn-p
               parent
               (task-agent-definition-name definition)
               orchestrator))
            definitions))
         (selectable-diagnostics
           (remove-if-not
            (lambda (diagnostic)
              (let ((name
                      (task-agent-definition-error-definition-name
                       diagnostic)))
                (and name
                     (task-parent-can-spawn-p parent name orchestrator))))
            diagnostics))
         (selectable-names
           (mapcar #'task-agent-definition-name selectable-definitions)))
    (mapcar
     (lambda (item)
       (let ((name (getf item :agent)))
         (unless (task-parent-can-spawn-p parent name orchestrator)
           (error 'task-error
                  :message "The current agent may not spawn the requested role."
                  :tool-name "task.run"))
         (let ((definition
                 (task-find-agent-definition selectable-definitions name)))
           (unless definition
             (let ((diagnostic
                     (task-find-agent-diagnostic selectable-diagnostics name)))
               (if diagnostic
                   (error diagnostic)
                   (error 'task-error
                          :message
                          (if selectable-names
                              (format nil
                                      "Unknown task agent ~S. Available agents: ~{~A~^, ~}."
                                      name selectable-names)
                              (format nil
                                      "Unknown task agent ~S. No task agents are available."
                                      name))
                          :tool-name "task.run"))))
           (task-agent-definition-validate-tools-available definition registry)
           (list :item item :definition definition :detached
                 (and (getf item :async)
                      (not (task-agent-definition-blocking-p definition)))))))
     items)))

(defun task-tool-result (content details &optional (success-p t))
  "Return exact readable CONTENT and portable DETAILS as a task tool result."
  (unless (and (stringp content)
               (<= (length content) +task-tool-content-limit+))
    (error 'task-error
           :message "A task tool produced an oversized native result."
           :tool-name "task.run"))
  (make-instance 'task-tool-result
                 :content content
                 :success-p (and success-p t)
                 :details details))

(-> task--validate-tool-arguments (t list string) null)
(defun task--validate-tool-arguments (arguments allowed-fields tool-name)
  "Require an object with only ALLOWED-FIELDS for TOOL-NAME."
  (unless (json-object-p arguments)
    (error 'task-error
           :message (format nil "~A arguments must be a JSON object." tool-name)
           :tool-name tool-name))
  (loop for field being the hash-keys of arguments
        unless (member field allowed-fields :test #'string=)
          do (error 'task-error
                    :message
                    (format nil "Unknown ~A field ~S." tool-name field)
                    :tool-name tool-name))
  nil)

(-> task--validate-job-identifier (t string) non-empty-string)
(defun task--validate-job-identifier (value tool-name)
  "Return bounded non-empty job identifier VALUE for TOOL-NAME."
  (unless (and (non-empty-string-p value)
               (<= (length value) +task-identifier-maximum-characters+))
    (error 'task-error
           :message
           (format nil "~A requires a non-empty job id of at most ~D characters."
                   tool-name +task-identifier-maximum-characters+)
           :tool-name tool-name))
  value)

(-> task--artifact-group-root (configuration string) pathname)
(defun task--artifact-group-root (configuration conversation-identifier)
  "Return the common artifact root for one primary conversation."
  (merge-pathnames
   (format nil "tasks/~A/"
           (or (task--identifier-fragment conversation-identifier)
               "conversation"))
   (configuration-data-root configuration)))

(-> task--artifact-field
    (t keyword &key (:preview-limit integer)
                    (:artifact-available-p boolean))
    t)
(defun task--artifact-field
    (value field &key preview-limit artifact-available-p)
  "Return VALUE inline or a typed descriptor naming FIELD in an artifact."
  (cond
    ((null value)
     nil)
    ((stringp value)
     (if (<= (length value) preview-limit)
         value
         (if artifact-available-p
             (list :in-artifact :field field :characters (length value))
             (list :omitted :field field :characters (length value)))))
    (t
     (let ((characters (length (task--write-readable-sexp value))))
       (if (<= characters preview-limit)
           value
           (if artifact-available-p
               (list :in-artifact :field field :characters characters)
               (list :omitted :field field :characters characters)))))))

(-> task--retained-result-field
    (list keyword &key (:preview-limit integer)
                       (:artifact-available-p boolean))
    t)
(defun task--retained-result-field
    (result field &key preview-limit artifact-available-p)
  "Return RESULT FIELD, respecting terminal compaction metadata."
  (let* ((storage-field
           (ecase field
             (:output :output-storage)
             (:error :error-storage)
             (:label :label-storage)
             (:structured-output :structured-output-storage)))
         (characters-field
           (ecase field
             (:output :output-characters)
             (:error :error-characters)
             (:label :label-characters)
             (:structured-output :structured-output-characters)))
         (storage (getf result storage-field))
         (characters (getf result characters-field)))
    (cond
      ((member storage '(:artifact :omitted) :test #'eq)
       (let* ((value (getf result field))
              (artifact-p
                (and (eq storage :artifact) artifact-available-p))
              (descriptor
                (list :field field :characters characters)))
         (if (and (stringp value) (plusp preview-limit))
             (list :preview
                   (task--retained-prefix value preview-limit)
                   (if artifact-p :in-artifact :omitted)
                   descriptor)
             (if artifact-p
                 (list :in-artifact :field field :characters characters)
                 (list :omitted :field field :characters characters)))))
      (t
       (task--artifact-field (getf result field)
                             field
                             :preview-limit preview-limit
                             :artifact-available-p artifact-available-p)))))

(-> task--job-native-record
    (list &key (:artifact-path (option string))
               (:preview-limit integer)
               (:include-progress-p boolean))
    list)
(defun task--job-native-record
    (snapshot &key artifact-path (preview-limit 0) include-progress-p)
  "Return one bounded native job record from SNAPSHOT."
  (let* ((state (getf snapshot :state))
         (terminal-p (task-job--terminal-state-p state))
         (result (getf snapshot :result))
         (progress (getf snapshot :progress))
         (artifact-available-p
           (and terminal-p result (getf result :output-path) t))
         (artifact
           (list :path artifact-path
                 :format :sexp
                 :available-p (and artifact-available-p t)))
         (result-record
           (and result
                (list
                 :status (getf result :status)
                 :error
                 (task--retained-result-field
                  result :error
                  :preview-limit preview-limit
                  :artifact-available-p artifact-available-p)
                 :label
                 (task--retained-result-field
                  result :label
                  :preview-limit preview-limit
                  :artifact-available-p artifact-available-p)
                 :output
                 (task--retained-result-field
                  result :output
                  :preview-limit preview-limit
                  :artifact-available-p artifact-available-p)
                 :structured-output-present-p
                 (and (getf result :structured-output-present-p) t)
                 :structured-output
                 (and (getf result :structured-output-present-p)
                      (task--retained-result-field
                       result :structured-output
                       :preview-limit preview-limit
                       :artifact-available-p artifact-available-p))
                 :duration-ms (getf result :duration-ms)
                 :model (getf result :model)
                 :agent-definition
                 (task--artifact-field
                  (getf result :agent-definition)
                  :agent-definition
                  :preview-limit preview-limit
                  :artifact-available-p artifact-available-p)
                 :artifact artifact))))
    (append
     (list :id (getf snapshot :job-id)
           :execution-id (getf snapshot :execution-id)
           :agent (getf snapshot :agent)
           :state state
           :detached (and (getf snapshot :detached) t)
           :result result-record
           :cancellation-reason (getf snapshot :cancellation-reason))
     (when include-progress-p
       (list
        :progress
        (list :status (getf progress :status)
              :current-tool (getf progress :current-tool)
              :recent-tools (getf progress :recent-tools)
              :recent-output
              (task--artifact-field
               (getf progress :recent-output)
               :progress-output
               :preview-limit preview-limit
               :artifact-available-p nil)
              :request-count (getf progress :request-count)
              :duration-ms (getf progress :duration-ms)
              :model (getf progress :model)))))))

(-> task--task-run-native-form
    (list list &key (:duration-milliseconds integer)
                    (:artifact-root string)
                    (:success-p boolean))
    (values list string))
(defun task--task-run-native-form
    (jobs snapshots &key duration-milliseconds artifact-root success-p)
  "Fit every admitted JOB into one fair bounded native task.run manifest."
  (loop with preview-limit = +task-result-preview-limit+
        for records =
          (loop for job in jobs
                for snapshot in snapshots
                collect
                (task--job-native-record
                 snapshot
                 :artifact-path
                 (format nil "~A/result.sexp"
                         (task-job-execution-identifier job))
                 :preview-limit preview-limit))
        for form =
          (list :task-run
                :succeeded-p (and success-p t)
                :total-duration-ms duration-milliseconds
                :artifact-root artifact-root
                :results records)
        for content = (task--write-readable-sexp form :pretty-p t)
        when (<= (length content) +task-tool-content-limit+)
          return (values form content)
        when (zerop preview-limit)
          do (error 'task-error
                    :message
                    "The mandatory task.run manifest exceeds its native result bound."
                    :tool-name "task.run")
        do (setf preview-limit (floor preview-limit 2))))

(-> task--job-native-form
    (task-job list agent &key (:preview-limit integer)
                              (:wrapper (option function)))
    (values list string))
(defun task--job-native-form
    (job snapshot viewer &key (preview-limit 6000) wrapper)
  "Fit one JOB snapshot into a bounded native tool result."
  (let* ((root
           (task--artifact-group-root
            (agent-configuration viewer)
            (task-job-root-conversation-identifier job)))
         (artifact-path
           (namestring
            (merge-pathnames
             (format nil "~A/result.sexp"
                     (task-job-execution-identifier job))
             root))))
    (loop for limit = preview-limit then (floor limit 2)
          for record =
            (task--job-native-record
             snapshot
             :artifact-path artifact-path
             :preview-limit limit
             :include-progress-p t)
          for form = (if wrapper
                         (funcall wrapper record)
                         (list :job record))
          for content = (task--write-readable-sexp form :pretty-p t)
          when (<= (length content) +task-tool-content-limit+)
            return (values form content)
          when (zerop limit)
            do (error 'task-error
                      :message
                      "The mandatory job snapshot exceeds its native result bound."
                      :tool-name "job.get"))))

(-> task--agent-policy-presentation (t) t)
(defun task--agent-policy-presentation (value)
  "Return bounded policy VALUE for task.agents discovery."
  (task--compact-native-value value 1000))

(-> task--agent-native-record (task-agent-definition) list)
(defun task--agent-native-record (definition)
  "Return model-visible native metadata for one child role."
  (list :kind :agent
        :name (task-agent-definition-name definition)
        :description
        (task--retained-prefix
         (task-agent-definition-description definition) 1000)
        :source (task-agent-definition-source definition)
        :pathname
        (let ((pathname (task-agent-definition-pathname definition)))
          (and pathname (namestring pathname)))
        :models
        (task--agent-policy-presentation
         (task-agent-definition-models definition))
        :reasoning-effort
        (task-agent-definition-reasoning-effort definition)
        :tools
        (task--agent-policy-presentation
         (task-agent-definition-tools definition))
        :spawns
        (task--agent-policy-presentation
         (task-agent-definition-spawns definition))
        :output-contract-p
        (and (task-agent-definition-output definition) t)
        :blocking-p (and (task-agent-definition-blocking-p definition) t)))

(-> task--agent-diagnostic-native-record (condition) list)
(defun task--agent-diagnostic-native-record (diagnostic)
  "Return bounded typed native metadata for one rejected role."
  (let ((pathname (task-agent-definition-error-pathname diagnostic)))
    (list :kind :diagnostic
          :type :task-agent-definition-error
          :name (task-agent-definition-error-definition-name diagnostic)
          :source (task-agent-definition-error-source diagnostic)
          :pathname (and pathname (namestring pathname))
          :line (task-agent-definition-error-line diagnostic)
          :field (task-agent-definition-error-field diagnostic)
          :cause
          (task--retained-prefix
           (princ-to-string (task-agent-definition-error-cause diagnostic))
           1000))))

(-> task--agent-diagnostic-visible-p
    (condition agent task-orchestrator)
    boolean)
(defun task--agent-diagnostic-visible-p (diagnostic parent orchestrator)
  "Return true when PARENT policy permits DIAGNOSTIC's reserved role name."
  (let ((name (task-agent-definition-error-definition-name diagnostic)))
    (and name (task-parent-can-spawn-p parent name orchestrator))))

(-> task--agents-page (list integer integer) (values list string))
(defun task--agents-page (entries offset limit)
  "Return a bounded native page of ENTRIES starting at OFFSET."
  (let* ((total (length entries))
         (end (min total (+ offset limit)))
         (candidates (subseq entries (min offset total) end))
         (selected nil))
    (dolist (entry candidates)
      (let* ((trial (nconc (copy-list selected) (list entry)))
             (next (+ offset (length trial)))
             (form
               (list :task-agents
                     :offset offset
                     :count (length trial)
                     :total total
                     :next-offset (and (< next total) next)
                     :entries trial))
             (content (task--write-readable-sexp form :pretty-p t)))
        (if (<= (length content) +task-tool-content-limit+)
            (setf selected trial)
            (return))))
    (when (and candidates (null selected))
      (error 'task-error
             :message
             "One task agent discovery record exceeds the native result bound."
             :tool-name "task.agents"))
    (let* ((next (+ offset (length selected)))
           (form
             (list :task-agents
                   :offset offset
                   :count (length selected)
                   :total total
                   :next-offset (and (< next total) next)
                   :entries selected)))
      (values form (task--write-readable-sexp form :pretty-p t)))))

(-> task--job-list-page (list integer integer) (values list string))
(defun task--job-list-page (snapshots offset limit)
  "Return a content-aware native page of compact job summaries."
  (let* ((total (length snapshots))
         (end (min total (+ offset limit)))
         (candidates (subseq snapshots (min offset total) end))
         (selected nil))
    (dolist (snapshot candidates)
      (let* ((summary
               (list :id (getf snapshot :job-id)
                     :agent (getf snapshot :agent)
                     :state (getf snapshot :state)
                     :detached (and (getf snapshot :detached) t)
                     :status (getf (getf snapshot :result) :status)))
             (trial (nconc (copy-list selected) (list summary)))
             (next (+ offset (length trial)))
             (form
               (list :job-list
                     :offset offset
                     :count (length trial)
                     :total total
                     :next-offset (and (< next total) next)
                     :jobs trial))
             (content (task--write-readable-sexp form :pretty-p t)))
        (if (<= (length content) +task-tool-content-limit+)
            (setf selected trial)
            (return))))
    (when (and candidates (null selected))
      (error 'task-error
             :message "One job summary exceeds the native result bound."
             :tool-name "job.list"))
    (let* ((next (+ offset (length selected)))
           (form
             (list :job-list
                   :offset offset
                   :count (length selected)
                   :total total
                   :next-offset (and (< next total) next)
                   :jobs selected)))
      (values form (task--write-readable-sexp form :pretty-p t)))))

(defmethod tool-execute ((tool task-run-tool) (context tool-context) arguments)
  "Validate, fan out, and aggregate synchronous and detached child agents."
  (let ((parent (tool-context-agent context)))
    (unless (typep parent 'agent)
      (error 'task-error :message
             "task.run requires an executing parent agent context." :tool-name
             "task.run"))
    (multiple-value-bind (definitions diagnostics)
        (task-discover-agents (agent-configuration parent))
      (let* ((orchestrator
              (task-orchestrator-refresh (task-run-tool-orchestrator tool)))
             (items (task-normalize-arguments arguments))
             (resolved
              (task--resolve-items
               parent orchestrator definitions
               :items items
               :diagnostics diagnostics
               :registry (agent-tool-registry parent)))
             (jobs nil)
             (synchronous nil)
             (detached nil)
             (completed-p nil))
        (unwind-protect
           (progn
             (multiple-value-bind (admitted inline)
                 (task-orchestrator-start-jobs
                  orchestrator
                  parent
                  resolved
                  (tool-context-call-id context)
                  (tool-context-command-authorization-function context))
               (setf jobs admitted
                     synchronous
                     (remove-if #'task-job-detached-p admitted)
                     detached
                     (remove-if-not #'task-job-detached-p admitted))
               (dolist (job inline)
                 (task-job--execute job)))
             (dolist (job synchronous) (task-job-await job nil))
             (let* ((snapshots (mapcar #'task-job-snapshot jobs))
                    (synchronous-snapshots
                      (remove-if
                       (lambda (snapshot) (getf snapshot :detached))
                       snapshots))
                    (success-p
                      (every
                       (lambda (snapshot)
                         (eq (getf (getf snapshot :result) :status) :success))
                       synchronous-snapshots))
                    (duration
                     (if jobs
                         (task--milliseconds-between
                          (reduce #'min jobs :key #'task-job-created-at)
                          (get-internal-real-time))
                         0))
                    (artifact-root
                      (namestring
                       (task--artifact-group-root
                        (agent-configuration parent)
                        (task-parent-root-conversation-identifier parent)))))
               (multiple-value-bind (form content)
                   (task--task-run-native-form
                    jobs snapshots
                    :duration-milliseconds duration
                    :artifact-root artifact-root
                    :success-p success-p)
                 (setf completed-p t)
                 (task-tool-result content form success-p))))
          (unless completed-p
            (dolist (job jobs)
              (task-job-cancel job :signal))))))))

(defmethod tool-execute
    ((tool task-agents-tool) (context tool-context) arguments)
  "Return the effective policy-filtered child roles as native data."
  (let* ((parent (tool-context-agent context))
         (orchestrator (task-agents-tool-orchestrator tool)))
    (unless (typep parent 'agent)
      (error 'task-error
             :message "task.agents requires an executing parent agent context."
             :tool-name "task.agents"))
    (task--validate-tool-arguments arguments '("offset" "limit")
                                   "task.agents")
    (let ((offset (or (tool-argument arguments "offset") 0))
          (limit (or (tool-argument arguments "limit")
                     +task-agent-page-default+)))
      (unless (and (integerp offset) (<= 0 offset 1000000))
        (error 'task-error
               :message "task.agents offset must be an integer from 0 to 1000000."
               :tool-name "task.agents"))
      (unless (and (integerp limit)
                   (<= 1 limit +task-agent-page-maximum+))
        (error 'task-error
               :message
               (format nil "task.agents limit must be an integer from 1 to ~D."
                       +task-agent-page-maximum+)
               :tool-name "task.agents"))
      (multiple-value-bind (definitions diagnostics)
          (task-discover-agents (agent-configuration parent))
        (let ((agent-records nil)
              (diagnostic-records nil)
              (registry (agent-tool-registry parent)))
          (dolist (definition definitions)
            (when (task-parent-can-spawn-p
                   parent
                   (task-agent-definition-name definition)
                   orchestrator)
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     definition registry)
                    (push (task--agent-native-record definition)
                          agent-records))
                (task-agent-definition-error (diagnostic)
                  (push (task--agent-diagnostic-native-record diagnostic)
                        diagnostic-records)))))
          (dolist (diagnostic diagnostics)
            (when (task--agent-diagnostic-visible-p
                   diagnostic parent orchestrator)
              (push (task--agent-diagnostic-native-record diagnostic)
                    diagnostic-records)))
          (multiple-value-bind (form content)
              (task--agents-page
               (nconc (nreverse agent-records)
                      (nreverse diagnostic-records))
               offset
               limit)
            (task-tool-result content form)))))))

(defmethod tool-execute ((tool task-job-tool) (context tool-context) arguments)
  "Execute the job operation named by TOOL."
  (let* ((viewer (tool-context-agent context))
         (orchestrator (task-job-tool-orchestrator tool))
         (operation (tool-name tool)))
    (unless (typep viewer 'agent)
      (error 'task-error
             :message "Job tools require an executing agent context."
             :tool-name (tool-canonical-name tool)))
    (cond
      ((string= operation "list")
       (task--validate-tool-arguments arguments '("offset" "limit")
                                      "job.list")
       (let ((offset (or (tool-argument arguments "offset") 0))
             (limit (or (tool-argument arguments "limit")
                        +task-job-page-default+)))
         (unless (and (integerp offset) (<= 0 offset 1000000))
           (error 'task-error
                  :message "job.list offset must be an integer from 0 to 1000000."
                  :tool-name "job.list"))
         (unless (and (integerp limit)
                      (<= 1 limit +task-job-page-maximum+))
           (error 'task-error
                  :message
                  (format nil "job.list limit must be an integer from 1 to ~D."
                          +task-job-page-maximum+)
                  :tool-name "job.list"))
         (let* ((jobs
                  (task-orchestrator-list-visible-jobs orchestrator viewer))
                (snapshots (mapcar #'task-job-snapshot jobs)))
           (multiple-value-bind (form content)
               (task--job-list-page snapshots offset limit)
             (task-tool-result content form)))))
      ((member operation '("get" "wait" "cancel") :test #'string=)
       (task--validate-tool-arguments
        arguments
        (if (string= operation "wait")
            '("id" "timeout-seconds")
            '("id"))
        (tool-canonical-name tool))
       (let* ((identifier
                (task--validate-job-identifier
                 (tool-argument arguments "id" :required t)
                 (tool-canonical-name tool)))
              (job (task-orchestrator-find-visible-job
                    orchestrator
                    identifier
                    viewer
                    (tool-canonical-name tool))))
         (cond
           ((string= operation "cancel")
            (multiple-value-bind (accepted-p cancelled-descendants)
                (task-job-cancel job :user)
              (let ((snapshot (task-job-snapshot job)))
                (multiple-value-bind (form content)
                    (task--job-native-form
                     job snapshot viewer
                     :wrapper
                     (lambda (record)
                       (list :job-cancel
                             :id identifier
                             :accepted-p (and accepted-p t)
                             :reason :user
                             :cancelled-descendants cancelled-descendants
                             :job record)))
                  (task-tool-result content form)))))
           ((string= operation "wait")
            (let ((timeout (or (tool-argument arguments "timeout-seconds")
                               60)))
              (unless (and (integerp timeout)
                           (<= 0 timeout +task-job-wait-maximum-seconds+))
                (error 'task-error
                       :message
                       (format nil
                               "job.wait timeout-seconds must be an integer from 0 to ~D."
                               +task-job-wait-maximum-seconds+)
                       :tool-name "job.wait"))
              (when (and (plusp timeout)
                         (typep viewer 'task-child-agent))
                (task-job-help-join job))
              (multiple-value-bind (snapshot terminal-p)
                  (task-job-await job timeout)
                (multiple-value-bind (form content)
                    (task--job-native-form
                     job snapshot viewer
                     :wrapper
                     (lambda (record)
                       (list :job-wait
                             :timeout-seconds timeout
                             :terminal-p terminal-p
                             :job record)))
                  (task-tool-result content form)))))
           (t
            (let ((snapshot (task-job-snapshot job)))
              (multiple-value-bind (form content)
                  (task--job-native-form job snapshot viewer)
                (task-tool-result content form)))))))
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
                       "async"
                       (tool-boolean-property
                        "Detach non-blocking children as background jobs.")
                       "tasks"
                       (json-object "type" "array" "description"
                                    "Child assignments executed with shared context."
                                    "items" item-schema "minItems" 1
                                    "maxItems" +task-maximum-batch-size+))))
    (tool-object-schema properties nil)))

(-> task-agents-parameters-schema () hash-table)
(defun task-agents-parameters-schema ()
  "Return the pagination schema advertised by task.agents."
  (tool-object-schema
   (json-object
    "offset"
    (tool-integer-property "Zero-based discovery offset; defaults to 0.")
    "limit"
    (tool-integer-property
     (format nil "Page size from 1 to ~D; defaults to ~D."
             +task-agent-page-maximum+
             +task-agent-page-default+)))
   nil))

(defun task-augment-tool-registry (registry)
  "Register one session-scoped task orchestrator and its task/job tools."
  (when (tool-registry-find registry "task" "run")
    (return-from task-augment-tool-registry registry))
  (let* ((orchestrator (task-orchestrator-create))
         (identifier-schema
          (tool-object-schema
           (json-object "id" (tool-string-property "The task job identifier."))
           '("id")))
         (job-list-schema
           (tool-object-schema
            (json-object
             "offset"
             (tool-integer-property "Zero-based job offset; defaults to 0.")
             "limit"
             (tool-integer-property
              (format nil "Page size from 1 to ~D; defaults to ~D."
                      +task-job-page-maximum+
                      +task-job-page-default+)))
            nil)))
    (tool-registry-register registry
                            (make-instance 'task-run-tool :orchestrator
                                           orchestrator :namespace "task" :name
                                           "run" :description
                                           "Spawn a real in-process child agent or a concurrency-limited batch. Children have explicit identities, restricted tools, recursion policy, progress, artifacts, and a required yield protocol."
                                           :parameters
                                           (task-run-parameters-schema)))
    (tool-registry-register registry
                            (make-instance
                             'task-agents-tool
                             :orchestrator orchestrator
                             :namespace "task"
                             :name "agents"
                             :description
                             "List child-agent roles allowed by the current depth and spawn policy, plus typed diagnostics for unavailable definitions."
                             :parameters (task-agents-parameters-schema)))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "list" :description
                                           "List synchronous and detached task jobs in this session."
                                           :parameters job-list-schema))
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
