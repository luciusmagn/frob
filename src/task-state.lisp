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
   (current-tool
    :initform nil
    :accessor task-progress-current-tool
    :type (option string)
    :documentation "The tool currently executing in the child.")
   (recent-tools
    :initform nil
    :accessor task-progress-recent-tools
    :type list
    :documentation "The newest completed child tools, newest first.")
   (output-tail
    :initform ""
    :accessor task-progress-output-tail
    :type string
    :documentation "The bounded tail of streamed assistant text.")
   (request-count
    :initform 0
    :accessor task-progress-request-count
    :type (integer 0)
    :documentation "The provider requests started by the child.")
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
  ((orchestrator
    :initarg :orchestrator
    :reader task-job-orchestrator
    :type task-orchestrator
    :documentation "The session orchestrator owning this job.")
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
   (parent-call-id
    :initarg :parent-call-id
    :initform nil
    :reader task-job-parent-call-id
    :type (option string)
    :documentation "The task.run function call that created this child.")
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
   (cancellation-reason
    :initform nil
    :accessor task-job-cancellation-reason
    :type (option keyword)
    :documentation "The structured cancellation reason requested by a controller.")
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
   (orchestrator
    :initarg :orchestrator
    :reader task-child-agent-orchestrator
    :type task-orchestrator
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
