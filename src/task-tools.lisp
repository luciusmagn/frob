(in-package #:autolith)

;;;; -- Task Provider Tools --

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
           (or (conversation-identifier-path-fragment conversation-identifier)
               (task--identifier-fragment conversation-identifier)
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
