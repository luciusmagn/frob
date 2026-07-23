(in-package #:autolith)

;;;; -- Scheduler Boundary Tests --

(-> task-tests--release-blocking-tool (task-test-blocking-tool) null)
(defun task-tests--release-blocking-tool (tool)
  "Permit TOOL to return normally when a cancellation test is unwinding."
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-released-p tool) t)
    (task--condition-broadcast
     (task-test-blocking-tool-condition-variable tool)))
  nil)

(-> test-task-running-cancellation () null)
(defun test-task-running-cancellation ()
  "Test prompt cancellation while a child executes an ordinary tool call."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait until the cancellation test releases this call."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode :blocking-tool))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "running-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "blocked-running-child"
                           "agent" "task"
                           "task" "Enter the blocking ordinary tool."
                           "async" t))
             (let ((job (first (task-orchestrator-list-jobs orchestrator))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held
                       ((task-test-blocking-tool-lock blocking-tool))
                     (task-test-blocking-tool-started-p blocking-tool)))
                 2)
                "the child reaches its ordinary tool call before cancellation")
               (let ((started-at (get-internal-real-time)))
                 (multiple-value-bind (accepted-p descendants)
                     (task-job-cancel job :user)
                   (declare (ignore descendants))
                   (multiple-value-bind (snapshot terminal-p)
                       (task-job-await job 2)
                     (test-assert
                      (and accepted-p
                           terminal-p
                           (eq (getf snapshot :state) :aborted)
                           (eq (getf (getf snapshot :result) :status)
                               :aborted)
                           (< (task--milliseconds-between
                               started-at (get-internal-real-time))
                              1000))
                      "running cancellation unwinds an ordinary tool call promptly"))))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held ((task-orchestrator-lock orchestrator))
                     (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator)))))
                 2)
                "cancelled ordinary tool execution releases scheduler accounting")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist :ignore))))
  nil)

(-> test-task-runtime-deadline () null)
(defun test-task-runtime-deadline ()
  "Test the finite default and timeout of a stalled synchronous child."
  (let ((previous-runtime
          (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS")
           (let ((orchestrator (task-orchestrator-create)))
             (test-assert
              (and
               (plusp
                (task-orchestrator-maximum-runtime-milliseconds orchestrator))
               (= (task-orchestrator-maximum-runtime-milliseconds orchestrator)
                  *task-default-maximum-runtime-milliseconds*))
              "task children have a finite default runtime deadline"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000" 1)
           (let* ((configuration (test-configuration))
                  (root          (test-configuration-root configuration))
                  (registry      (make-default-tool-registry))
                  (blocking-tool
                    (make-instance
                     'task-test-blocking-tool
                     :namespace "test"
                     :name "block"
                     :description
                     "Wait until the deadline test releases this call."
                     :parameters (tool-object-schema (json-object) nil)))
                  (runner nil)
                  (run-result nil)
                  (run-condition nil))
             (tool-registry-register registry blocking-tool)
             (task-augment-tool-registry registry)
             (let* ((provider
                      (make-instance 'task-test-provider
                                     :mode :blocking-tool))
                    (conversation (conversation-create configuration))
                    (primary
                      (agent-create :configuration configuration
                                    :provider provider
                                    :conversation conversation
                                    :tool-registry registry
                                    :worker nil))
                    (run-tool (tool-registry-find registry "task" "run"))
                    (orchestrator (task-run-tool-orchestrator run-tool))
                    (context
                      (make-instance
                       'tool-context
                       :configuration configuration
                       :worker nil
                       :conversation conversation
                       :registry registry
                       :agent primary
                       :call-id "runtime-deadline")))
               (unwind-protect
                    (progn
                      (setf runner
                            (make-thread
                             (lambda ()
                               (handler-case
                                   (setf run-result
                                         (tool-execute
                                          run-tool
                                          context
                                          (json-object
                                           "name" "stalled-child"
                                           "agent" "task"
                                           "task"
                                           "Enter the blocking ordinary tool.")))
                                 (serious-condition (condition)
                                   (setf run-condition condition))))
                             :name "Autolith task deadline test"))
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-blocking-tool-lock blocking-tool))
                            (task-test-blocking-tool-started-p blocking-tool)))
                        2)
                       "the child stalls inside a real ordinary tool call")
                      (test-assert
                       (task-tests--wait-until
                        (lambda () (not (thread-alive-p runner)))
                        3)
                       "the runtime deadline releases synchronous task.run")
                      (join-thread runner)
                      (setf runner nil)
                      (let* ((jobs
                               (task-orchestrator-list-jobs orchestrator))
                             (snapshot
                               (and (= (length jobs) 1)
                                    (task-job-snapshot (first jobs)))))
                        (test-assert
                         (and (null run-condition)
                              (typep run-result 'tool-result)
                              (not (tool-result-success-p run-result))
                              snapshot
                              (eq (getf snapshot :state) :aborted)
                              (eq (getf snapshot :cancellation-reason)
                                  :timeout)
                              (eq (getf (getf snapshot :result) :status)
                                  :aborted)
                              (task-tests--wait-until
                               (lambda ()
                                 (with-lock-held
                                     ((task-orchestrator-lock orchestrator))
                                   (and
                                    (zerop
                                     (task-orchestrator-active-count
                                      orchestrator))
                                    (zerop
                                     (task-orchestrator-live-count
                                      orchestrator)))))
                               2))
                         "a deadline publishes one timeout result and releases scheduler accounting")))
                 (task-tests--release-blocking-tool blocking-tool)
                 (when runner
                   (join-thread runner))
                 (ignore-errors
                   (tool-registry-close-runtime-state registry))
                 (uiop:delete-directory-tree root :validate t
                                                  :if-does-not-exist
                                                  :ignore)))))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS"
                          previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-nested-parent-cancellation () null)
(defun test-task-nested-parent-cancellation ()
  "Test parent cancellation while its synchronous descendant is running."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry))
         (blocking-tool
           (make-instance
            'task-test-blocking-tool
            :namespace "test"
            :name "block"
            :description "Wait inside a nested synchronous child."
            :parameters (tool-object-schema (json-object) nil))))
    (tool-registry-register registry blocking-tool)
    (task-augment-tool-registry registry)
    (let* ((provider
             (make-instance 'task-test-provider :mode :nested-cancel))
           (conversation (conversation-create configuration))
           (primary
             (agent-create :configuration configuration
                           :provider provider
                           :conversation conversation
                           :tool-registry registry
                           :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (context
             (make-instance 'tool-context
                            :configuration configuration
                            :worker nil
                            :conversation conversation
                            :registry registry
                            :agent primary
                            :call-id "nested-parent-cancellation")))
      (unwind-protect
           (progn
             (tool-execute
              run-tool context
              (json-object "name" "nested-cancel-parent"
                           "agent" "task"
                           "task" "Delegate synchronously, then wait."
                           "async" t))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held
                     ((task-test-blocking-tool-lock blocking-tool))
                   (task-test-blocking-tool-started-p blocking-tool)))
               2)
              "the synchronous descendant reaches its blocking tool")
             (let* ((jobs (task-orchestrator-list-jobs orchestrator))
                    (parent (first jobs))
                    (descendant (second jobs))
                    (descendant-id
                      (getf (task-job-identity descendant) :id)))
               (test-assert (= (length jobs) 2)
                            "nested cancellation observes parent and descendant")
               (multiple-value-bind (accepted-p descendants)
                   (task-job-cancel parent :user)
                 (test-assert
                  (and accepted-p
                       (equal descendants (list descendant-id)))
                  "parent cancellation reaches the live synchronous descendant"))
               (dolist (job jobs)
                 (multiple-value-bind (snapshot terminal-p)
                     (task-job-await job 2)
                   (test-assert
                    (and terminal-p
                         (eq (getf snapshot :state) :aborted)
                         (eq (getf (getf snapshot :result) :status)
                             :aborted))
                    "parent and synchronous descendant both terminalize as aborted")))
               (test-assert
                (task-tests--wait-until
                 (lambda ()
                   (with-lock-held ((task-orchestrator-lock orchestrator))
                     (and (zerop
                           (task-orchestrator-active-count orchestrator))
                          (zerop
                           (task-orchestrator-live-count orchestrator))
                          (null (task-orchestrator-queue orchestrator)))))
                 2)
                "nested parent cancellation leaves no orphan or live-count leak")))
        (task-tests--release-blocking-tool blocking-tool)
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist :ignore))))
  nil)

(-> task-tests--lock-held-by-another-p (t) boolean)
(defun task-tests--lock-held-by-another-p (lock)
  "Return true when LOCK cannot be acquired immediately by this thread."
  (if (bordeaux-threads:acquire-lock lock nil)
      (progn
        (bordeaux-threads:release-lock lock)
        nil)
      t))

(-> test-task-admission-cancellation-barrier () null)
(defun test-task-admission-cancellation-barrier ()
  "Test that nested admission and parent cancellation form one atomic boundary."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "admission-race"
            :description "Exercise admission and cancellation ordering."
            :instructions "Remain queued for the scheduler race."
            :spawns :all
            :source :test))
         (primary
           (task-tests--primary-agent configuration "admission-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-orchestrator-create))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition :name "race-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entries
                    (list
                     (list
                      :definition definition
                      :item
                      (list :name "racing-descendant"
                            :agent "admission-race"
                            :task "Be admitted before cancellation scans."
                            :context nil
                            :async t)
                      :detached t)))
                  (admitted nil)
                  (admission-condition nil)
                  (cancel-started-p nil)
                  (cancel-accepted-p nil)
                  (cancelled-descendants nil)
                  (admission-thread nil)
                  (cancel-thread nil)
                  (orchestrator-lock-held-p nil))
             (unwind-protect
                  (progn
                    (bordeaux-threads:acquire-lock
                     (task-orchestrator-lock orchestrator))
                    (setf orchestrator-lock-held-p t
                          admission-thread
                          (make-thread
                           (lambda ()
                             (handler-case
                                 (setf admitted
                                       (first
                                        (multiple-value-list
                                         (task-orchestrator-start-jobs
                                          orchestrator viewer entries
                                          nil nil))))
                               (condition (condition)
                                 (setf admission-condition condition))))
                           :name "Autolith admission race"))
                    (test-assert
                     (task-tests--wait-until
                      (lambda ()
                        (task-tests--lock-held-by-another-p
                         (task-job-lock parent)))
                      2)
                     "nested admission holds the parent lifecycle lock before commit")
                    (setf cancel-thread
                          (make-thread
                           (lambda ()
                             (setf cancel-started-p t)
                             (multiple-value-setq
                                 (cancel-accepted-p cancelled-descendants)
                               (task-job-cancel parent :user)))
                           :name "Autolith cancellation race"))
                    (test-assert
                     (task-tests--wait-until
                      (lambda () cancel-started-p)
                      2)
                     "competing cancellation reaches the parent barrier")
                    (bordeaux-threads:release-lock
                     (task-orchestrator-lock orchestrator))
                    (setf orchestrator-lock-held-p nil)
                    (join-thread admission-thread)
                    (setf admission-thread nil)
                    (join-thread cancel-thread)
                    (setf cancel-thread nil)
                    (let* ((child (first admitted))
                           (child-id
                             (and child
                                  (getf (task-job-identity child) :id))))
                      (test-assert
                       (and (null admission-condition)
                            (= (length admitted) 1)
                            cancel-accepted-p
                            (equal cancelled-descendants (list child-id))
                            (eq (task-job-state parent) :aborted)
                            (eq (task-job-state child) :aborted)
                            (member
                             (getf (task-job-identity parent) :id)
                             (task-job-owner-identifiers child)
                             :test #'string=)
                            (zerop
                             (task-orchestrator-live-count orchestrator)))
                       "admission that wins the barrier is visible to cascading cancellation")))
               (when orchestrator-lock-held-p
                 (bordeaux-threads:release-lock
                  (task-orchestrator-lock orchestrator)))
               (when admission-thread
                 (join-thread admission-thread))
               (when cancel-thread
                 (join-thread cancel-thread))))
           (let* ((orchestrator (task-orchestrator-create))
                  (parent
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "cancel-first-parent"))
                  (viewer (task-tests--child-viewer configuration parent))
                  (entry
                    (list
                     :definition definition
                     :item
                     (list :name "too-late"
                           :agent "admission-race"
                           :task "This child must not be admitted."
                           :context nil
                           :async t)
                     :detached t)))
             (task-job-cancel parent :user)
             (test-assert
              (handler-case
                  (progn
                    (task-orchestrator-start-jobs
                     orchestrator viewer (list entry) nil nil)
                    nil)
                (task-aborted ()
                  t))
              "admission loses atomically when parent cancellation wins")
             (test-assert
              (and (= (hash-table-count
                       (task-orchestrator-jobs orchestrator))
                      1)
                   (zerop (task-orchestrator-live-count orchestrator))
                   (= (task-orchestrator-next-index orchestrator) 1))
              "cancel-first admission consumes no identity or live capacity")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> task-tests--release-publication-barrier
    (task-test-publication-barrier)
    null)
(defun task-tests--release-publication-barrier (barrier)
  "Permit BARRIER's artifact printer to finish or signal its test failure."
  (with-lock-held ((task-test-publication-barrier-lock barrier))
    (setf (task-test-publication-barrier-released-p barrier) t)
    (task--condition-broadcast
     (task-test-publication-barrier-condition-variable barrier)))
  nil)

(-> test-task-publication-coherence () null)
(defun test-task-publication-coherence ()
  "Test coherent snapshots, forced failure, and terminal role compaction."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (secret
           "AUTOLITH-TERMINAL-ROLE-INSTRUCTION-SENTINEL-71D21A")
         (definition
           (task-agent-definition-create
            :name "publication"
            :description "Exercise terminal publication."
            :instructions secret
            :tools :all
            :spawns :all
            :models '("@task")
            :reasoning-effort :high
            :source :test))
         (primary
           (task-tests--primary-agent configuration "publication-primary")))
    (unwind-protect
         (progn
           (let* ((job
                    (task-tests--register-job
                     orchestrator primary definition
                     :name "coherent-publication"))
                  (barrier (make-instance 'task-test-publication-barrier))
                  (result
                    (task-tests--terminal-result
                     job :status :success :output "published"))
                  (publication-result nil)
                  (publication-condition nil))
             (setf (getf result :publication-barrier) barrier
                   (getf result :portable-integer) 42)
             (let ((thread
                     (make-thread
                      (lambda ()
                        (let ((*print-base* 2)
                              (*print-radix* nil)
                              (*print-case* :downcase))
                          (handler-case
                              (setf publication-result
                                    (task-job--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition)))))
                      :name "Autolith coherent publication")))
               (unwind-protect
                    (progn
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "terminal publication reaches its post-claim artifact phase")
                      (dotimes (index 32)
                        (declare (ignore index))
                        (let ((snapshot (task-job-snapshot job)))
                          (test-assert
                           (and (eq (getf snapshot :state) :queued)
                                (null (getf snapshot :result)))
                           "a concurrent snapshot never exposes a partial terminal result")))
                      (test-assert
                       (with-lock-held ((task-job-lock job))
                         (task-job-publication-claimed-p job))
                       "snapshot sampling occurs while terminal publication is claimed"))
                 (task-tests--release-publication-barrier barrier))
               (join-thread thread))
             (let* ((snapshot (task-job-snapshot job))
                    (summary (task-job-definition-summary job))
                    (result-summary
                      (getf (getf snapshot :result) :agent-definition))
                    (artifact
                      (task-tests--read-exact-native-value
                       (uiop:read-file-string
                        (getf (getf snapshot :result) :output-path)))))
               (test-assert
                (and publication-result
                     (null publication-condition)
                     (eq (getf snapshot :state) :completed)
                     (eq (getf (getf snapshot :result) :status) :success)
                     (null (task-job-publication-claimed-p job))
                     (task-job-retained-p job)
                     (zerop (task-orchestrator-live-count orchestrator))
                     (listp artifact)
                     (= (getf artifact :portable-integer) 42))
                "publication moves atomically from a public live snapshot to terminal state")
               (test-assert
                (and (null (task-job-definition job))
                     summary
                     (equal summary result-summary)
                     (null (member :instructions summary :test #'eq))
                     (null
                      (search
                       secret
                       (task--write-readable-sexp
                        (list summary result-summary)))))
                "terminal jobs discard full role definitions and instruction text")))
           (dolist (failure '(:error :abort))
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "publication-~A" failure)))
                    (barrier
                      (make-instance
                       'task-test-publication-barrier
                       :failure failure))
                    (result
                      (task-tests--terminal-result
                       job
                       :status :success
                       :output (make-string 3000 :initial-element #\O)))
                    (publication-result nil)
                    (publication-condition nil))
               (setf (getf result :publication-barrier) barrier
                     (getf result :structured-output-present-p) t
                     (getf result :structured-output)
                     (make-string 3000 :initial-element #\S))
               (let ((thread
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf publication-result
                                    (task-job--publish-terminal
                                     job :completed result))
                            (condition (condition)
                              (setf publication-condition condition))))
                        :name
                        (format nil "Autolith publication ~A" failure))))
                 (unwind-protect
                      (test-assert
                       (task-tests--wait-until
                        (lambda ()
                          (with-lock-held
                              ((task-test-publication-barrier-lock barrier))
                            (task-test-publication-barrier-reached-p
                             barrier)))
                        2)
                       "failing publication reaches the post-claim barrier")
                   (task-tests--release-publication-barrier barrier))
                 (join-thread thread))
               (let* ((snapshot (task-job-snapshot job))
                      (terminal-result (getf snapshot :result))
                      (output-path (getf terminal-result :output-path)))
                 (test-assert
                  (and publication-result
                       (null publication-condition)
                       (eq (getf snapshot :state) :failed)
                       (eq (getf terminal-result :status) :failed)
                       (task-job-retained-p job)
                       (null (task-job-publication-claimed-p job))
                       (null (task-job-definition job))
                       (not
                        (and (null output-path)
                             (or
                              (eq (getf terminal-result :output-storage)
                                  :artifact)
                              (eq
                               (getf terminal-result
                                     :structured-output-storage)
                               :artifact)
                              (eq (getf terminal-result :error-storage)
                                  :artifact))))
                       (zerop
                        (task-orchestrator-live-count orchestrator)))
                  (format nil
                          "post-claim ~A forces one coherent terminal failure"
                          failure))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-terminal-wakeup-ordering () null)
(defun test-task-terminal-wakeup-ordering ()
  "Test that terminal waiters wake before arbitrary lifecycle listeners run."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "wakeup-order"
            :description "Exercise terminal wakeup ordering."
            :instructions "Publish one result."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "wakeup-primary"))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "wakeup-job"))
         (result
           (task-tests--terminal-result
            job :status :success :output "wake the waiter"))
         (ready-lock (make-lock "Autolith waiter readiness"))
         (ready-condition (make-condition-variable))
         (waiter-ready-p nil)
         (waiter-returned-p nil)
         (listener-lock (make-lock "Autolith blocking lifecycle listener"))
         (listener-condition (make-condition-variable))
         (listener-reached-p nil)
         (listener-released-p nil)
         (waiter nil)
         (publisher nil))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (declare (ignore payload))
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (listener-lock)
           (setf listener-reached-p t)
           (task--condition-broadcast listener-condition)
           (loop until listener-released-p
                 do (condition-wait listener-condition listener-lock))))))
    (unwind-protect
         (progn
           (setf waiter
                 (make-thread
                  (lambda ()
                    (with-lock-held ((task-job-lock job))
                      (with-lock-held (ready-lock)
                        (setf waiter-ready-p t)
                        (task--condition-broadcast ready-condition))
                      (loop until
                            (task-job--terminal-state-p
                             (task-job-state job))
                            do (condition-wait
                                (task-job-condition-variable job)
                                (task-job-lock job))))
                    (setf waiter-returned-p t))
                  :name "Autolith terminal waiter"))
           (test-assert
            (task-tests--wait-until (lambda () waiter-ready-p) 2)
            "the terminal waiter is parked before publication")
           (setf publisher
                 (make-thread
                  (lambda ()
                    (task-job--publish-terminal job :completed result))
                  :name "Autolith listener-blocked publisher"))
           (test-assert
            (task-tests--wait-until (lambda () listener-reached-p) 2)
            "terminal publication reaches the blocking lifecycle listener")
           (let ((woke-before-listener-release-p
                   (task-tests--wait-until
                    (lambda () waiter-returned-p)
                    0.5)))
             (with-lock-held (listener-lock)
               (setf listener-released-p t)
               (task--condition-broadcast listener-condition))
             (join-thread publisher)
             (setf publisher nil)
             (join-thread waiter)
             (setf waiter nil)
             (test-assert
              woke-before-listener-release-p
              "terminal publication wakes waiters before invoking listeners")))
      (with-lock-held (listener-lock)
        (setf listener-released-p t)
        (task--condition-broadcast listener-condition))
      (when publisher
        (join-thread publisher))
      (when waiter
        (with-lock-held ((task-job-lock job))
          (task--condition-broadcast (task-job-condition-variable job)))
        (join-thread waiter))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-job-visibility () null)
(defun test-task-job-visibility ()
  "Test conversation ownership, child ancestry, and opaque job lookup errors."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (missing-orchestrator (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "visibility"
            :description "Exercise task-tree visibility."
            :instructions "Inspect only descendant task jobs."
            :source :test))
         (primary-a
           (task-tests--primary-agent configuration "visibility-primary-a"))
         (primary-b
           (task-tests--primary-agent configuration "visibility-primary-b")))
    (unwind-protect
         (let* ((root-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "root-a"))
                (viewer-a
                  (task-tests--child-viewer configuration root-a))
                (descendant-a
                  (task-tests--register-job
                   orchestrator viewer-a definition :name "descendant-a"))
                (sibling-a
                  (task-tests--register-job
                   orchestrator primary-a definition :name "sibling-a"))
                (sibling-viewer
                  (task-tests--child-viewer configuration sibling-a))
                (sibling-descendant
                  (task-tests--register-job
                   orchestrator sibling-viewer definition
                   :name "sibling-descendant"))
                (foreign
                  (task-tests--register-job
                   orchestrator primary-b definition :name "foreign"))
                (root-a-id (getf (task-job-identity root-a) :id))
                (descendant-a-id
                  (getf (task-job-identity descendant-a) :id))
                (sibling-a-id (getf (task-job-identity sibling-a) :id))
                (sibling-descendant-id
                  (getf (task-job-identity sibling-descendant) :id))
                (foreign-id (getf (task-job-identity foreign) :id)))
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator primary-a))
             (list root-a-id descendant-a-id
                   sibling-a-id sibling-descendant-id))
            "a primary sees only jobs owned by its conversation")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator primary-b))
             (list foreign-id))
            "another primary cannot see the first conversation's task tree")
           (test-assert
            (equal
             (mapcar
              (lambda (job) (getf (task-job-identity job) :id))
              (task-orchestrator-list-visible-jobs orchestrator viewer-a))
             (list descendant-a-id))
            "a child sees descendants but not itself, its parent, or siblings")
           (dolist (operation '("get" "wait" "cancel"))
             (let ((invisible-report
                     (task-tests--job-tool-error-report
                      orchestrator primary-a
                      :operation operation
                      :identifier foreign-id))
                   (missing-report
                     (task-tests--job-tool-error-report
                      missing-orchestrator primary-a
                      :operation operation
                      :identifier foreign-id)))
               (test-assert
                (and (string= invisible-report missing-report)
                     (search "No visible task job" invisible-report))
                (format nil
                        "job.~A does not disclose whether an invisible identifier exists"
                        operation))))
           (test-assert
            (and (eq (task-job-state foreign) :queued)
                 (null (task-job-cancellation-reason foreign)))
            "invisible get, wait, and cancel attempts cannot mutate the job"))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-job-list-pagination () null)
(defun test-task-job-list-pagination ()
  "Test content-aware native pagination at the maximum job.list page size."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (agent-name
           (concatenate 'string "a" (make-string 63 :initial-element #\z)))
         (requested-name (make-string 64 :initial-element #\n))
         (definition
           (task-agent-definition-create
            :name agent-name
            :description "Exercise the largest job listing page."
            :instructions "Remain queued for pagination."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "job-list-primary"))
         (tool
           (make-instance
            'task-job-tool
            :orchestrator orchestrator
            :namespace "job"
            :name "list"
            :description "List test jobs."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation (agent-conversation primary)
                          :registry (agent-tool-registry primary)
                          :agent primary)))
    (unwind-protect
         (progn
           (dotimes (index *task-job-page-maximum*)
             (declare (ignore index))
             (task-tests--register-job
              orchestrator primary definition :name requested-name))
           (let* ((result
                    (tool-execute
                     tool context
                     (json-object
                      "offset" 0
                      "limit" *task-job-page-maximum*)))
                  (content (tool-result-content result))
                  (form (task-tests--read-exact-native-value content))
                  (properties (rest form)))
             (test-assert
              (and (eq (first form) :job-list)
                   (= (getf properties :count)
                      *task-job-page-maximum*)
                   (= (getf properties :total)
                      *task-job-page-maximum*)
                   (null (getf properties :next-offset))
                   (<= (length content) *task-tool-content-limit*))
              "a maximum-size job.list request returns a bounded native page"))
           (let ((offset 0)
                 (identifiers nil)
                 (page-count 0))
             (loop
               (let* ((result
                        (tool-execute
                         tool context
                         (json-object
                          "offset" offset
                          "limit" 17)))
                      (content (tool-result-content result))
                      (form
                        (task-tests--read-exact-native-value content))
                      (properties (rest form))
                      (jobs (getf properties :jobs))
                      (next-offset (getf properties :next-offset)))
                 (incf page-count)
                 (test-assert
                  (and (eq (first form) :job-list)
                       (= (getf properties :offset) offset)
                       (= (getf properties :count) (length jobs))
                       (= (getf properties :total)
                          *task-job-page-maximum*)
                       (plusp (length jobs))
                       (<= (length content) *task-tool-content-limit*))
                  "each maximum-size job.list request returns one bounded native page")
                 (setf identifiers
                       (nconc identifiers
                              (mapcar (lambda (job) (getf job :id)) jobs)))
                 (if next-offset
                     (setf offset next-offset)
                     (return))))
             (test-assert
              (and (> page-count 1)
                   (= (length identifiers) *task-job-page-maximum*)
                   (= (length
                       (remove-duplicates identifiers :test #'string=))
                      *task-job-page-maximum*))
              "job.list pagination returns every oversized summary exactly once")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-refresh-after-delayed-close () null)
(defun test-task-refresh-after-delayed-close ()
  "Test reopening after a timed-out close's final worker exits later."
  (let* ((orchestrator (task-orchestrator-create))
         (barrier-lock (make-lock "Autolith delayed close test"))
         (barrier (make-condition-variable))
         (started-p nil)
         (released-p nil)
         (thread
           (make-thread
            (lambda ()
              (with-lock-held (barrier-lock)
                (setf started-p t)
                (task--condition-broadcast barrier)
                (loop until released-p
                      do (condition-wait barrier barrier-lock))))
            :name "Autolith delayed closing worker")))
    (unwind-protect
         (progn
           (test-assert
            (task-tests--wait-until (lambda () started-p) 2)
            "the delayed closing worker reaches its barrier")
           (with-lock-held ((task-orchestrator-lock orchestrator))
             (setf (task-orchestrator-worker-threads orchestrator)
                   (list thread)
                   (task-orchestrator-lifecycle-state orchestrator) :closing
                   (task-orchestrator-close-owner orchestrator) nil
                   (task-orchestrator-shutdown-p orchestrator) t
                   (task-orchestrator-active-count orchestrator) 1))
           (test-assert
            (handler-case
                (progn
                  (task-orchestrator-refresh orchestrator)
                  nil)
              (task-error ()
                t))
            "refresh refuses a timed-out close while its delayed worker is live")
           (with-lock-held (barrier-lock)
             (setf released-p t)
             (task--condition-broadcast barrier))
           (join-thread thread)
           (task-orchestrator-refresh orchestrator)
           (test-assert
            (with-lock-held ((task-orchestrator-lock orchestrator))
              (and (eq (task-orchestrator-lifecycle-state orchestrator)
                       :open)
                   (null (task-orchestrator-close-owner orchestrator))
                   (not (task-orchestrator-shutdown-p orchestrator))
                   (zerop (task-orchestrator-active-count orchestrator))
                   (plusp
                    (length
                     (task-orchestrator-worker-threads orchestrator)))
                   (every
                    #'thread-alive-p
                    (task-orchestrator-worker-threads orchestrator))))
            "refresh reaps the delayed death and reopens a fresh worker pool"))
      (with-lock-held (barrier-lock)
        (setf released-p t)
        (task--condition-broadcast barrier))
      (when (thread-alive-p thread)
        (join-thread thread))
      (ignore-errors (task-orchestrator-close orchestrator))))
  nil)

(-> test-task-terminal-cancellation-and-publication () null)
(defun test-task-terminal-cancellation-and-publication ()
  "Test terminal-parent cascades and exactly-once terminal publication."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "lifecycle"
            :description "Exercise terminal lifecycle invariants."
            :instructions "Publish one terminal result."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "lifecycle-primary"))
         (events nil)
         (event-lock (make-lock "Autolith task lifecycle event test")))
    (task-orchestrator-add-listener
     orchestrator
     (lambda (channel payload)
       (when (eq channel :task-subagent-lifecycle)
         (with-lock-held (event-lock)
           (push (copy-tree payload) events)))))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name "terminal-parent"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status :success :output "parent complete"))
                (parent-id (getf (task-job-identity parent) :id))
                (viewer
                  (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (descendant-id
                  (getf (task-job-identity descendant) :id))
                (unrelated
                  (task-tests--register-job
                   orchestrator primary definition :name "unrelated")))
           (test-assert
            (task-job--publish-terminal parent :completed parent-result)
            "the parent publishes its first terminal result")
           (multiple-value-bind (parent-accepted-p descendants)
               (task-job-cancel parent :terminate)
             (test-assert
              (and (null parent-accepted-p)
                   (equal descendants (list descendant-id)))
              "cancelling a terminal parent still cancels its live descendants"))
           (test-assert
            (and (eq (task-job-state parent) :completed)
                 (eq (task-job-state descendant) :aborted)
                 (eq (task-job-state unrelated) :queued)
                 (null (task-job-cancellation-reason unrelated)))
            "terminal-parent cancellation preserves parent and unrelated states")
           (let ((event-count (length events))
                 (terminal-count
                   (length (task-orchestrator-terminal-identifiers
                            orchestrator))))
             (multiple-value-bind (parent-accepted-p descendants)
                 (task-job-cancel parent :terminate)
               (test-assert
                (and (null parent-accepted-p)
                     (null descendants)
                     (= (length events) event-count)
                     (= (length
                         (task-orchestrator-terminal-identifiers orchestrator))
                        terminal-count))
                "duplicate cancellation publishes no second result or event")))
           (dolist (job (list parent descendant))
             (let* ((result (task-job-result job))
                    (pathname (getf result :output-path))
                    (artifact
                      (and pathname
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname)))))
               (test-assert
                (and pathname
                     (probe-file pathname)
                     (listp artifact)
                     (eq (getf artifact :status)
                         (getf result :status)))
                "each cancellation result artifact is exactly one readable s-expression")))
           (let* ((race-job
                    (task-tests--register-job
                     orchestrator primary definition :name "publication-race"))
                  (result
                    (task-tests--terminal-result
                     race-job :status :success :output "race winner"))
                  (barrier-lock
                    (make-lock "Autolith terminal publication barrier"))
                  (barrier (make-condition-variable))
                  (ready 0)
                  (released-p nil)
                  (claims nil)
                  (claim-lock
                    (make-lock "Autolith terminal publication claims")))
             (labels ((publish ()
                        (with-lock-held (barrier-lock)
                          (incf ready)
                          (task--condition-broadcast barrier)
                          (loop until released-p
                                do (condition-wait barrier barrier-lock)))
                        (let ((claimed-p
                                (task-job--publish-terminal
                                 race-job :completed result)))
                          (with-lock-held (claim-lock)
                            (push claimed-p claims)))))
               (let ((first-thread
                       (make-thread #'publish
                                    :name "Autolith publication race one"))
                     (second-thread
                       (make-thread #'publish
                                    :name "Autolith publication race two")))
                 (with-lock-held (barrier-lock)
                   (loop until (= ready 2)
                         do (condition-wait barrier barrier-lock))
                   (setf released-p t)
                   (task--condition-broadcast barrier))
                 (join-thread first-thread)
                 (join-thread second-thread)))
             (let* ((race-id (getf (task-job-identity race-job) :id))
                    (terminal-occurrences
                      (count race-id
                             (task-orchestrator-terminal-identifiers
                              orchestrator)
                             :test #'string=))
                    (event-occurrences
                      (count race-id events
                             :test #'string=
                             :key (lambda (event) (getf event :id)))))
               (test-assert
                (and (= (count t claims) 1)
                     (= (count nil claims) 1)
                     (= terminal-occurrences 1)
                     (= event-occurrences 1)
                     (probe-file
                      (getf (task-job-result race-job) :output-path)))
                "concurrent duplicate publication claims one artifact and event"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-retention-and-admission () null)
(defun test-task-retention-and-admission ()
  "Test exact terminal retention and atomic live-job admission limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "capacity"
            :description "Exercise scheduler capacity."
            :instructions "Remain inert until the test changes state."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "capacity-primary")))
    (unwind-protect
         (progn
           (let* ((orchestrator (task-orchestrator-create))
                  (live
                    (task-tests--register-job
                     orchestrator primary definition :name "live-sentinel"))
                  (identifiers nil))
             (dotimes (index (1+ *task-terminal-retention-limit*))
               (let* ((job
                        (task-tests--register-job
                         orchestrator primary definition
                         :name (format nil "retained-~2,'0D" index)))
                      (identifier (getf (task-job-identity job) :id))
                      (result
                        (task-tests--terminal-result
                         job
                         :status :success
                         :output (format nil "terminal result ~D" index))))
                 (push identifier identifiers)
                 (test-assert
                  (task-job--publish-terminal job :completed result)
                  "each retained job publishes exactly once")))
             (setf identifiers (nreverse identifiers))
             (let ((first-id (first identifiers))
                   (last-id (car (last identifiers)))
                   (live-id (getf (task-job-identity live) :id)))
               (test-assert
                (and (= (length
                         (task-orchestrator-terminal-identifiers orchestrator))
                        *task-terminal-retention-limit*)
                     (= (hash-table-count
                         (task-orchestrator-jobs orchestrator))
                        (1+ *task-terminal-retention-limit*))
                     (= (hash-table-count
                         (task-orchestrator-names orchestrator))
                        (1+ *task-terminal-retention-limit*))
                     (= (task-orchestrator-live-count orchestrator) 1)
                     (null
                      (gethash first-id
                               (task-orchestrator-jobs orchestrator)))
                     (null
                      (gethash first-id
                               (task-orchestrator-names orchestrator)))
                     (gethash last-id
                              (task-orchestrator-jobs orchestrator))
                     (eq (gethash live-id
                                  (task-orchestrator-jobs orchestrator))
                         live))
                "retention keeps exactly 64 newest terminals without evicting live jobs")
               (test-assert
                (every
                 (lambda (identifier)
                   (let* ((job
                            (gethash identifier
                                     (task-orchestrator-jobs orchestrator)))
                          (pathname
                            (and job
                                 (getf (task-job-result job) :output-path))))
                     (and job
                          (task-job-terminal-p job)
                          pathname
                          (listp
                           (task-tests--read-exact-native-value
                            (uiop:read-file-string pathname))))))
                 (task-orchestrator-terminal-identifiers orchestrator))
                "every retained identifier maps to one terminal job and readable artifact")))
           (let* ((orchestrator (task-orchestrator-create))
                  (entries
                    (lambda (offset count)
                      (loop for index from offset below (+ offset count)
                            collect
                            (list
                             :definition definition
                             :item
                             (list :name (format nil "live-~2,'0D" index)
                                   :agent "capacity"
                                   :task "Remain queued."
                                   :context nil
                                   :async t)
                             :detached t)))))
             (dotimes (batch 4)
               (task-orchestrator-start-jobs
                orchestrator primary
                (funcall entries (* batch 16) 16)
                nil nil))
             (let ((next-index (task-orchestrator-next-index orchestrator))
                   (job-count
                     (hash-table-count
                      (task-orchestrator-jobs orchestrator)))
                   (name-count
                     (hash-table-count
                      (task-orchestrator-names orchestrator)))
                   (queue-count (length (task-orchestrator-queue orchestrator)))
                   (live-count (task-orchestrator-live-count orchestrator)))
               (test-assert
                (= live-count *task-maximum-live-jobs*)
                "the scheduler admits exactly 64 simultaneous live jobs")
               (test-assert
                (handler-case
                    (progn
                      (task-orchestrator-start-jobs
                       orchestrator primary
                       (funcall entries 64 1)
                       nil nil)
                      nil)
                  (task-error ()
                    t))
                "admission beyond 64 live jobs fails")
               (test-assert
                (and (= (task-orchestrator-next-index orchestrator)
                        next-index)
                     (= (hash-table-count
                         (task-orchestrator-jobs orchestrator))
                        job-count)
                     (= (hash-table-count
                         (task-orchestrator-names orchestrator))
                        name-count)
                     (= (length (task-orchestrator-queue orchestrator))
                        queue-count)
                     (= (task-orchestrator-live-count orchestrator)
                        live-count))
                "failed live admission consumes no identity or scheduler state"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-evicted-identity-retention () null)
(defun test-task-evicted-identity-retention ()
  "Test that evicted generated identities remain unique across live ancestry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (orchestrator  (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "identity-retention"
            :description "Exercise retained task ancestry."
            :instructions "Keep descendant ownership unambiguous."
            :source :test))
         (primary
           (task-tests--primary-agent configuration "identity-primary")))
    (unwind-protect
         (let* ((parent
                  (task-tests--register-job
                   orchestrator primary definition :name nil))
                (parent-id (getf (task-job-identity parent) :id))
                (viewer (task-tests--child-viewer configuration parent))
                (descendant
                  (task-tests--register-job
                   orchestrator viewer definition :name "live-descendant"))
                (parent-result
                  (task-tests--terminal-result
                   parent :status :success :output "parent terminal")))
           (task-job--publish-terminal parent :completed parent-result)
           (dotimes (index *task-terminal-retention-limit*)
             (let* ((job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name (format nil "identity-filler-~D" index)))
                    (result
                      (task-tests--terminal-result
                       job :status :success :output "filler terminal")))
               (task-job--publish-terminal job :completed result)))
           (let* ((replacement
                    (task-tests--register-job
                     orchestrator primary definition :name nil))
                  (replacement-id
                    (getf (task-job-identity replacement) :id)))
             (test-assert
              (and (null
                    (gethash parent-id
                             (task-orchestrator-jobs orchestrator)))
                   (null
                    (gethash parent-id
                             (task-orchestrator-names orchestrator)))
                   (member parent-id
                           (task-job-owner-identifiers descendant)
                           :test #'string=)
                   (not (string= replacement-id parent-id))
                   (> (getf (task-job-identity replacement) :index)
                      (getf (task-job-identity parent) :index))
                   (= (task-orchestrator-live-count orchestrator) 2))
              "eviction never permits a generated ancestor identity to be reused")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-live-activity-snapshots () null)
(defun test-task-live-activity-snapshots ()
  "Test stable lightweight projection of queued and running task jobs."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (orchestrator (task-orchestrator-create))
         (definition
           (task-agent-definition-create
            :name "activity"
            :description "Exercise live task presentation."
            :instructions "Remain observable while the test inspects you."
            :source ':test))
         (primary
           (task-tests--primary-agent configuration "activity-primary"))
         (queued nil)
         (running nil))
    (unwind-protect
         (progn
           (setf queued
                 (task-tests--register-job
                  orchestrator primary definition :name "queued-child")
                 running
                 (task-tests--register-job
                  orchestrator primary definition :name "running-child"))
           (with-lock-held ((task-job-lock running))
             (setf (task-job-state running) ':running)
             (task-job--set-progress-state running ':running))
           (task-progress-note-status
            running ':tool-call-started
            (list :tool "search.content"))
           (let ((activities
                   (task-orchestrator-live-activities orchestrator)))
             (test-assert
              (and (= (length activities) 2)
                   (equal
                    (mapcar (lambda (activity)
                              (getf activity :id))
                            activities)
                    (mapcar (lambda (job)
                              (getf (task-job-identity job) :id))
                            (list queued running)))
                   (eq (getf (first activities) :state) ':queued)
                   (eq (getf (second activities) :state) ':running)
                   (string= (getf (second activities) :current-tool)
                            "search.content")
                   (string= (getf (second activities) :agent) "activity")
                   (getf (second activities) :detached))
              "live activity snapshots are ordered, bounded task summaries"))
           (task-job--publish-terminal
            queued
            ':completed
            (task-tests--terminal-result
             queued :status ':success :output "queued complete"))
           (test-assert
            (equal (mapcar (lambda (activity)
                             (getf activity :id))
                           (task-orchestrator-live-activities orchestrator))
                   (list (getf (task-job-identity running) :id)))
            "terminal publication immediately removes a child snapshot")
           (task-job--publish-terminal
            running
            ':completed
            (task-tests--terminal-result
             running :status ':success :output "running complete"))
           (test-assert
            (null (task-orchestrator-live-activities orchestrator))
            "no task presentation remains after every child is terminal"))
      (dolist (job (remove nil (list queued running)))
        (unless (task-job-terminal-p job)
          (task-job--publish-terminal
           job
           ':aborted
           (task-tests--terminal-result
            job :status ':aborted :output "test cleanup"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> task-tests--run-scheduler-case (task-test-provider json-object) list)
(defun task-tests--run-scheduler-case (provider arguments)
  "Execute one real task tool case and return observations after clean shutdown."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (task-augment-tool-registry
                    (make-default-tool-registry)))
         (conversation (conversation-create configuration))
         (parent
          (agent-create :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry registry
                        :worker nil))
         (tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator tool))
         (context
          (make-instance 'tool-context
                         :configuration configuration
                         :worker nil
                         :conversation conversation
                         :registry registry
                         :agent parent
                         :call-id "task-scheduler-test"))
         (started-at (get-internal-real-time)))
    (unwind-protect
         (let* ((result (tool-execute tool context arguments))
                (idle-p
                  (task-tests--wait-until
                   (lambda ()
                     (with-lock-held ((task-orchestrator-lock orchestrator))
                       (zerop
                        (task-orchestrator-active-count orchestrator))))
                   1))
                (jobs (task-orchestrator-list-jobs orchestrator))
                (details (tool-result-details result))
                (content (tool-result-content result))
                (native-content
                  (task-tests--read-exact-native-value content))
                (artifact-forms
                  (mapcar
                   (lambda (job)
                     (task-tests--read-exact-native-value
                      (uiop:read-file-string
                       (getf (task-job-result job) :output-path))))
                   jobs))
                (workers
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (copy-list
                    (task-orchestrator-worker-threads orchestrator)))))
           (list :success-p (tool-result-success-p result)
                 :content content
                 :native-content native-content
                 :details details
                 :artifact-forms artifact-forms
                 :job-count (length jobs)
                 :all-terminal-p (every #'task-job-terminal-p jobs)
                 :heavy-references-cleared-p
                 (every (lambda (job)
                          (and (null (task-job-parent-agent job))
                               (null
                                (task-job-command-authorization-function job))
                               (null (task-job-thread job))))
                        jobs)
                 :worker-count (length workers)
                 :workers-alive-p (every #'thread-alive-p workers)
                 :provider-worker-count
                 (length (task-test-provider-threads provider))
                 :provider-maximum-active
                 (task-test-provider-maximum-active-count provider)
                 :provider-conversation-identifiers
                 (with-lock-held ((task-test-provider-lock provider))
                   (copy-list
                    (task-test-provider-conversation-identifiers provider)))
                 :execution-identifiers
                 (mapcar #'task-job-execution-identifier jobs)
                 :provider-request-count
                 (task-test-provider-request-count provider)
                 :scheduler-idle-p idle-p
                 :active-count
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (task-orchestrator-active-count orchestrator))
                 :live-count
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (task-orchestrator-live-count orchestrator))
                 :artifacts-exist-p
                 (every (lambda (job)
                          (let ((path (getf (task-job-result job)
                                           :output-path)))
                            (and path (probe-file path))))
                        jobs)
                 :private-transcripts-p
                 (every (lambda (job)
                          (let ((path (getf (task-job-result job)
                                           :conversation-file)))
                            (and path (search "/tasks/" path))))
                        jobs)
                 :public-conversation-count
                 (length (conversation-list configuration))
                 :duration-ms
                 (task--milliseconds-between started-at
                                             (get-internal-real-time))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore))))

(-> test-task-run-native-manifest () null)
(defun test-task-run-native-manifest ()
  "Test fair bounded native manifests for the largest synchronous batch."
  (let ((previous-concurrency
          (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "5000" 1)
           (let* ((provider
                    (make-instance 'task-test-provider :mode :manifest))
                  (tasks
                    (coerce
                     (loop for index from 1 to *task-maximum-batch-size*
                           collect
                           (json-object
                            "name" (format nil "manifest-~2,'0D" index)
                            "agent" "task"
                            "task"
                            (format nil
                                    "Return manifest result ~D."
                                    index)))
                     'vector))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "context" "Exercise fair native aggregation."
                      "tasks" tasks)))
                  (content (getf observation :content))
                  (form (getf observation :native-content))
                  (results (getf (rest form) :results))
                  (artifacts (getf observation :artifact-forms))
                  (first-result (first results))
                  (last-result (car (last results)))
                  (last-result-value (getf last-result :result)))
             (test-assert
              (and (not (getf observation :success-p))
                   (equal form (getf observation :details))
                   (eq (first form) :task-run)
                   (null (getf (rest form) :succeeded-p))
                   (<= (length content) *task-tool-content-limit*))
              "task.run returns one exact bounded native manifest on partial failure")
             (test-assert
              (and (= (length results) *task-maximum-batch-size*)
                   (= (length artifacts) *task-maximum-batch-size*)
                   (= (length
                       (remove-duplicates
                        (mapcar (lambda (result) (getf result :id)) results)
                        :test #'string=))
                      *task-maximum-batch-size*)
                   (every
                    (lambda (result)
                      (let ((artifact (getf (getf result :result) :artifact)))
                        (and (non-empty-string-p (getf result :id))
                             (getf result :execution-id)
                             (member (getf result :state)
                                     '(:completed :failed :aborted)
                                     :test #'eq)
                             (stringp (getf artifact :path))
                             (eq (getf artifact :format) :sexp)
                             (getf artifact :available-p))))
                    results))
              "the manifest retains every child identity, state, and artifact descriptor")
             (test-assert
              (and
               (equal (getf first-result :id) "manifest-01-1")
               (equal (getf last-result :id) "manifest-16-16")
               (eq (getf first-result :state) :completed)
               (eq (getf last-result :state) :failed)
               (eq (getf last-result-value :status) :failed)
               (string=
                (getf last-result-value :error)
                "AUTOLITH-LAST-MANIFEST-CHILD-FAILED"))
              "a huge first result cannot hide the final failed child")
             (test-assert
              (and (> (length (getf (first artifacts) :output)) 90000)
                   (string=
                    (getf (car (last artifacts)) :error)
                    "AUTOLITH-LAST-MANIFEST-CHILD-FAILED")
                   (every #'listp artifacts))
              "every child artifact remains exactly one readable native result")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS"
                          previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)

(-> test-task-scheduler () null)
(defun test-task-scheduler ()
  "Test bounded reusable workers, private artifacts, and nested help-join."
  (let ((previous-concurrency (uiop:getenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
        (previous-runtime (uiop:getenv "AUTOLITH_TASK_MAX_RUNTIME_MS")))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "2" 1)
           (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" "1000" 1)
           (let* ((provider (make-instance 'task-test-provider
                                           :mode :concurrent))
                  (tasks
                   (coerce
                    (loop for index from 1 to 4
                          collect (json-object
                                   "agent" "task"
                                   "task" (format nil "Return result ~D." index)))
                    'vector))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "context" "Independent scheduler checks."
                                 "tasks" tasks))))
             (test-assert (getf observation :success-p)
                          "a concurrent task batch succeeds")
             (test-assert (= (getf observation :job-count) 4)
                          "the scheduler retains every admitted job")
             (test-assert (getf observation :all-terminal-p)
                          "synchronous scheduler jobs are terminal on return")
             (test-assert (getf observation :heavy-references-cleared-p)
                          "terminal jobs release live agent capabilities")
             (test-assert (= (getf observation :worker-count) 2)
                          "the configured pool contains only two workers")
             (test-assert (getf observation :workers-alive-p)
                          "reusable workers remain live until registry shutdown")
             (test-assert (= (getf observation :provider-worker-count) 2)
                          "four children reuse the bounded worker pair")
             (test-assert (= (getf observation :provider-maximum-active) 2)
                          "the pool executes up to its configured concurrency")
             (let ((identifiers
                     (getf observation :provider-conversation-identifiers))
                   (execution-identifiers
                     (getf observation :execution-identifiers)))
               (test-assert
                (and (= (length identifiers) 4)
                     (= (length (remove-duplicates identifiers :test #'string=))
                        4)
                     (equal (sort (copy-list identifiers) #'string<)
                            (sort (copy-list execution-identifiers) #'string<)))
                "parallel children use their unique execution identifiers as provider threads"))
             (test-assert (getf observation :artifacts-exist-p)
                          "every terminal child publishes one unique artifact")
             (test-assert (getf observation :private-transcripts-p)
                          "child transcripts live in the private task tree")
             (test-assert (zerop (getf observation
                                       :public-conversation-count))
                          "private child transcripts stay out of conversation lists"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "1" 1)
           (let* ((provider (make-instance 'task-test-provider :mode :nested))
                  (observation
                   (task-tests--run-scheduler-case
                    provider
                    (json-object "agent" "task"
                                 "task" "Delegate once, then return."))))
             (test-assert (getf observation :success-p)
                          "a nested synchronous task succeeds at concurrency one")
             (test-assert (= (getf observation :job-count) 2)
                          "nested execution retains parent and leaf jobs")
             (test-assert (= (getf observation :provider-request-count) 3)
                          "nested help-join resumes the parent after the leaf")
             (test-assert (= (getf observation :provider-worker-count) 1)
                          "nested synchronous work reuses its parent's worker")
             (test-assert (< (getf observation :duration-ms) 1000)
                          "nested help-join avoids a concurrency-one deadlock"))
           (let* ((provider
                    (make-instance 'task-test-provider :mode :async-wait))
                  (observation
                    (task-tests--run-scheduler-case
                     provider
                     (json-object
                      "agent" "task"
                      "task"
                      "Spawn one detached task, wait for it, then return.")))
                  (artifacts (getf observation :artifact-forms)))
             (test-assert
              (and (getf observation :success-p)
                   (= (getf observation :job-count) 2)
                   (getf observation :all-terminal-p)
                   (= (getf observation :provider-request-count) 4)
                   (= (getf observation :provider-worker-count) 1)
                   (getf observation :scheduler-idle-p)
                   (zerop (getf observation :active-count))
                   (zerop (getf observation :live-count))
                   (every (lambda (artifact)
                            (eq (getf artifact :status) :success))
                          artifacts)
                   (< (getf observation :duration-ms) 1000))
              "a child can await detached work at concurrency one without starvation"))
           (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY" "999" 1)
           (let ((orchestrator (task-orchestrator-create)))
             (test-assert
              (= (task-orchestrator-maximum-concurrency orchestrator)
                 *task-maximum-concurrency*)
              "environment concurrency cannot exceed the hard pool cap")))
      (if previous-concurrency
          (sb-posix:setenv "AUTOLITH_TASK_MAX_CONCURRENCY"
                          previous-concurrency 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_CONCURRENCY"))
      (if previous-runtime
          (sb-posix:setenv "AUTOLITH_TASK_MAX_RUNTIME_MS" previous-runtime 1)
          (sb-posix:unsetenv "AUTOLITH_TASK_MAX_RUNTIME_MS"))))
  nil)
