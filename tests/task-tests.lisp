(in-package #:autolith)

;;;; -- In-Process Task Orchestration Tests --

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, agent discovery, and yields."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((registry (task-augment-tool-registry
                            (make-default-tool-registry))))
             (test-assert (= (length (tool-registry-tools registry)) 51)
                          "task augmentation adds one task and four job tools")
             (dolist (name '("list" "get" "wait" "cancel"))
               (test-assert (tool-registry-find registry "job" name)
                            (format nil "task augmentation registers job.~A" name)))
             (test-assert (eq registry (task-augment-tool-registry registry))
                          "task augmentation is idempotent"))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Inspect the parser."
                                            "agent" "SCOUT"
                                            "async" t)))))
             (test-assert (string= (getf item :agent) "scout")
                          "task normalization canonicalizes agent names")
             (test-assert (getf item :async)
                          "task normalization preserves detached execution"))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "tasks"
                                (json-array
                                 (json-object "task" "First")
                                 (json-object "task" "Second"))))
                  nil)
              (task-error ()
                t))
            "batch task normalization requires shared context")
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.md" agent-directory))
                  (project-configuration
                    (configuration--clone configuration :working-directory root)))
             (ensure-directories-exist agent-path)
             (with-open-file (stream agent-path
                                     :direction :output
                                     :if-exists :supersede)
               (format stream "---~%name: scout~%description: Project scout~%---~%Project instructions."))
             (let ((definition
                     (task-find-agent-definition
                      (task-discover-agents project-configuration)
                      "scout")))
               (test-assert (eq (task-agent-definition-source definition) :project)
                            "project agents override bundled definitions")
               (test-assert (string= (task-agent-definition-system-prompt definition)
                                     "Project instructions.")
                            "agent discovery retains the Markdown body")))
           (let* ((definition
                    (task-agent-definition-create
                     :name "structured"
                     :description "Yield structured data."
                     :system-prompt ""
                     :output (json-object "type" "object"
                                          "required" (json-array "answer")
                                          "properties"
                                          (json-object "answer"
                                                       (json-object "type" "string")))
                     :source :test))
                  (completion (make-instance 'task-completion))
                  (orchestrator (task-orchestrator-create))
                  (parent (agent-create
                           :configuration configuration
                           :provider (make-instance 'model-provider)
                           :conversation (conversation-create configuration)
                           :tool-registry (make-instance 'tool-registry)
                           :worker nil))
                  (job (make-instance 'task-job
                                      :orchestrator orchestrator
                                      :identity (list :id "yield-test" :index 1)
                                      :definition definition
                                      :item (list :task "Yield")
                                      :parent-agent parent
                                      :detached-p nil))
                  (child (make-instance 'task-child-agent
                                        :configuration configuration
                                        :provider (make-instance 'model-provider)
                                        :conversation (conversation-create configuration)
                                        :tool-registry (make-instance 'tool-registry)
                                        :worker nil
                                        :definition definition
                                        :identity (task-job-identity job)
                                        :depth 1
                                        :completion completion
                                        :orchestrator orchestrator
                                        :job job))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker nil
                                          :conversation nil
                                          :agent child))
                  (tool (make-instance 'task-yield-tool
                                       :namespace "yield"
                                       :name "submit"
                                       :description ""
                                       :parameters (json-object))))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute tool context
                                  (json-object "status" "success"
                                               "data" (json-object "wrong" "shape")))
                    nil)
                (task-yield-error ()
                  t))
              "yield validation rejects data outside the output contract")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
