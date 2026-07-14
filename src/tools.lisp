(in-package #:autolith)

;;;; -- Tool Metadata --

(defclass tool ()
  ((namespace
    :initarg :namespace
    :reader tool-namespace
    :type non-empty-string
    :documentation "The Responses namespace containing this tool.")
   (name
    :initarg :name
    :reader tool-name
    :type non-empty-string
    :documentation "The function name inside the namespace.")
   (description
    :initarg :description
    :reader tool-description
    :type non-empty-string
    :documentation "The model-visible operation documentation.")
   (parameters
    :initarg :parameters
    :reader tool-parameters
    :type json-object
    :documentation "The JSON Schema accepted by this tool."))
  (:documentation "A documented, model-visible operation."))

(defclass lisp-tool (tool)
  ()
  (:documentation "A tool whose operation is isolated in a named Lisp worker."))

(defclass self-tool (tool)
  ()
  (:documentation "A tool whose operation targets the active Autolith image."))

(defclass mutable-self-tool (self-tool)
  ()
  (:documentation "A self tool omitted when Autolith runs in immutable mode."))

(defclass lisp-eval-tool (lisp-tool)
  ()
  (:documentation "Evaluate one Common Lisp form in the worker."))

(defclass lisp-compile-tool (lisp-tool)
  ()
  (:documentation "Compile and execute one Common Lisp form in the worker."))

(defclass lisp-load-system-tool (lisp-tool)
  ()
  (:documentation "Load one ASDF or Quicklisp system in the worker."))

(defclass lisp-describe-tool (lisp-tool)
  ()
  (:documentation "Describe one Lisp object or symbol in the worker."))

(defclass lisp-source-tool (lisp-tool)
  ()
  (:documentation "Read exact matching source for one worker definition."))

(defclass lisp-run-tests-tool (lisp-tool)
  ()
  (:documentation "Run an ASDF system's tests in the worker."))

(defclass lisp-reset-tool (lisp-tool)
  ()
  (:documentation "Reset one named Lisp REPL from a selected image."))

(defclass lisp-start-tool (lisp-tool)
  ()
  (:documentation "Start one named Lisp REPL from a selected image."))

(defclass lisp-stop-tool (lisp-tool)
  ()
  (:documentation "Stop and forget one named Lisp REPL."))

(defclass lisp-repls-tool (lisp-tool)
  ()
  (:documentation "List the active named Lisp REPLs."))

(defclass lisp-images-tool (lisp-tool)
  ()
  (:documentation "List pristine and saved Lisp worker images with notes."))

(defclass lisp-save-image-tool (lisp-tool)
  ()
  (:documentation "Save one named REPL as an immutable worker image."))

(defclass self-inspect-tool (self-tool)
  ()
  (:documentation "Inspect a documented symbol in the active image."))

(defclass self-source-tool (self-tool)
  ()
  (:documentation "Read tracked top-level definitions for one active symbol."))

(defclass self-eval-tool (mutable-self-tool)
  ()
  (:documentation "Evaluate one exploratory form in the active image."))

(defclass self-redefine-tool (mutable-self-tool)
  ()
  (:documentation "Compile and install one exploratory top-level definition."))

(defclass self-set-tool (mutable-self-tool)
  ()
  (:documentation "Set one active global binding to an evaluated value."))

(defclass self-persist-definition-tool (mutable-self-tool)
  ()
  (:documentation "Install and privately commit one complete definition."))

(defclass self-status-tool (self-tool)
  ()
  (:documentation "Summarize active-image mutation and recovery state."))

(defclass self-discard-tool (mutable-self-tool)
  ()
  (:documentation "Restore and discard one effective exploratory mutation."))

(defclass self-exercise-tool (mutable-self-tool)
  ()
  (:documentation "Run and journal one focused live-mutation exercise."))

(defclass self-diff-tool (self-tool)
  ()
  (:documentation "Show uncommitted reconstructible active-image mutations."))

(defclass self-commit-tool (mutable-self-tool)
  ()
  (:documentation "Persist pending live mutations as a private image commit."))

(defclass self-checkpoint-tool (mutable-self-tool)
  ()
  (:documentation "Save the active image as a retained working generation."))

(defclass self-generations-tool (self-tool)
  ()
  (:documentation "List retained working generations and compatibility."))

(defclass self-rollback-tool (mutable-self-tool)
  ()
  (:documentation "Select a retained generation and request immediate rollback."))

(-> tool-canonical-name (tool) string)
(defun tool-canonical-name (tool)
  "Return TOOL's dotted human-readable name."
  (format nil "~A.~A" (tool-namespace tool) (tool-name tool)))

(-> tool-object-schema (json-object list) json-object)
(defun tool-object-schema (properties required)
  "Return a closed JSON object schema with PROPERTIES and REQUIRED names."
  (json-object
   "type" "object"
   "properties" properties
   "required" (coerce required 'vector)
   "additionalProperties" false))

(-> tool-string-property (string) json-object)
(defun tool-string-property (description)
  "Return a documented JSON string property schema."
  (json-object "type" "string" "description" description))

(-> tool-string-array-property (string) json-object)
(defun tool-string-array-property (description)
  "Return a documented array-of-strings property schema."
  (json-object
   "type" "array"
   "description" description
   "items" (json-object "type" "string")))

(-> tool-integer-property (string) json-object)
(defun tool-integer-property (description)
  "Return a documented JSON integer property schema."
  (json-object "type" "integer" "description" description))

(-> tool-boolean-property (string) json-object)
(defun tool-boolean-property (description)
  "Return a documented JSON boolean property schema."
  (json-object "type" "boolean" "description" description))

(-> tool-restart-property () json-object)
(defun tool-restart-property ()
  "Return the shared schema of the optional restart selection argument."
  (tool-string-property
   "A restart name to invoke when the operation signals a correctable condition, for example CONTINUE."))

(-> tool-restart-value-property () json-object)
(defun tool-restart-value-property ()
  "Return the shared schema of the optional restart value argument."
  (tool-string-property
   "A value form passed to a restart that consumes a value, such as use-value or store-value."))

(-> tool-namespace-description (string) string)
(defun tool-namespace-description (namespace)
  "Return the model-visible description of tool NAMESPACE."
  (cond
    ((string= namespace "fs")
     "Workspace file reading and listing.")
    ((string= namespace "search")
     "Fast indexed workspace path and content discovery through fff.")
    ((string= namespace "shell")
     "External commands run in the workspace.")
    ((string= namespace "memory")
     "Persistent facts, preferences, decisions, and guidance across conversations.")
    ((string= namespace "agenda")
     "Short persistent tasks and notes keyed by workspace directory.")
    ((string= namespace "lisp")
     "Operations in named, heap-isolated Common Lisp REPLs.")
    ((string= namespace "self")
     "Operations on the active Autolith Common Lisp image.")
    ((string= namespace "task")
     "In-process child-agent delegation with batching and detached jobs.")
    ((string= namespace "job")
     "Inspection, waiting, and cancellation for task jobs.")
    ((string= namespace "yield")
     "Required terminal result submission for child agents.")
    (t
     "Autolith operations.")))

(-> tool-provider-schema (tool) json-object)
(defun tool-provider-schema (tool)
  "Return TOOL in the Responses API namespaced function schema."
  (json-object
   "type" "function"
   "name" (tool-name tool)
   "description" (tool-description tool)
   "strict" false
   "parameters" (tool-parameters tool)))


;;;; -- Tool Context and Results --

(defclass tool-context ()
  ((configuration
    :initarg :configuration
    :reader tool-context-configuration
    :type configuration
    :documentation "The active process configuration.")
   (worker
    :initarg :worker
    :reader tool-context-worker
    :type t
    :documentation "The named Lisp worker manager.")
   (conversation
    :initarg :conversation
    :reader tool-context-conversation
    :type conversation
    :documentation "The conversation requesting the operation.")
   (mutation-checker
    :initarg :mutation-checker
    :initform nil
    :reader tool-context-mutation-checker
    :type t
    :documentation "The optional durable-mutation check strategy.")
   (registry
    :initarg :registry
    :initform nil
    :reader tool-context-registry
    :type t
    :documentation "The registry dispatching this execution, when available.")
   (command-authorization-function
    :initarg :command-authorization-function
    :initform nil
    :reader tool-context-command-authorization-function
    :type (option function)
    :documentation "The callback deciding whether and how shell commands may run.")
   (agent
    :initarg :agent
    :initform nil
    :reader tool-context-agent
    :type t
    :documentation "The agent whose provider call requested this tool.")
   (observer
    :initarg :observer
    :initform nil
    :reader tool-context-observer
    :type t
    :documentation "The observer presenting the parent tool lifecycle.")
   (call-id
    :initarg :call-id
    :initform nil
    :reader tool-context-call-id
    :type (option string)
    :documentation "The provider function call identifier for this execution."))
  (:documentation "The explicit capabilities supplied to one tool execution."))

(-> tool-context-authorize-command
    (tool-context string pathname)
    keyword)
(defun tool-context-authorize-command (context command directory)
  "Return :DENY, :SANDBOXED, or :FULL-ACCESS for COMMAND in DIRECTORY."
  (let* ((function (tool-context-command-authorization-function context))
         (decision (if function
                       (funcall function command directory)
                       ':deny)))
    (unless (member decision '(:deny :sandboxed :full-access))
      (error 'tool-error
             :message (format nil "Command authorization returned invalid decision ~S."
                              decision)
             :tool-name "shell.run"))
    decision))

(defclass tool-result ()
  ((content
    :initarg :content
    :reader tool-result-content
    :type string
    :documentation "The bounded model-visible result.")
   (image-attachments
    :initarg :image-attachments
    :initform nil
    :reader tool-result-image-attachments
    :type list
    :documentation
    "Provider-visible local images returned by a successful tool operation.")
   (success-p
    :initarg :success-p
    :reader tool-result-success-p
    :type boolean
    :documentation "True when the tool operation succeeded."))
  (:documentation "The model-visible outcome of exactly one tool call."))

(defgeneric tool-result-details (result)
  (:documentation "Return RESULT's machine-readable details, or NIL when plain.")
  (:method ((result tool-result))
    nil))

(-> tool-success (t &key (:image-attachments list)) tool-result)
(defun tool-success (content &key image-attachments)
  "Return a successful bounded tool result with CONTENT and optional images."
  (unless (every (lambda (attachment)
                   (typep attachment 'image-attachment))
                 image-attachments)
    (error 'tool-error
           :message "Tool image results must contain image attachments."
           :tool-name "unknown"))
  (make-instance 'tool-result
                 :content (bounded-string content)
                 :image-attachments (copy-list image-attachments)
                 :success-p t))

(-> tool-failure (t) tool-result)
(defun tool-failure (content)
  "Return a failed bounded tool result containing CONTENT."
  (make-instance 'tool-result
                 :content (bounded-string content)
                 :success-p nil))

(-> tool-execute (tool tool-context json-object) tool-result)
(defgeneric tool-execute (tool context arguments)
  (:documentation "Execute TOOL with validated JSON ARGUMENTS inside CONTEXT."))

(-> tool-argument (json-object string &key (:required boolean)) t)
(defun tool-argument (arguments name &key required)
  "Return NAME from ARGUMENTS, signaling TOOL-ERROR when REQUIRED and absent."
  (multiple-value-bind (value present-p)
      (gethash name arguments)
    (when (and required (not present-p))
      (error 'tool-error
             :message (format nil "Required tool argument ~S is missing." name)
             :tool-name "unknown"))
    value))


;;;; -- Registry and Dispatch --

(defclass tool-registry ()
  ((tools
    :initform nil
    :accessor tool-registry-tools
    :type list
    :documentation "Registered tools in presentation order.")
   (index
    :initform (make-hash-table :test #'equal)
    :reader tool-registry-index
    :type hash-table
    :documentation "Canonical dotted tool names mapped to tool objects."))
  (:documentation "The model-visible tools and their active dispatch objects."))

(-> tool-registry-register (tool-registry tool) tool)
(defun tool-registry-register (registry tool)
  "Register TOOL in REGISTRY, replacing an existing object with the same name."
  (let* ((canonical-name (tool-canonical-name tool))
         (existing (gethash canonical-name (tool-registry-index registry))))
    (when existing
      (setf (tool-registry-tools registry)
            (remove existing (tool-registry-tools registry))))
    (setf (gethash canonical-name (tool-registry-index registry)) tool
          (tool-registry-tools registry)
          (nconc (tool-registry-tools registry) (list tool)))
    tool))

(-> tool-registry-find (tool-registry string string) (option tool))
(defun tool-registry-find (registry namespace name)
  "Return the tool named NAMESPACE.NAME from REGISTRY, or NIL."
  (gethash (format nil "~A.~A" namespace name)
           (tool-registry-index registry)))

(-> tool-registry-provider-schemas (tool-registry) vector)
(defun tool-registry-provider-schemas (registry)
  "Return REGISTRY grouped into Responses API namespace schemas."
  (let ((namespace-order nil)
        (namespace-tools (make-hash-table :test #'equal)))
    (dolist (tool (tool-registry-tools registry))
      (unless (gethash (tool-namespace tool) namespace-tools)
        (setf (gethash (tool-namespace tool) namespace-tools) nil)
        (setf namespace-order
              (nconc namespace-order (list (tool-namespace tool)))))
      (setf (gethash (tool-namespace tool) namespace-tools)
            (nconc (gethash (tool-namespace tool) namespace-tools)
                   (list (tool-provider-schema tool)))))
    (coerce
     (mapcar
      (lambda (namespace)
        (json-object
         "type" "namespace"
         "name" namespace
         "description" (tool-namespace-description namespace)
         "tools" (coerce (gethash namespace namespace-tools) 'vector)))
      namespace-order)
     'vector)))

(-> function-call-canonical-name (json-object) string)
(defun function-call-canonical-name (call)
  "Return the dotted canonical name carried by function CALL."
  (format nil "~A.~A"
          (or (json-get call "namespace") "")
          (or (json-get call "name") "")))

(-> tool-registry-execute-call
    (tool-registry json-object tool-context)
    tool-result)
(defun tool-registry-execute-call (registry call context)
  "Validate and execute one Responses function CALL through REGISTRY."
  (let* ((namespace (json-get call "namespace"))
         (name (json-get call "name"))
         (canonical-name (function-call-canonical-name call)))
    (handler-case
        (progn
          (unless (and (non-empty-string-p namespace) (non-empty-string-p name))
            (error 'tool-error
                   :message "The provider returned a function call without namespace or name."
                   :tool-name canonical-name))
          (let ((tool (tool-registry-find registry namespace name)))
            (unless tool
              (error 'tool-error
                     :message (format nil "Unknown tool ~A." canonical-name)
                     :tool-name canonical-name))
            (let ((arguments (json-decode (or (json-get call "arguments") "{}"))))
              (unless (json-object-p arguments)
                (error 'tool-error
                       :message (format nil "Arguments for ~A are not a JSON object."
                                        canonical-name)
                       :tool-name canonical-name))
              (tool-execute tool context arguments))))
      (rollback-requested (condition)
        (error condition))
      (active-image-corruption (condition)
        (error condition))
      (error (condition)
        (tool-failure
         (format nil "~A failed: ~A" canonical-name condition))))))
