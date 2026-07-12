(in-package #:frob)

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
  (:documentation "A tool whose operation is isolated in a disposable Lisp worker."))

(defclass self-tool (tool)
  ()
  (:documentation "A tool whose operation targets the active Frob image."))

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

(defclass lisp-run-tests-tool (lisp-tool)
  ()
  (:documentation "Run an ASDF system's tests in the worker."))

(defclass lisp-reset-tool (lisp-tool)
  ()
  (:documentation "Discard and recreate the Lisp worker."))

(defclass self-inspect-tool (self-tool)
  ()
  (:documentation "Inspect a documented symbol in the active image."))

(defclass self-source-tool (self-tool)
  ()
  (:documentation "Read tracked top-level definitions for one active symbol."))

(defclass self-eval-tool (self-tool)
  ()
  (:documentation "Evaluate one exploratory form in the active image."))

(defclass self-redefine-tool (self-tool)
  ()
  (:documentation "Compile and install one exploratory top-level definition."))

(defclass self-set-tool (self-tool)
  ()
  (:documentation "Set one active global binding to an evaluated value."))

(defclass self-persist-definition-tool (self-tool)
  ()
  (:documentation "Install and persist one complete top-level definition."))

(defclass self-diff-tool (self-tool)
  ()
  (:documentation "Show tracked source changes in the Frob repository."))

(defclass self-commit-tool (self-tool)
  ()
  (:documentation "Commit an explicit set of checked Frob source paths."))

(defclass self-checkpoint-tool (self-tool)
  ()
  (:documentation "Save the active image as a retained working generation."))

(defclass self-generations-tool (self-tool)
  ()
  (:documentation "List retained working generations and compatibility."))

(defclass self-rollback-tool (self-tool)
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

(-> tool-namespace-description (string) string)
(defun tool-namespace-description (namespace)
  "Return the model-visible description of tool NAMESPACE."
  (cond
    ((string= namespace "fs")
     "Workspace file reading and listing.")
    ((string= namespace "shell")
     "External commands run in the workspace.")
    ((string= namespace "lisp")
     "Operations in a separate disposable Common Lisp worker.")
    ((string= namespace "self")
     "Operations on the active Frob Common Lisp image.")
    (t
     "Frob operations.")))

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
    :documentation "The disposable worker manager.")
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
    :documentation "The optional durable-mutation check strategy."))
  (:documentation "The explicit capabilities supplied to one tool execution."))

(defclass tool-result ()
  ((content
    :initarg :content
    :reader tool-result-content
    :type string
    :documentation "The bounded model-visible result.")
   (success-p
    :initarg :success-p
    :reader tool-result-success-p
    :type boolean
    :documentation "True when the tool operation succeeded."))
  (:documentation "The model-visible outcome of exactly one tool call."))

(-> tool-success (t) tool-result)
(defun tool-success (content)
  "Return a successful bounded tool result containing CONTENT."
  (make-instance 'tool-result
                 :content (bounded-string content)
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


;;;; -- Default Tool Set --

(-> required-form-schema (string) json-object)
(defun required-form-schema (description)
  "Return a closed schema containing one required Lisp FORM string."
  (let ((properties (json-object
                     "form" (tool-string-property description))))
    (tool-object-schema properties '("form"))))

(-> make-default-tool-registry () tool-registry)
(defun make-default-tool-registry ()
  "Create the initial documented lisp.* and self.* tool registry."
  (let ((registry (make-instance 'tool-registry))
        (empty-schema (tool-object-schema (json-object) nil)))
    (flet ((register (class namespace name description parameters)
             (tool-registry-register
              registry
              (make-instance class
                             :namespace namespace
                             :name name
                             :description description
                             :parameters parameters))))
      (register 'fs-read-tool
                "fs" "read"
                "Read one workspace file, returning numbered lines from an optional window."
                (tool-object-schema
                 (json-object
                  "path" (tool-string-property
                          "The file path, absolute or workspace-relative.")
                  "start-line" (tool-integer-property
                                "The first line to return, starting at 1.")
                  "line-count" (tool-integer-property
                                "How many lines to return; default 400."))
                 '("path")))
      (register 'fs-list-tool
                "fs" "list"
                "List one workspace directory's entries with kinds and byte sizes."
                (tool-object-schema
                 (json-object
                  "path" (tool-string-property
                          "The directory path; defaults to the workspace."))
                 nil))
      (register 'fs-write-tool
                "fs" "write"
                "Create or replace one workspace file with the supplied content."
                (tool-object-schema
                 (json-object
                  "path" (tool-string-property
                          "The file path, absolute or workspace-relative.")
                  "content" (tool-string-property
                             "The complete new file content."))
                 '("path" "content")))
      (register 'fs-edit-tool
                "fs" "edit"
                "Replace one exact text occurrence inside a workspace file."
                (tool-object-schema
                 (json-object
                  "path" (tool-string-property
                          "The file path, absolute or workspace-relative.")
                  "old-text" (tool-string-property
                              "The exact existing text to replace; include enough context to be unique.")
                  "new-text" (tool-string-property
                              "The replacement text.")
                  "replace-all" (tool-boolean-property
                                 "Replace every occurrence instead of requiring a unique match."))
                 '("path" "old-text" "new-text")))
      (register 'shell-run-tool
                "shell" "run"
                "Run one external command line in the workspace and return its exit code and combined output."
                (tool-object-schema
                 (json-object
                  "command" (tool-string-property
                             "The shell command line to execute.")
                  "directory" (tool-string-property
                               "The working directory; defaults to the workspace.")
                  "timeout-seconds" (tool-integer-property
                                     "Seconds before the command is stopped; default 60."))
                 '("command")))
      (register 'lisp-eval-tool
                "lisp" "eval"
                "Evaluate one Common Lisp form in the disposable worker."
                (required-form-schema "One readable Common Lisp form."))
      (register 'lisp-compile-tool
                "lisp" "compile"
                "Compile and execute one Common Lisp form in the disposable worker."
                (required-form-schema "One readable Common Lisp form."))
      (register 'lisp-load-system-tool
                "lisp" "load-system"
                "Load one ASDF or Quicklisp system in the disposable worker."
                (tool-object-schema
                 (json-object
                  "system" (tool-string-property "The ASDF system name."))
                 '("system")))
      (register 'lisp-describe-tool
                "lisp" "describe"
                "Describe a readable Lisp object or symbol in the disposable worker."
                (tool-object-schema
                 (json-object
                  "designator" (tool-string-property
                                "A readable Lisp form naming the object."))
                 '("designator")))
      (register 'lisp-run-tests-tool
                "lisp" "run-tests"
                "Run ASDF tests for one system in the disposable worker."
                (tool-object-schema
                 (json-object
                  "system" (tool-string-property "The ASDF system name."))
                 '("system")))
      (register 'lisp-reset-tool
                "lisp" "reset"
                "Discard the current Lisp worker and start a pristine one."
                empty-schema)
      (register 'self-inspect-tool
                "self" "inspect"
                "Inspect documentation, bindings, lambda list, and description for an active symbol."
                (tool-object-schema
                 (json-object
                  "symbol" (tool-string-property
                            "A symbol name, optionally package-qualified."))
                 '("symbol")))
      (register 'self-source-tool
                "self" "source"
                "Read complete tracked source definitions for an active symbol without general evaluation."
                (tool-object-schema
                 (json-object
                  "symbol" (tool-string-property
                            "A symbol name, optionally package-qualified."))
                 '("symbol")))
      (register 'self-eval-tool
                "self" "eval"
                "Evaluate one exploratory Common Lisp form in the active image."
                (required-form-schema "One readable Common Lisp form."))
      (register 'self-redefine-tool
                "self" "redefine"
                "Compile and install one complete exploratory top-level definition."
                (tool-object-schema
                 (json-object
                  "definition" (tool-string-property
                                "A complete defining Common Lisp form."))
                 '("definition")))
      (register 'self-set-tool
                "self" "set"
                "Set one active global binding to the value of a Common Lisp form."
                (tool-object-schema
                 (json-object
                  "symbol" (tool-string-property "The active global symbol name.")
                  "value" (tool-string-property "A Common Lisp value form."))
                 '("symbol" "value")))
      (register 'self-persist-definition-tool
                "self" "persist-definition"
                "Compile, install, and form-aware persist one complete top-level definition."
                (tool-object-schema
                 (json-object
                  "definition" (tool-string-property "A complete defining Common Lisp form.")
                  "pathname" (tool-string-property
                              "A source pathname relative to the Frob root."))
                 '("definition" "pathname")))
      (register 'self-diff-tool
                "self" "diff"
                "Show the current tracked source diff without modifying the repository."
                empty-schema)
      (register 'self-commit-tool
                "self" "commit"
                "Commit explicit checked source paths with one short title."
                (tool-object-schema
                 (json-object
                  "title" (tool-string-property "A single commit title under 72 characters.")
                  "paths" (tool-string-array-property
                           "Repository-relative paths to stage and commit."))
                 '("title" "paths")))
      (register 'self-checkpoint-tool
                "self" "checkpoint"
                "Validate and asynchronously save the active image as a generation."
                empty-schema)
      (register 'self-generations-tool
                "self" "generations"
                "List retained generations and whether this SBCL can boot them."
                empty-schema)
      (register 'self-rollback-tool
                "self" "rollback"
                "Select a compatible retained generation and request immediate rollback."
                (tool-object-schema
                 (json-object
                  "generation" (tool-string-property
                                 "The retained generation identifier."))
                 '("generation"))))
    registry))
