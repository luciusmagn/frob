(in-package #:autolith)

;;;; -- Default Tool Set --

(-> default-tools--register (tool-registry list) tool)
(defun default-tools--register (registry specification)
  "Create and register one default tool from SPECIFICATION."
  (destructuring-bind
      (class namespace name description parameters
       &rest initialization-arguments)
      specification
    (tool-registry-register
     registry
     (apply #'make-instance
            class
            :namespace namespace
            :name name
            :description description
            :parameters parameters
            initialization-arguments))))

(-> default-tools--required-form-schema (string) json-object)
(defun default-tools--required-form-schema (description)
  "Return a closed schema containing FORM and an optional named REPL."
  (let ((properties
          (json-object
           "form" (tool-string-property description)
           "repl" (tool-string-property
                    "The persistent REPL name; defaults to default."))))
    (tool-object-schema properties '("form"))))

(-> default-tools--lisp-repl-control-schema
    (&key (:include-image boolean))
    json-object)
(defun default-tools--lisp-repl-control-schema (&key include-image)
  "Return the shared schema for named REPL lifecycle operations."
  (let ((properties
          (json-object
           "repl" (tool-string-property
                   "The persistent REPL name; defaults to default."))))
    (when include-image
      (setf (gethash "image" properties)
            (tool-string-property
             "The pristine or saved worker image; defaults to pristine.")))
    (tool-object-schema properties nil)))

(-> default-tools--register-workspace (tool-registry) tool-registry)
(defun default-tools--register-workspace (registry)
  "Register the default filesystem tools in REGISTRY."
  (dolist
      (specification
       (list
        (list
         'fs-read-tool
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
        (list
         'fs-view-image-tool
         "fs" "view-image"
         "View a local image file when visual inspection is needed. The image is returned directly to the model."
         (tool-object-schema
          (json-object
           "path" (tool-string-property
                   "The image path, absolute or workspace-relative."))
          '("path")))
        (list
         'fs-list-tool
         "fs" "list"
         "List one workspace directory's entries with kinds and byte sizes."
         (tool-object-schema
          (json-object
           "path" (tool-string-property
                   "The directory path; defaults to the workspace."))
          nil))
        (list
         'fs-write-tool
         "fs" "write"
         "Create or replace one workspace file with the supplied content."
         (tool-object-schema
          (json-object
           "path" (tool-string-property
                   "The file path, absolute or workspace-relative.")
           "content" (tool-string-property
                      "The complete new file content."))
          '("path" "content")))
        (list
         'fs-edit-tool
         "fs" "edit"
         "Replace one exact text occurrence inside a workspace file."
         (tool-object-schema
          (json-object
           "path" (tool-string-property
                   "The file path, absolute or workspace-relative.")
           "old-text" (tool-string-property
                       "The exact existing text to replace; include enough context to be unique.")
           "new-text" (tool-string-property "The replacement text.")
           "replace-all" (tool-boolean-property
                          "Replace every occurrence instead of requiring a unique match."))
          '("path" "old-text" "new-text")))))
    (default-tools--register registry specification))
  registry)

(-> default-tools--register-shell (tool-registry) tool-registry)
(defun default-tools--register-shell (registry)
  "Register the workspace command tool in REGISTRY."
  (default-tools--register
   registry
   (list
    'shell-run-tool
    "shell" "run"
    "Run one external command line in the workspace and return its exit code and combined output."
    (tool-object-schema
     (json-object
      "command" (tool-string-property "The shell command line to execute.")
      "directory" (tool-string-property
                   "The working directory; defaults to the workspace.")
      "timeout-seconds" (tool-integer-property
                         "Seconds before the command is stopped; default 60."))
     '("command"))))
  registry)

(-> default-tools--register-search (tool-registry worker) tool-registry)
(defun default-tools--register-search (registry worker)
  "Register indexed workspace search tools using WORKER in REGISTRY."
  (dolist
      (specification
       (list
        (list
         'search-files-tool
         "search" "files"
         "Fuzzy-search indexed workspace file paths. Use a short one- or two-term topic, filename, or path query."
         (tool-object-schema
          (json-object
           "query" (tool-string-property
                    "A short fuzzy filename, topic, or path query.")
           "page" (tool-integer-property
                   "Zero-based result page; default 0.")
           "max-results" (tool-integer-property
                          "Results per page from 1 to 100; default 20."))
          '("query"))
         :engine worker)
        (list
         'search-glob-tool
         "search" "glob"
         "Filter indexed workspace paths with one literal glob such as **/*.lisp."
         (tool-object-schema
          (json-object
           "pattern" (tool-string-property
                      "One literal glob matched against workspace-relative paths.")
           "page" (tool-integer-property
                   "Zero-based result page; default 0.")
           "max-results" (tool-integer-property
                          "Results per page from 1 to 100; default 20."))
          '("pattern"))
         :engine worker)
        (list
         'search-content-tool
         "search" "content"
         "Search indexed workspace contents. Plain matching is the default; put file or path constraints in the query, for example '*.lisp symbol', 'src/ symbol', or '!tests/ symbol'."
         (tool-object-schema
          (json-object
           "query" (tool-string-property
                    "Text plus optional inline file constraints to find.")
           "mode" (json-object
                   "type" "string"
                   "enum" #("plain" "regex" "fuzzy")
                   "description" "Matching mode; default plain.")
           "file-offset" (tool-integer-property
                          "Pagination cursor from next-file-offset; default 0.")
           "max-results" (tool-integer-property
                          "Matches returned from 1 to 100; default 20.")
           "max-matches-per-file" (tool-integer-property
                                   "Matches retained per file from 1 to 100; default 20.")
           "context" (tool-integer-property
                      "Lines before and after each match from 0 to 10; default 0.")
           "time-budget-ms" (tool-integer-property
                             "Search budget from 1 to 10000 milliseconds; default 3000."))
          '("query"))
         :engine worker)
        (list
         'search-multi-content-tool
         "search" "multi-content"
         "Search indexed contents once for any of several literal patterns, with optional file and path constraints."
         (tool-object-schema
          (json-object
           "patterns" (tool-string-array-property
                       "Non-empty literal alternatives searched in one pass.")
           "constraints" (tool-string-property
                          "Optional space-separated file constraints such as '*.lisp src/ !tests/'.")
           "file-offset" (tool-integer-property
                          "Pagination cursor from next-file-offset; default 0.")
           "max-results" (tool-integer-property
                          "Matches returned from 1 to 100; default 20.")
           "max-matches-per-file" (tool-integer-property
                                   "Matches retained per file from 1 to 100; default 20.")
           "context" (tool-integer-property
                      "Lines before and after each match from 0 to 10; default 0.")
           "time-budget-ms" (tool-integer-property
                             "Search budget from 1 to 10000 milliseconds; default 3000."))
          '("patterns"))
         :engine worker)))
    (default-tools--register registry specification))
  registry)

(-> default-tools--register-memory (tool-registry) tool-registry)
(defun default-tools--register-memory (registry)
  "Register persistent memory tools in REGISTRY."
  (dolist
      (specification
       (list
        (list
         'memory-remember-tool
         "memory" "remember"
         "Create one persistent memory, or completely replace an existing memory by id. Store only durable, useful facts, preferences, decisions, and guidance, never credentials or other secrets."
         (tool-object-schema
          (json-object
           "title" (tool-string-property "A short retrieval-oriented title.")
           "content" (tool-string-property
                      "Complete durable memory content, at most 5000 characters.")
           "scope" (tool-string-property
                    "global or workspace; defaults to workspace for a new memory and preserves scope on replacement.")
           "tags" (tool-string-array-property
                   "Optional short retrieval terms.")
           "id" (tool-string-property
                 "An existing memory id to replace; omit to create."))
          '("title" "content")))
        (list
         'memory-list-tool
         "memory" "list"
         "List persistent memory metadata, newest first."
         (tool-object-schema
          (json-object
           "scope" (tool-string-property
                    "relevant, global, workspace, or all; defaults to relevant.")
           "max-results" (tool-integer-property
                          "Maximum entries to return; defaults to 20 and is capped at 50."))
          nil))
        (list
         'memory-search-tool
         "memory" "search"
         "Search persistent memory titles, bodies, tags, and workspace names in weighted relevance order."
         (tool-object-schema
          (json-object
           "query" (tool-string-property
                    "Case-insensitive lexical search terms; title and tag matches rank highest.")
           "scope" (tool-string-property
                    "relevant, global, workspace, or all; defaults to relevant.")
           "max-results" (tool-integer-property
                          "Maximum entries to return; defaults to 20 and is capped at 50."))
          '("query")))
        (list
         'memory-read-tool
         "memory" "read"
         "Read one complete active persistent memory by id."
         (tool-object-schema
          (json-object
           "id" (tool-string-property "The exact memory identifier."))
          '("id")))
        (list
         'memory-forget-tool
         "memory" "forget"
         "Stop recalling one persistent memory by id. Use only when the user asks to forget it or confirms that it is obsolete."
         (tool-object-schema
          (json-object
           "id" (tool-string-property "The exact memory identifier."))
          '("id")))))
    (default-tools--register registry specification))
  registry)

(-> default-tools--register-agenda (tool-registry) tool-registry)
(defun default-tools--register-agenda (registry)
  "Register workspace agenda tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'agenda-list-tool
           "agenda" "list"
           "Read the current workspace's complete agenda."
           empty-schema)
          (list
           'agenda-add-tool
           "agenda" "add"
           "Add one short task, thought, or note to the current workspace agenda."
           (tool-object-schema
            (json-object
             "text" (tool-string-property
                     "The complete item text, at most 500 characters.")
             "status" (json-object
                       "type" "string"
                       "enum" #("todo" "doing" "blocked" "done" "note")
                       "description" "The item status; defaults to todo.")
             "memory-ids" (tool-string-array-property
                           "Optional active persistent-memory ids to attach."))
            '("text")))
          (list
           'agenda-update-tool
           "agenda" "update"
           "Change one current-workspace agenda item by stable id."
           (tool-object-schema
            (json-object
             "id" (tool-string-property "The exact agenda item identifier.")
             "text" (tool-string-property
                     "Optional complete replacement text, at most 500 characters.")
             "status" (json-object
                       "type" "string"
                       "enum" #("todo" "doing" "blocked" "done" "note")
                       "description" "Optional replacement item status.")
             "memory-ids" (tool-string-array-property
                           "Optional complete replacement memory-id list; an empty array detaches all memories."))
            '("id")))
          (list
           'agenda-remove-tool
           "agenda" "remove"
           "Remove one current-workspace agenda item by stable id."
           (tool-object-schema
            (json-object
             "id" (tool-string-property "The exact agenda item identifier."))
            '("id")))
          (list
           'agenda-transport-tool
           "agenda" "transport"
           "Enumerate or inspect workspace agendas, or copy or move one agenda to an existing workspace directory. Move rekeys an agenda after its repository changes location."
           (tool-object-schema
            (json-object
             "operation" (json-object
                          "type" "string"
                          "enum" #("workspaces" "view" "copy" "move")
                          "description" "workspaces lists known keys; view reads one; copy merges into a target while retaining the source; move merges and removes the source key.")
             "source-directory" (tool-string-property
                                 "The source workspace key. Required for view, copy, and move; it may name a repository path that no longer exists.")
             "target-directory" (tool-string-property
                                 "An existing destination workspace for copy or move; defaults to the current workspace."))
            '("operation")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-lisp (tool-registry) tool-registry)
(defun default-tools--register-lisp (registry)
  "Register named, isolated Common Lisp worker tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'lisp-eval-tool
           "lisp" "eval"
           "Evaluate one Common Lisp form in a named persistent REPL."
           (default-tools--required-form-schema
            "One readable Common Lisp form."))
          (list
           'lisp-compile-tool
           "lisp" "compile"
           "Compile and execute one Common Lisp form in a named persistent REPL."
           (default-tools--required-form-schema
            "One readable Common Lisp form."))
          (list
           'lisp-load-system-tool
           "lisp" "load-system"
           "Load one ASDF or Quicklisp system in a named persistent REPL."
           (tool-object-schema
            (json-object
             "system" (tool-string-property "The ASDF system name.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default."))
            '("system")))
          (list
           'lisp-describe-tool
           "lisp" "describe"
           "Describe a readable Lisp object or symbol in a named persistent REPL."
           (tool-object-schema
            (json-object
             "designator" (tool-string-property
                           "A readable Lisp form naming the object.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default."))
            '("designator")))
          (list
           'lisp-source-tool
           "lisp" "source"
           "Read a definition from the hash-verified source matching the pinned SBCL runtime, using source locations from one named REPL."
           (tool-object-schema
            (json-object
             "name" (tool-string-property
                     "A readable definition name, such as CL:MAPCAR or SB-C::IR1-CONVERT.")
             "kind" (tool-string-property
                     "An optional SBCL definition kind, such as function, optimizer, transform, or vop.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default."))
            '("name")))
          (list
           'lisp-run-tests-tool
           "lisp" "run-tests"
           "Run ASDF tests for one system in a named persistent REPL."
           (tool-object-schema
            (json-object
             "system" (tool-string-property "The ASDF system name.")
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default."))
            '("system")))
          (list
           'lisp-reset-tool
           "lisp" "reset"
           "Discard one named REPL and recreate it from pristine or a compatible saved image."
           (default-tools--lisp-repl-control-schema :include-image t))
          (list
           'lisp-start-tool
           "lisp" "start"
           "Start a named persistent REPL from pristine or a compatible saved image without silently switching an existing REPL."
           (default-tools--lisp-repl-control-schema :include-image t))
          (list
           'lisp-stop-tool
           "lisp" "stop"
           "Stop and forget one named persistent REPL."
           (default-tools--lisp-repl-control-schema))
          (list
           'lisp-repls-tool
           "lisp" "repls"
           "List named persistent REPLs, whether they are running, and their base images."
           empty-schema)
          (list
           'lisp-images-tool
           "lisp" "images"
           "List pristine and saved worker images with compatibility, parentage, and durable notes."
           empty-schema)
          (list
           'lisp-save-image-tool
           "lisp" "save-image"
           "Fork and save one named REPL as an immutable SBCL worker image, then boot-probe it and record why it exists."
           (tool-object-schema
            (json-object
             "repl" (tool-string-property
                     "The persistent REPL name; defaults to default.")
             "image" (tool-string-property
                      "The new immutable image name.")
             "note" (tool-string-property
                     "What changed in this image and when to use it."))
            '("image" "note")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--register-self (tool-registry) tool-registry)
(defun default-tools--register-self (registry)
  "Register active-image inspection and mutation tools in REGISTRY."
  (let ((empty-schema (tool-object-schema (json-object) nil)))
    (dolist
        (specification
         (list
          (list
           'self-inspect-tool
           "self" "inspect"
           "Inspect documentation, bindings, lambda list, and description for an active symbol."
           (tool-object-schema
            (json-object
             "symbol" (tool-string-property
                       "A symbol name, optionally package-qualified."))
            '("symbol")))
          (list
           'self-source-tool
           "self" "source"
           "Read complete tracked Autolith or direct dependency source, or hash-verified matching SBCL source, for an active symbol without general evaluation."
           (tool-object-schema
            (json-object
             "symbol" (tool-string-property
                       "A symbol name, optionally package-qualified.")
             "package" (tool-string-property
                        "The reader package for an unqualified symbol; defaults to AUTOLITH.")
             "system" (tool-string-property
                        "An optional direct Autolith ASDF dependency containing the symbol.")
             "kind" (tool-string-property
                      "An optional SBCL definition kind when inspecting implementation source."))
            '("symbol")))
          (list
           'self-eval-tool
           "self" "eval"
           "Evaluate one exploratory Common Lisp form in the active image."
           (tool-object-schema
            (json-object
             "form" (tool-string-property "One readable Common Lisp form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("form")))
          (list
           'self-redefine-tool
           "self" "redefine"
           "Compile and install one complete exploratory top-level definition in the active image, including Lisp-level SBCL implementation packages."
           (tool-object-schema
            (json-object
             "definition" (tool-string-property
                           "A complete defining Common Lisp form.")
             "package" (tool-string-property
                        "The active package in which to read and install the definition; defaults to AUTOLITH.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("definition")))
          (list
           'self-set-tool
           "self" "set"
           "Set one active global binding to the value of a Common Lisp form."
           (tool-object-schema
            (json-object
             "symbol" (tool-string-property "The active global symbol name.")
             "value" (tool-string-property "A Common Lisp value form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("symbol" "value")))
          (list
           'self-persist-definition-tool
           "self" "persist-definition"
           "Compile, install, check, and persist one complete definition in a private image commit backed by Autolith's private mutation-history Git repository. The tracked source repository is never modified."
           (tool-object-schema
            (json-object
             "definition" (tool-string-property
                           "A complete defining Common Lisp form.")
             "restart" (tool-restart-property)
             "restart-value" (tool-restart-value-property))
            '("definition")))
          (list
           'self-status-tool
           "self" "status"
           "Summarize running and selected private image state, effective pending mutations, and retained generations."
           empty-schema)
          (list
           'self-discard-tool
           "self" "discard"
           "Restore and discard the newest effective exploratory mutation, or the effective mutation with a specified identifier."
           (tool-object-schema
            (json-object
             "mutation" (tool-string-property
                         "An effective mutation identifier from self.diff; defaults to the newest effective mutation."))
            nil))
          (list
           'self-exercise-tool
           "self" "exercise"
           "Evaluate and journal one focused assertion-style Common Lisp form against an effective pending mutation. A signaled condition fails the exercise; this does not replace self.commit's full checks."
           (tool-object-schema
            (json-object
             "form" (tool-string-property
                     "One Common Lisp exercise form; use ASSERT or ERROR to signal failure.")
             "mutation" (tool-string-property
                         "An effective mutation identifier from self.diff; defaults to the newest effective mutation."))
            '("form")))
          (list
           'self-diff-tool
           "self" "diff"
           "Show the effective self.redefine and self.set changes not yet persisted by self.commit, collapsing repeated edits to each target."
           empty-schema)
          (list
           'self-commit-tool
           "self" "commit"
           "Check and persist all pending self.redefine and self.set mutations as an immutable private image commit and complete Lisp replay script, then retain the snapshot in Autolith's private mutation-history Git repository. This never changes a workspace repository."
           (tool-object-schema
            (json-object
             "title" (tool-string-property
                      "A single private image-commit title under 72 characters."))
            '("title")))
          (list
           'self-checkpoint-tool
           "self" "checkpoint"
           "Validate and asynchronously save the active image as a generation."
           empty-schema)
          (list
           'self-generations-tool
           "self" "generations"
           "List retained generations and whether this SBCL can boot them."
           empty-schema)
          (list
           'self-rollback-tool
           "self" "rollback"
           "Select a compatible retained generation and request immediate rollback."
           (tool-object-schema
            (json-object
             "generation" (tool-string-property
                           "The retained generation identifier."))
            '("generation")))))
      (default-tools--register registry specification)))
  registry)

(-> default-tools--remove-mutable-self-tools (tool-registry) tool-registry)
(defun default-tools--remove-mutable-self-tools (registry)
  "Remove every mutable active-image tool from REGISTRY."
  (dolist (tool (copy-list (tool-registry-tools registry)))
    (when (typep tool 'mutable-self-tool)
      (remhash (tool-canonical-name tool) (tool-registry-index registry))))
  (setf (tool-registry-tools registry)
        (remove-if (lambda (tool)
                     (typep tool 'mutable-self-tool))
                   (tool-registry-tools registry)))
  registry)

(-> make-default-tool-registry (&key (:immutable-p boolean)) tool-registry)
(defun make-default-tool-registry (&key immutable-p)
  "Create Autolith's tool registry, omitting mutable self tools when requested."
  (let ((registry (make-instance 'tool-registry))
        (search-worker (search-worker-create)))
    (default-tools--register-workspace registry)
    (default-tools--register-search registry search-worker)
    (default-tools--register-shell registry)
    (default-tools--register-memory registry)
    (default-tools--register-agenda registry)
    (default-tools--register-lisp registry)
    (default-tools--register-self registry)
    (when immutable-p
      (default-tools--remove-mutable-self-tools registry))
    registry))
