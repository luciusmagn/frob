(in-package #:autolith)

;;;; -- Skill Test Support --

(-> skill-tests--write (pathname string string) pathname)
(defun skill-tests--write (root relative content)
  "Write CONTENT beneath ROOT at RELATIVE and return the resulting pathname."
  (let ((pathname (merge-pathnames relative root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string content stream))
    pathname))

(-> skill-tests--definition
    (string string string &key (:version t))
    string)
(defun skill-tests--definition
    (name description instructions &key (version 1))
  "Return one native skill definition string."
  (format nil
          "(:autolith-skill~% :version ~S~% :name ~S~% :description ~S~% :instructions ~S)~%"
          version
          name
          description
          instructions))

(-> skill-tests--names (skill-catalog) list)
(defun skill-tests--names (catalog)
  "Return selected skill names from CATALOG."
  (mapcar #'skill-metadata-name (skill-catalog-skills catalog)))

(-> skill-tests--diagnostic-kinds (skill-catalog) list)
(defun skill-tests--diagnostic-kinds (catalog)
  "Return diagnostic kinds from CATALOG."
  (mapcar #'skill-diagnostic-kind (skill-catalog-diagnostics catalog)))

(-> skill-tests--contribution (list string) (option context-contribution))
(defun skill-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key #'context-contribution-identifier
        :test #'string=))

(-> skill-tests--contribution-identifiers (list) list)
(defun skill-tests--contribution-identifiers (contributions)
  "Return stable identifiers from request-local skill CONTRIBUTIONS."
  (mapcar #'context-contribution-identifier contributions))

(-> skill-tests--delete-root (pathname) null)
(defun skill-tests--delete-root (root)
  "Delete temporary test ROOT when it exists."
  (when (probe-file root)
    (uiop:delete-directory-tree root
                                :validate t
                                :if-does-not-exist :ignore))
  nil)

(-> skill-tests--definition-error-kind (function) (option keyword))
(defun skill-tests--definition-error-kind (function)
  "Return the internal skill error kind signaled by FUNCTION, if any."
  (handler-case
      (progn
        (funcall function)
        nil)
    (skill--definition-error (condition)
      (skill--definition-error-kind condition))))

(-> skill-tests--symlink (pathname pathname) pathname)
(defun skill-tests--symlink (target link)
  "Create LINK pointing to TARGET and return LINK."
  (ensure-directories-exist link)
  (sb-posix:symlink (namestring target) (namestring link))
  link)

(-> skill-tests--tool-namespaces (&key (:load-p boolean)) vector)
(defun skill-tests--tool-namespaces (&key (load-p t))
  "Return a provider namespace vector with optional skill.load visibility."
  (vector
   (json-object
    "type" "namespace"
    "name" "skill"
    "tools"
    (vector
     (json-object
      "name" (if load-p "load" "unavailable"))))))


;;;; -- Discovery and Native Form Tests --

(-> skill-tests--discovery-and-precedence () null)
(defun skill-tests--discovery-and-precedence ()
  "Test recursive discovery, strict filenames, precedence, and fail closure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (primary (merge-pathnames "primary/" root))
         (secondary (merge-pathnames "secondary/" root)))
    (unwind-protect
         (progn
           (skill-tests--write
            primary
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "Handles several
related operations."
             "Primary alpha instructions."))
           (skill-tests--write
            primary
            "blocked/SKILL.sexp"
            "(:autolith-skill :version 1 :name \"blocked\" :description \"Broken\")")
           (skill-tests--write
            primary
            "wrong/SKILL.sexp"
            (skill-tests--definition
             "different"
             "The parent directory does not match."
             "Never selected."))
           (skill-tests--write
            primary
            ".hidden/SKILL.sexp"
            (skill-tests--definition
             "hidden"
             "This hidden directory must not be scanned."
             "Hidden."))
           (skill-tests--write
            primary
            "legacy/SKILL.md"
            "This compatibility filename must be ignored.")
           (skill-tests--write
            secondary
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "This lower-precedence definition must lose."
             "Secondary alpha instructions."))
           (skill-tests--write
            secondary
            "beta/SKILL.sexp"
            (skill-tests--definition
             "beta"
             "A valid secondary skill."
             "Beta instructions."))
           (skill-tests--write
            secondary
            "blocked/SKILL.sexp"
            (skill-tests--definition
             "blocked"
             "A valid lower-precedence definition."
             "Must remain blocked."))
           (let* ((catalog
                    (skill-catalog-discover (list primary secondary)))
                  (kinds (skill-tests--diagnostic-kinds catalog)))
             (test-assert
              (equal (skill-tests--names catalog) '("alpha" "beta"))
              "native skills are path-sorted and ordered by root precedence")
             (test-assert
              (string=
               (skill-metadata-description
                (skill-catalog-find catalog "alpha"))
               "Handles several related operations.")
              "skill descriptions become bounded single-line metadata")
             (test-assert
              (member ':missing-field kinds)
              "a missing native field produces a typed diagnostic")
             (test-assert
              (member ':name-directory-mismatch kinds)
              "the declared name must match the immediate parent directory")
             (test-assert
              (= (count ':shadowed kinds) 2)
              "valid and malformed higher-precedence directories reserve names")
             (test-assert
              (null (skill-catalog-find catalog "blocked"))
              "a malformed higher-precedence skill blocks a lower definition")
             (test-assert
              (null (skill-catalog-find catalog "hidden"))
              "hidden directories beneath a skill root are skipped")
             (test-assert
              (null (skill-catalog-find catalog "legacy"))
              "SKILL.md compatibility files are ignored")))
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--filesystem-boundaries () null)
(defun skill-tests--filesystem-boundaries ()
  "Test root confinement, lexical reservation, and regular-file enforcement."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (primary (merge-pathnames "primary/" root))
         (secondary (merge-pathnames "secondary/" root))
         (outside (merge-pathnames "outside/" root)))
    (unwind-protect
         (progn
           (let ((target
                   (skill-tests--write
                    outside
                    "different/SKILL.sexp"
                    (skill-tests--definition
                     "different"
                     "An out-of-root file target."
                     "Must not load."))))
             (skill-tests--symlink
              target
              (merge-pathnames "file-link/SKILL.sexp" primary)))
           (skill-tests--write
            secondary
            "file-link/SKILL.sexp"
            (skill-tests--definition
             "file-link"
             "A lower file-link definition."
             "Must remain blocked."))
           (let ((target-directory
                   (uiop:pathname-directory-pathname
                    (skill-tests--write
                     outside
                     "directory-link/SKILL.sexp"
                     (skill-tests--definition
                      "directory-link"
                      "An out-of-root directory target."
                      "Must not load.")))))
             (skill-tests--symlink
              target-directory
              (merge-pathnames "directory-link" primary)))
           (skill-tests--write
            secondary
            "directory-link/SKILL.sexp"
            (skill-tests--definition
             "directory-link"
             "A lower directory-link definition."
             "Must remain blocked."))
           (skill-tests--symlink
            (merge-pathnames "missing-directory/" outside)
            (merge-pathnames "broken-link" primary))
           (skill-tests--write
            secondary
            "broken-link/SKILL.sexp"
            (skill-tests--definition
             "broken-link"
             "A lower broken-link definition."
             "Must remain blocked."))
           (let ((fifo (merge-pathnames "fifo/SKILL.sexp" primary)))
             (ensure-directories-exist fifo)
             (sb-posix:mkfifo (namestring fifo) #o600))
           (skill-tests--write
            secondary
            "fifo/SKILL.sexp"
            (skill-tests--definition
             "fifo"
             "A lower FIFO definition."
             "Must remain blocked."))
           (let ((fifo-target (merge-pathnames "skill-pipe" outside)))
             (ensure-directories-exist fifo-target)
             (sb-posix:mkfifo (namestring fifo-target) #o600)
             (skill-tests--symlink
              fifo-target
              (merge-pathnames "fifo-link/SKILL.sexp" primary)))
           (skill-tests--write
            secondary
            "fifo-link/SKILL.sexp"
            (skill-tests--definition
             "fifo-link"
             "A lower FIFO-link definition."
             "Must remain blocked."))
           (let* ((catalog
                    (skill-catalog-discover (list primary secondary)))
                  (kinds (skill-tests--diagnostic-kinds catalog)))
             (test-assert
              (null (skill-catalog-skills catalog))
              "unsafe higher-precedence filesystem entries reserve every name")
             (test-assert
              (member ':outside-root kinds)
              "file and directory symlinks cannot escape a canonical skill root")
             (test-assert
             (member ':not-regular-file kinds)
              "direct and symbolic FIFO candidates fail without being opened")
             (test-assert
              (= (count ':shadowed kinds) 5)
              "rejected lexical candidates block all lower-precedence definitions")))
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--native-parser-rejections () null)
(defun skill-tests--native-parser-rejections ()
  "Test strict native fields, types, syntax, and structural rejection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (skills (merge-pathnames "skills/" root))
         (cases
           `(("unknown"
              ,(format nil
                       "(:autolith-skill :version 1 :name \"unknown\" :description \"Unknown.\" :instructions \"x\" :extra t)")
              :unknown-field)
             ("duplicate"
              ,(format nil
                       "(:autolith-skill :version 1 :name \"duplicate\" :name \"duplicate\" :description \"Duplicate.\" :instructions \"x\")")
              :duplicate-field)
             ("missing"
              "(:autolith-skill :version 1 :name \"missing\" :description \"Missing.\")"
              :missing-field)
             ("bad-version"
              ,(skill-tests--definition
                "bad-version" "Bad version." "x" :version "1")
              :invalid-version)
             ("bad-name"
              ,(skill-tests--definition "Bad-Name" "Bad name." "x")
              :invalid-name)
             ("bad-description"
              "(:autolith-skill :version 1 :name \"bad-description\" :description 4 :instructions \"x\")"
              :invalid-description)
             ("bad-instructions"
              "(:autolith-skill :version 1 :name \"bad-instructions\" :description \"Bad instructions.\" :instructions nil)"
              :invalid-instructions)
             ("empty-instructions"
              ,(skill-tests--definition
                "empty-instructions" "Empty instructions." "   ")
              :invalid-instructions)
             ("mismatch"
              ,(skill-tests--definition
                "another-name" "Mismatched name." "x")
              :name-directory-mismatch)
             ("extra-form"
              ,(concatenate
                'string
                (skill-tests--definition
                 "extra-form" "Extra form." "x")
                "(:name)")
              :invalid-syntax)
             ("trailing-garbage"
              ,(concatenate
                'string
                (skill-tests--definition
                 "trailing-garbage" "Trailing garbage." "x")
                "garbage")
             :invalid-syntax)
             ("improper"
              "(:autolith-skill :version 1 . :name)"
              :invalid-structure)
             ("shared"
              "#1=(:autolith-skill :version 1 :name \"shared\" :description \"Shared.\" :instructions #1#)"
              :invalid-syntax)
             ("quoted"
              "'(:autolith-skill :version 1 :name \"quoted\" :description \"Quoted.\" :instructions \"x\")"
              :invalid-syntax)
             ("package-qualified"
              "(:autolith-skill :version 1 :name \"package-qualified\" :description \"Package-qualified.\" :instructions \"x\" autolith::reader-pollution \"rejected\")"
              :invalid-syntax)
             ("escaped-symbol"
              "(:autolith-skill :version 1 :name \"escaped-symbol\" :description \"Escaped symbol.\" :instructions \"x\" :|escaped-field| \"rejected\")"
              :invalid-syntax)
             ("empty"
              " ; only a comment"
              :invalid-syntax))))
    (unwind-protect
         (progn
           (dolist (case cases)
             (destructuring-bind (directory content expected-kind) case
               (skill-tests--write
                skills
                (format nil "~A/SKILL.sexp" directory)
                content)
               (let ((catalog
                       (skill-catalog-discover (list skills))))
                 (test-assert
                  (member expected-kind
                          (skill-tests--diagnostic-kinds catalog))
                  (format nil
                          "native parser rejects ~A with ~S"
                          directory
                          expected-kind)))
               (skill-tests--delete-root skills)))
           (skill-tests--write
            skills
            "too-deep/SKILL.sexp"
            (skill-tests--definition
             "too-deep"
             "Deep data."
             "((((nested))))"))
           (let* ((*skill-form-depth-limit* 0)
                  (catalog (skill-catalog-discover (list skills))))
             (test-assert
              (member ':data-too-deep
                      (skill-tests--diagnostic-kinds catalog))
              "preflight rejects data beyond the configured depth bound"))
           (skill-tests--delete-root skills)
           (skill-tests--write
            skills
            "too-large/SKILL.sexp"
            (skill-tests--definition
             "too-large"
             "Large file."
             (make-string 200 :initial-element #\x)))
           (let* ((*skill-file-character-limit* 100)
                  (catalog (skill-catalog-discover (list skills))))
             (test-assert
              (member ':file-too-large
                      (skill-tests--diagnostic-kinds catalog))
              "the parser reads at most its exact file character bound"))
           (test-assert
            (eq
             (skill-tests--definition-error-kind
              (lambda ()
                (let ((shared (list "shared")))
                  (skill--validate-tree (list shared shared)))))
             ':invalid-structure)
            "the structure validator rejects shared conses")
           (test-assert
            (eq
             (skill-tests--definition-error-kind
              (lambda ()
                (let ((circular (list "circular")))
                  (setf (rest circular) circular)
                  (skill--validate-tree circular))))
             ':invalid-structure)
            "the structure validator rejects circular conses")
           (let ((*skill-form-node-limit* 3))
             (test-assert
              (eq
               (skill-tests--definition-error-kind
               (lambda ()
                  (skill--validate-tree '(:one :two :three))))
               ':data-too-large)
              "the structure validator rejects data beyond its node bound"))
           (let ((novel-symbol-name
                   (format nil
                           "AUTOLITH-NOVEL-SKILL-SYMBOL-~A"
                           (make-identifier))))
             (test-assert
              (null
               (find-symbol novel-symbol-name
                            (find-package '#:autolith)))
              "the novel reader-safety symbol begins uninterned")
             (skill-tests--delete-root skills)
             (skill-tests--write
              skills
              "temporary-package/SKILL.sexp"
              (format nil
                      "(:autolith-skill :version 1 :name \"temporary-package\" :description \"Temporary package.\" :instructions \"x\" ~A \"rejected\")"
                      novel-symbol-name))
             (let ((catalog (skill-catalog-discover (list skills))))
               (test-assert
                (member ':unknown-field
                        (skill-tests--diagnostic-kinds catalog))
                "a bare novel field is rejected after native reading"))
             (test-assert
              (null
               (find-symbol novel-symbol-name
                            (find-package '#:autolith)))
              "native reading never interns novel symbols into AUTOLITH"))
           (let ((novel-keyword-name
                   (format nil
                           "AUTOLITH-NOVEL-SKILL-KEYWORD-~A"
                           (make-identifier))))
             (test-assert
              (null
               (find-symbol novel-keyword-name
                            (find-package '#:keyword)))
              "the novel reader-safety keyword begins uninterned")
             (skill-tests--delete-root skills)
             (skill-tests--write
              skills
              "keyword-pollution/SKILL.sexp"
              (format nil
                      "(:autolith-skill :version 1 :name \"keyword-pollution\" :description \"Keyword safety.\" :instructions \"x\" :~A \"rejected\")"
                      novel-keyword-name))
             (let ((catalog (skill-catalog-discover (list skills))))
               (test-assert
                (member ':unknown-field
                        (skill-tests--diagnostic-kinds catalog))
                "a novel keyword is rejected during lexical preflight"))
             (test-assert
              (null
               (find-symbol novel-keyword-name
                            (find-package '#:keyword)))
              "lexical preflight never interns novel symbols into KEYWORD")))
      (skill-tests--delete-root root)))
  nil)


;;;; -- Root and Catalog Tests --

(-> skill-tests--roots-and-rendering () null)
(defun skill-tests--roots-and-rendering ()
  "Test effective roots, root precedence, and bounded catalog rendering."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (working-directory (merge-pathnames "src/module/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (ensure-directories-exist
              (merge-pathnames "directory-marker" working-directory))
             (configuration-with-working-directory
              base-configuration
              working-directory)))
         (project-skills (merge-pathnames ".autolith/skills/" project))
         (user-skills
           (merge-pathnames
            "skills/"
            (configuration-config-root configuration))))
    (unwind-protect
         (progn
           (let ((roots (skill-roots configuration)))
             (test-assert
              (= (length roots) 3)
              "skill discovery has project, user, and bundled root positions")
             (test-assert
              (equal (first roots) project-skills)
              "only the effective Git root supplies project-local skills")
             (test-assert
              (equal (second roots) user-skills)
              "the XDG Autolith skill root follows the project root")
             (test-assert
              (equal
               (third roots)
               (merge-pathnames
                "skills/"
                (configuration-source-root configuration)))
              "the optional bundled root has lowest precedence"))
           (skill-tests--write
            project-skills
            "winner/SKILL.sexp"
            (skill-tests--definition
             "winner"
             "Project definition."
             "Project instructions."))
           (skill-tests--write
            user-skills
            "winner/SKILL.sexp"
            (skill-tests--definition
             "winner"
             "User definition."
             "User instructions."))
           (dotimes (index 8)
             (let ((name (format nil "skill-~D" index)))
               (skill-tests--write
                user-skills
                (format nil "~A/SKILL.sexp" name)
                (skill-tests--definition
                 name
                 (make-string 700
                              :initial-element
                              (code-char (+ (char-code #\a) index)))
                 (format nil "Instructions ~D." index)))))
           (let* ((catalog
                    (skill-catalog-for-configuration configuration))
                  (winner (skill-catalog-find catalog "winner")))
             (test-assert
              (equal
               (skill-metadata-pathname winner)
               (truename
                (merge-pathnames "winner/SKILL.sexp" project-skills)))
              "project-local skills take precedence over user skills")
             (multiple-value-bind (rendered included omitted)
                 (skill-catalog-render catalog :character-budget 1500)
               (test-assert
                (<= (length rendered) 1500)
                "the rendered catalog obeys its exact character budget")
               (test-assert
                (= (+ included omitted)
                   (length (skill-catalog-skills catalog)))
                "catalog rendering accounts for every discovered skill")
               (test-assert
                (plusp omitted)
                "catalog rendering reports metadata that does not fit")
               (test-assert
                (> included 1)
                "catalog packing prioritizes usable names and paths over descriptions")
               (test-assert
                (and (search "call `skill.load`" rendered)
                     (not (search "$name" rendered))
                     (search "SKILL.sexp" rendered))
                "catalog guidance describes only native skill selection"))
             (test-assert
              (handler-case
                  (progn
                    (skill-catalog-render catalog :character-budget 40)
                    nil)
                (skill-catalog-render-error ()
                  t))
              "a budget below protocol text signals a structured error")))
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--scan-limits () null)
(defun skill-tests--scan-limits ()
  "Test aggregate traversal and depth bounds."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first (merge-pathnames "first/" root))
         (second (merge-pathnames "second/" root)))
    (unwind-protect
         (progn
           (skill-tests--write
            first
            "one/SKILL.sexp"
            (skill-tests--definition
             "one" "First skill." "One."))
           (skill-tests--write
            second
            "two/SKILL.sexp"
            (skill-tests--definition
             "two" "Second skill." "Two."))
           (let ((catalog
                   (skill-catalog-discover
                    (list first second)
                    :max-directories 2
                    :max-entries 100)))
             (test-assert
              (member ':scan-directory-limit
                      (skill-tests--diagnostic-kinds catalog))
              "directory limits apply across ordered roots"))
           (let ((deep (merge-pathnames "deep/" root)))
             (skill-tests--write
              deep
              "one/two/too-deep/SKILL.sexp"
              (skill-tests--definition
               "too-deep" "Deep skill." "Deep."))
             (let ((catalog
                     (skill-catalog-discover
                      (list deep)
                      :max-depth 1)))
               (test-assert
                (and (null (skill-catalog-find catalog "too-deep"))
                     (member ':scan-depth-limit
                             (skill-tests--diagnostic-kinds catalog)))
                "depth-limited traversal is observable and non-partial")))
           (let ((entries (merge-pathnames "entries/" root)))
             (skill-tests--write
              entries
              "entry/SKILL.sexp"
             (skill-tests--definition
               "entry" "Entry skill." "Entry."))
             (skill-tests--write entries "extra.txt" "extra")
             (multiple-value-bind
                   (files subdirectories exceeded-p entry-count)
                 (skill--directory-entries-bounded entries 1)
               (test-assert
                (and exceeded-p
                     (= entry-count 1)
                     (null files)
                     (null subdirectories))
                "bounded directory enumeration retains no partial overfull listing"))
             (let ((catalog
                     (skill-catalog-discover
                      (list entries)
                      :max-directories 10
                      :max-entries 1)))
               (test-assert
                (and (null (skill-catalog-skills catalog))
                     (member ':scan-entry-limit
                             (skill-tests--diagnostic-kinds catalog)))
                "an overfull directory is not partially scanned"))))
      (skill-tests--delete-root root)))
  nil)


;;;; -- Ephemeral Selection Tests --

(-> skill-tests--ephemeral-selection () null)
(defun skill-tests--ephemeral-selection ()
  "Test fresh reads, explicit tool selection, stacking, and observability."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skill-root (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-ephemeral")))
    (unwind-protect
         (progn
           (skill-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "The alpha skill."
             "Original alpha instructions."))
           (skill-tests--write
            skill-root
            "beta/SKILL.sexp"
            (skill-tests--definition
             "beta"
             "The beta skill."
             "Beta instructions."))
           (let* ((catalog
                    (skill-catalog-for-configuration configuration))
                  (alpha (skill-catalog-find catalog "alpha")))
             (test-assert
              (string=
               (skill-metadata-read alpha)
               "Original alpha instructions.")
              "selected instructions are read on demand")
             (skill-tests--write
              skill-root
              "alpha/SKILL.sexp"
              (skill-tests--definition
               "alpha"
               "A changed description."
               "Replacement alpha instructions."))
             (test-assert
              (string=
               (skill-metadata-read alpha)
               "Replacement alpha instructions.")
              "selected instructions are reparsed fresh from disk")
             (test-assert
              (string=
               (skill-metadata-description alpha)
               "The alpha skill.")
              "catalog metadata does not absorb later file changes"))
           (conversation-append-user-message
            conversation
            "Use alpha and beta and even write skill.load if useful.")
           (let ((outside
                   (skill-request-contributions
                    configuration
                    conversation)))
             (test-assert
              (equal
               (skill-tests--contribution-identifiers outside)
               '("skill-catalog"))
              "durable conversation text never selects a skill"))
           (call-with-skill-logical-turn
            (user-message-input-create
             :text "This text names alpha but does not select it.")
            (lambda ()
              (skill-record-steering-input
               (user-message-input-create
                :text "Steering also names beta without selecting it."))
              (test-assert
               (null *skill-logical-turn-selection-names*)
               "initial and steering text do not infer skill selection")
              (multiple-value-bind (metadata new-p)
                  (skill-select-for-logical-turn configuration "beta")
                (test-assert
                 (and new-p
                      (string=
                       (skill-metadata-name metadata)
                       "beta"))
                 "skill.load state selects an exact discovered name"))
              (skill-select-for-logical-turn configuration "alpha")
              (multiple-value-bind (metadata new-p)
                  (skill-select-for-logical-turn configuration "beta")
                (declare (ignore metadata))
                (test-assert
                 (not new-p)
                 "selecting one skill twice is idempotent"))
              (test-assert
               (equal *skill-logical-turn-selection-names*
                      '("beta" "alpha"))
               "multiple skills stack in deterministic selection order")
              (let* ((contributions
                       (skill-request-contributions
                        configuration
                        conversation))
                     (identifiers
                       (skill-tests--contribution-identifiers
                        contributions)))
                (test-assert
                 (equal identifiers
                        '("skill-catalog"
                          "skill-selected-alpha"
                          "skill-selected-beta"))
                 "selected skill instructions stack in catalog order")
                (test-assert
                 (search
                  "Replacement alpha instructions."
                  (context-contribution-instruction
                   (second contributions)))
                 "reverse tool selection still places alpha first")
                (test-assert
                 (search
                  "Beta instructions."
                  (context-contribution-instruction
                   (third contributions)))
                 "reverse tool selection still places beta second")
                (test-assert
                 (= (length (conversation-input-items conversation)) 1)
                 "skill context never appends durable conversation records"))
              (test-assert
               (null
                (skill-context-contributor
                 (make-instance
                  'request-context
                  :configuration configuration
                  :conversation conversation
                  :tool-namespaces #())))
               "restricted child requests without skill.load receive no catalog")
              (test-assert
               (null
                (skill-context-contributor
                 (make-instance
                  'request-context
                  :configuration configuration
                  :conversation conversation
                  :tool-namespaces
                  (skill-tests--tool-namespaces :load-p nil))))
               "a skill namespace without load does not enable the catalog")
              (test-assert
               (equal
                (skill-tests--contribution-identifiers
                 (skill-context-contributor
                  (make-instance
                   'request-context
                   :configuration configuration
                   :conversation conversation
                   :tool-namespaces
                   (skill-tests--tool-namespaces))))
                '("skill-catalog"
                  "skill-selected-alpha"
                  "skill-selected-beta"))
               "visible skill.load enables catalog and selected instructions")
              (test-assert
               (null
                (skill-context-contributor
                 (make-instance
                  'request-context
                  :configuration configuration
                  :conversation conversation
                  :tool-namespaces
                  (skill-tests--tool-namespaces)
                  :compaction-p t)))
               "skills are absent from compaction side requests")))
           (test-assert
            (not *skill-logical-turn-active-p*)
            "logical-turn selection is dynamically scoped")
           (context-runtime-reset)
           (let* ((delivery
                    (context-resolve-request
                     configuration
                     conversation
                     (skill-tests--tool-namespaces)))
                  (identifiers
                    (skill-tests--contribution-identifiers
                     (context-delivery-contributions delivery)))
                  (status (context-status conversation)))
             (test-assert
              (equal identifiers '("skill-catalog"))
              "ordinary context assembly exposes only the native catalog")
             (test-assert
              (search "skill-catalog" status)
              "/context makes the skill catalog contribution observable"))
           (let ((status (skill-status configuration)))
             (test-assert
              (and (search "alpha" status)
                   (search "beta" status)
                   (search "SKILL.sexp" status))
              "/skills exposes bounded native skill metadata")))
      (context-runtime-reset)
      (skill-tests--delete-root root)))
  nil)

(-> skill-tests--selection-failures-and-limits () null)
(defun skill-tests--selection-failures-and-limits ()
  "Test selected-file failures and aggregate instruction limits."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skills (merge-pathnames ".autolith/skills/" project))
         (lower-skills
           (merge-pathnames
            "skills/"
            (configuration-config-root base-configuration)))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-limits")))
    (unwind-protect
         (progn
           (let ((missing-path
                   (skill-tests--write
                    skills
                    "missing/SKILL.sexp"
                    (skill-tests--definition
                     "missing"
                     "This file disappears."
                     "Missing instructions."))))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Select explicitly.")
              (lambda ()
                (skill-select-for-logical-turn configuration "missing")
                (delete-file missing-path)
                (let* ((*skill-warning-character-limit* 96)
                       (contributions
                         (skill-request-contributions
                          configuration
                          conversation))
                       (warning
                         (skill-tests--contribution
                          contributions
                          "skill-warning-missing")))
                  (test-assert
                   (and warning
                        (<=
                         (length
                          (context-contribution-instruction warning))
                         *skill-warning-character-limit*))
                   "a disappearing selected file becomes a bounded warning")
                  (test-assert
                   (null
                    (skill-tests--contribution
                     contributions
                     "skill-selected-missing"))
                   "unreadable selected instructions are never applied")))))
           (let ((selected-path
                   (skill-tests--write
                    skills
                    "revealed/SKILL.sexp"
                    (skill-tests--definition
                     "revealed"
                     "The selected higher-precedence definition."
                     "Selected higher instructions."))))
             (skill-tests--write
              lower-skills
              "revealed/SKILL.sexp"
              (skill-tests--definition
               "revealed"
               "The hidden lower-precedence definition."
               "Hidden lower instructions."))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Select explicitly.")
              (lambda ()
                (skill-select-for-logical-turn configuration "revealed")
                (delete-file selected-path)
                (let ((contributions
                        (skill-request-contributions
                         configuration
                         conversation)))
                  (test-assert
                   (and
                    (skill-tests--contribution
                     contributions
                     "skill-warning-revealed")
                    (null
                     (skill-tests--contribution
                      contributions
                      "skill-selected-revealed"))
                    (not
                     (some
                      (lambda (contribution)
                        (search
                         "Hidden lower instructions."
                         (context-contribution-instruction contribution)))
                      contributions)))
                   "deletion cannot silently expose a lower-precedence skill")))))
           (skill-tests--write
            lower-skills
            "promoted/SKILL.sexp"
            (skill-tests--definition
             "promoted"
             "The initially selected lower definition."
             "Initially selected instructions."))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Select explicitly.")
            (lambda ()
              (skill-select-for-logical-turn configuration "promoted")
              (skill-tests--write
               skills
               "promoted/SKILL.sexp"
               (skill-tests--definition
                "promoted"
                "A newly added higher definition."
                "Replacement higher instructions."))
              (let ((contributions
                      (skill-request-contributions
                       configuration
                       conversation)))
                (test-assert
                 (and
                  (skill-tests--contribution
                   contributions
                   "skill-warning-promoted")
                  (null
                   (skill-tests--contribution
                    contributions
                    "skill-selected-promoted")))
                 "a newly winning higher-precedence path is not silently applied"))))
           (let ((replaced-path
                   (skill-tests--write
                    skills
                    "replaced/SKILL.sexp"
                    (skill-tests--definition
                     "replaced"
                     "The original inode."
                     "Original inode instructions."))))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Select explicitly.")
              (lambda ()
                (skill-select-for-logical-turn configuration "replaced")
                (delete-file replaced-path)
                (skill-tests--write
                 skills
                 "replaced/SKILL.sexp"
                 (skill-tests--definition
                  "replaced"
                  "A replacement inode."
                  "Replacement inode instructions."))
                (let ((contributions
                        (skill-request-contributions
                         configuration
                         conversation)))
                  (test-assert
                   (and
                    (skill-tests--contribution
                     contributions
                     "skill-warning-replaced")
                    (null
                     (skill-tests--contribution
                      contributions
                      "skill-selected-replaced")))
                   "same-path inode replacement invalidates the selection")))))
           (skill-tests--write
            skills
            "first/SKILL.sexp"
            (skill-tests--definition
             "first" "First aggregate skill." "First body."))
           (skill-tests--write
            skills
            "second/SKILL.sexp"
            (skill-tests--definition
             "second" "Second aggregate skill." "Second body."))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Select explicitly.")
            (lambda ()
              (skill-select-for-logical-turn configuration "first")
              (skill-select-for-logical-turn configuration "second")
              (let* ((catalog
                       (skill-catalog-for-configuration configuration))
                     (first
                       (skill-catalog-find catalog "first"))
                     (first-size
                       (length
                        (skill--explicit-instruction
                         first
                         (skill-metadata-read first))))
                     (*skill-selection-character-limit* first-size)
                     (contributions
                       (skill-request-contributions
                        configuration
                        conversation)))
                (test-assert
                 (skill-tests--contribution
                  contributions
                  "skill-selected-first")
                 "one skill may exactly fill the aggregate bound")
                (test-assert
                 (skill-tests--contribution
                  contributions
                  "skill-warning-second")
                 "a later skill beyond the aggregate bound becomes a warning")
                (test-assert
                 (null
                  (skill-tests--contribution
                   contributions
                   "skill-selected-second"))
                 "aggregate limits prevent excess instruction injection"))))
           (skill-tests--write
            skills
            "oversized/SKILL.sexp"
            (skill-tests--definition
             "oversized"
             "Deferred size failure."
             (make-string 256 :initial-element #\x)))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Select explicitly.")
            (lambda ()
              (skill-select-for-logical-turn configuration "oversized")
              (let* ((*skill-instruction-character-limit* 128)
                     (contributions
                       (skill-request-contributions
                        configuration
                        conversation))
                     (warning
                       (skill-tests--contribution
                        contributions
                        "skill-warning-oversized")))
                (test-assert
                 (and warning
                      (search
                       "could not be read"
                       (context-contribution-instruction warning)))
                 "a fresh selected-file size failure becomes an ephemeral warning")))))
      (skill-tests--delete-root root)))
  nil)


;;;; -- Skill Test Entry Point --

(-> test-skills () null)
(defun test-skills ()
  "Run native skill discovery, parsing, selection, and context tests."
  (skill-tests--discovery-and-precedence)
  (skill-tests--filesystem-boundaries)
  (skill-tests--native-parser-rejections)
  (skill-tests--roots-and-rendering)
  (skill-tests--scan-limits)
  (skill-tests--ephemeral-selection)
  (skill-tests--selection-failures-and-limits)
  nil)
