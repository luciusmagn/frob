(in-package #:autolith)

;;;; -- Durable Mutation Checks --

(defclass mutation-checker ()
  ()
  (:documentation "A strategy for checking live and source mutation states."))

(defclass standard-mutation-checker (mutation-checker)
  ()
  (:documentation "The production checker backed by ASDF and the repository check command."))

(defclass callback-mutation-checker (mutation-checker)
  ((active-callback
    :initarg :active-callback
    :reader callback-mutation-checker-active-callback
    :type function
    :documentation "The injected active-image check callback.")
   (source-callback
    :initarg :source-callback
    :reader callback-mutation-checker-source-callback
    :type function
    :documentation "The injected clean-source check callback."))
  (:documentation "A mutation checker backed by explicit boundary callbacks."))

(-> mutation-checker-check-active
    (mutation-checker configuration string)
    string)
(defgeneric mutation-checker-check-active (checker configuration definition-source)
  (:documentation
   "Check installed DEFINITION-SOURCE in the active image and return captured output."))

(-> mutation-checker-check-source
    (mutation-checker configuration list)
    string)
(defgeneric mutation-checker-check-source (checker configuration paths)
  (:documentation
   "Check the rebuildable source tree containing PATHS and return captured output."))

(defmethod mutation-checker-check-active
    ((checker standard-mutation-checker)
     (configuration configuration)
     (definition-source string))
  "Run Autolith's ASDF tests against the installed active-image definition."
  (declare (ignore checker configuration definition-source))
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*trace-output* stream))
      (asdf:test-system :autolith))))

(defmethod mutation-checker-check-source
    ((checker standard-mutation-checker)
     (configuration configuration)
     (paths list))
  "Run the repository check command after source changes to PATHS."
  (declare (ignore checker paths))
  (let ((check-pathname
          (merge-pathnames "script/check"
                           (configuration-source-root configuration))))
    (unless (probe-file check-pathname)
      (error 'source-mutation-error
             :message "The repository has no script/check command."
             :tool-name "self.commit"
             :pathname check-pathname))
    (uiop:run-program (list (namestring check-pathname))
                      :directory (configuration-source-root configuration)
                      :output :string
                      :error-output :output)))

(defmethod mutation-checker-check-active
    ((checker callback-mutation-checker)
     (configuration configuration)
     (definition-source string))
  "Invoke CHECKER's injected active-image callback."
  (or (funcall (callback-mutation-checker-active-callback checker)
               configuration
               definition-source)
      ""))

(defmethod mutation-checker-check-source
    ((checker callback-mutation-checker)
     (configuration configuration)
     (paths list))
  "Invoke CHECKER's injected clean-source callback."
  (or (funcall (callback-mutation-checker-source-callback checker)
               configuration
               paths)
      ""))

(-> tool-context-effective-mutation-checker (tool-context) mutation-checker)
(defun tool-context-effective-mutation-checker (context)
  "Return CONTEXT's checker or a production checker when none was injected."
  (or (tool-context-mutation-checker context)
      (make-instance 'standard-mutation-checker)))


;;;; -- Durable Mutation State --

(defclass durable-mutation ()
  ((identifier
    :initarg :identifier
    :reader durable-mutation-identifier
    :type non-empty-string
    :documentation "The stable identifier joining this mutation's journal records.")
   (target
    :initarg :target
    :reader durable-mutation-target
    :type non-empty-string
    :documentation "The semantic definition signature being changed.")
   (pathname
    :initarg :pathname
    :reader durable-mutation-pathname
    :type non-empty-string
    :documentation "The repository-relative source pathname being changed.")
   (previous-source
    :initarg :previous-source
    :reader durable-mutation-previous-source
    :type string
    :documentation "The complete source definition preceding this mutation.")
   (proposed-source
    :initarg :proposed-source
    :reader durable-mutation-proposed-source
    :type string
    :documentation "The complete proposed source definition.")
   (base-commit
    :initarg :base-commit
    :initform nil
    :reader durable-mutation-base-commit
    :type (option string)
    :documentation "The Git revision preceding the source mutation.")
   (phase
    :initarg :phase
    :accessor durable-mutation-phase
    :type keyword
    :documentation "The latest journaled transaction phase.")
   (git-commit
    :initform nil
    :accessor durable-mutation-git-commit
    :type (option string)
    :documentation "The Git commit making the mutation durable, when complete."))
  (:documentation "One checked live-to-source definition transaction."))

(defvar *durable-mutations* (make-hash-table :test #'equal)
  "Durable mutation transactions retained by the active Lisp image.")

(-> durable-mutation-journal
    (configuration durable-mutation &key (:detail t))
    list)
(defun durable-mutation-journal (configuration mutation &key detail)
  "Append MUTATION's current phase and optional DETAIL to its journal."
  (mutation-journal-append
   configuration
   (append
    (list :mutation
          :kind :durable-definition
          :id (durable-mutation-identifier mutation)
          :target (durable-mutation-target mutation)
          :pathname (durable-mutation-pathname mutation)
          :previous (durable-mutation-previous-source mutation)
          :proposed (durable-mutation-proposed-source mutation)
          :base-commit (durable-mutation-base-commit mutation)
          :result (durable-mutation-phase mutation))
    (when (durable-mutation-git-commit mutation)
      (list :git-commit (durable-mutation-git-commit mutation)))
    (when detail
      (list :detail (bounded-string detail :limit 2000))))))

(-> durable-mutation-transition-allowed-p (keyword keyword) boolean)
(defun durable-mutation-transition-allowed-p (from-phase to-phase)
  "Return true when FROM-PHASE may legally advance to TO-PHASE."
  (and (member
        to-phase
        (case from-phase
          (:pending '(:installed :failed :superseded))
          (:installed '(:checked :failed :superseded))
          (:checked '(:source-written :failed :superseded))
          (:source-written '(:durable :failed :superseded))
          (otherwise nil))
        :test #'eq)
       t))

(-> durable-mutation-transition
    (configuration durable-mutation keyword &key (:detail t) (:git-commit (option string)))
    durable-mutation)
(defun durable-mutation-transition
    (configuration mutation phase &key detail git-commit)
  "Validate, apply, and journal MUTATION's transition to PHASE."
  (unless (durable-mutation-transition-allowed-p
           (durable-mutation-phase mutation)
           phase)
      (error 'source-mutation-error
             :message (format nil "Invalid durable mutation transition from ~S to ~S."
                              (durable-mutation-phase mutation)
                              phase)
             :tool-name "self.persist-definition"
             :pathname (durable-mutation-pathname mutation)))
  (setf (durable-mutation-phase mutation) phase)
  (when git-commit
    (setf (durable-mutation-git-commit mutation) git-commit))
  (durable-mutation-journal configuration mutation :detail detail)
  mutation)

(-> durable-mutation-create
    (configuration list
     &key
     (:relative-pathname string)
     (:previous-source string)
     (:proposed-source string))
    durable-mutation)
(defun durable-mutation-create
    (configuration definition
     &key relative-pathname previous-source proposed-source)
  "Create and journal a pending durable transaction for DEFINITION."
  (let* ((target (definition-key definition))
         (mutation
           (make-instance 'durable-mutation
                          :identifier (make-identifier)
                          :target target
                          :pathname relative-pathname
                          :previous-source previous-source
                          :proposed-source proposed-source
                          :base-commit
                          (string-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           (self-git-command configuration '("rev-parse" "HEAD")))
                          :phase :pending)))
    (maphash
     (lambda (identifier existing)
       (declare (ignore identifier))
       (when (and (string= target (durable-mutation-target existing))
                  (member (durable-mutation-phase existing)
                          '(:pending :installed :checked :source-written)
                          :test #'eq))
         (durable-mutation-transition
          configuration
          existing
          :superseded
          :detail (format nil "Superseded by mutation ~A."
                          (durable-mutation-identifier mutation)))))
     *durable-mutations*)
    (setf (gethash (durable-mutation-identifier mutation)
                   *durable-mutations*)
          mutation)
    (durable-mutation-journal configuration mutation)
    mutation))

(-> durable-mutation-source-current-p
    (configuration durable-mutation)
    boolean)
(defun durable-mutation-source-current-p (configuration mutation)
  "Return true when MUTATION's complete proposed form is authoritative source."
  (handler-case
      (let* ((pathname
               (merge-pathnames (durable-mutation-pathname mutation)
                                (configuration-source-root configuration)))
             (proposed
               (self-read-form (durable-mutation-proposed-source mutation)
                               :read-eval nil)))
        (multiple-value-bind (source-form source)
            (source-find-definition pathname proposed)
          (declare (ignore source))
          (and (equal (source-form-form source-form) proposed) t)))
    (error ()
      nil)))

(-> durable-mutation--revision-source
    (configuration durable-mutation string)
    (option string))
(defun durable-mutation--revision-source (configuration mutation revision)
  "Return MUTATION's source file at REVISION, or NIL when Git cannot read it."
  (handler-case
      (self-git-command
       configuration
       (list "show"
             (format nil
                     "~A:~A"
                     revision
                     (durable-mutation-pathname mutation))))
    (error ()
      nil)))

(-> durable-mutation--source-contains-definition-p (string list) boolean)
(defun durable-mutation--source-contains-definition-p (source definition)
  "Return true when SOURCE contains a top-level form exactly equal to DEFINITION."
  (handler-case
      (and (definition-form-p definition)
           (some (lambda (source-form)
                   (equal (source-form-form source-form) definition))
                 (source-read-forms source))
           t)
    (error ()
      nil)))

(-> durable-mutation-committing-revision
    (configuration durable-mutation)
    (option string))
(defun durable-mutation-committing-revision (configuration mutation)
  "Return the first post-base commit containing MUTATION's proposed definition."
  (let ((base-commit (durable-mutation-base-commit mutation)))
    (when (non-empty-string-p base-commit)
      (handler-case
          (let* ((output
                   (self-git-command
                    configuration
                    (list "log"
                          "--format=%H"
                          "--reverse"
                          (format nil "~A..HEAD" base-commit)
                          "--"
                          (durable-mutation-pathname mutation))))
                 (commits
                   (remove-if-not
                    #'non-empty-string-p
                    (uiop:split-string output
                                       :separator '(#\Newline #\Return))))
                 (proposed
                   (self-read-form
                    (durable-mutation-proposed-source mutation)
                    :read-eval nil)))
            (loop for commit in commits
                  for source = (durable-mutation--revision-source
                                configuration mutation commit)
                  when (and source
                            (durable-mutation--source-contains-definition-p
                             source proposed))
                    return commit))
        (error ()
          nil)))))

(-> durable-mutation-mark-paths
    (configuration list string)
    list)
(defun durable-mutation-mark-paths (configuration paths git-commit)
  "Mark source-written mutations in committed PATHS durable at GIT-COMMIT."
  (let ((marked nil))
    (maphash
     (lambda (identifier mutation)
       (declare (ignore identifier))
       (when (and (eq (durable-mutation-phase mutation) :source-written)
                  (member (durable-mutation-pathname mutation)
                          paths
                          :test #'string=))
         (if (durable-mutation-source-current-p configuration mutation)
             (progn
               (durable-mutation-transition configuration
                                            mutation
                                            :durable
                                            :git-commit git-commit)
               (push mutation marked))
             (durable-mutation-transition
              configuration
              mutation
              :superseded
              :detail "Committed source no longer contains the proposed definition."))))
     *durable-mutations*)
    (nreverse marked)))

(-> durable-mutations-reconcile (configuration) list)
(defun durable-mutations-reconcile (configuration)
  "Finish journal state for source-written mutations already committed to Git."
  (let ((reconciled nil))
    (maphash
     (lambda (identifier mutation)
       (declare (ignore identifier))
       (when (eq (durable-mutation-phase mutation) :source-written)
         (let* ((pathname (durable-mutation-pathname mutation))
                (dirty
                  (self-git-command configuration
                                    (list "status" "--porcelain" "--" pathname)))
                (commit
                  (and (zerop (length dirty))
                       (durable-mutation-committing-revision
                        configuration mutation))))
           (when commit
             (if (durable-mutation-source-current-p configuration mutation)
                 (progn
                   (durable-mutation-transition configuration
                                                mutation
                                                :durable
                                                :git-commit commit
                                                :detail
                                                "Reconciled after Git committed before journal publication.")
                   (push mutation reconciled))
                 (durable-mutation-transition
                  configuration
                  mutation
                  :superseded
                  :detail "Committed source superseded the proposed definition."))))))
     *durable-mutations*)
    (nreverse reconciled)))

(-> mutation-journal-read-records (configuration) list)
(defun mutation-journal-read-records (configuration)
  "Read complete portable records from CONFIGURATION's mutation journal."
  (let ((pathname (configuration-journal-path configuration)))
    (if (probe-file pathname)
        (with-open-file (stream pathname
                                :direction :input
                                :external-format :utf-8)
          (let ((*read-eval* nil)
                (end-marker (cons nil nil))
                (records nil))
            (handler-case
                (loop for record = (read stream nil end-marker)
                      until (eq record end-marker)
                      do (push record records))
              (end-of-file ()
                nil)
              (reader-error (condition)
                (error 'source-mutation-error
                       :message (format nil "Malformed mutation journal: ~A" condition)
                       :tool-name "self.inspect"
                       :pathname pathname)))
            (nreverse records)))
        nil)))

(-> durable-mutation-journal-record-p (t) boolean)
(defun durable-mutation-journal-record-p (record)
  "Return true when RECORD claims to be a durable-definition journal state."
  (and (listp record)
       (eq (first record) :mutation)
       (eq (getf (rest record) :kind) :durable-definition)
       t))

(-> durable-mutation-record-p (configuration t) boolean)
(defun durable-mutation-record-p (configuration record)
  "Return true when RECORD is a valid durable-definition journal state.

The recorded pathname is either a startup overlay file or, for records
written before overlays existed, a tracked src/ file."
  (and (durable-mutation-journal-record-p record)
       (non-empty-string-p (getf (rest record) :id))
       (non-empty-string-p (getf (rest record) :target))
       (non-empty-string-p (getf (rest record) :pathname))
       (let ((pathname (merge-pathnames
                        (getf (rest record) :pathname)
                        (configuration-source-root configuration))))
         (or (uiop:subpathp pathname
                            (merge-pathnames
                             "src/"
                             (configuration-source-root configuration)))
             (uiop:subpathp pathname
                            (configuration-overlay-root configuration))
             ;; Journals may be replayed under a different data root, so a
             ;; foreign overlays directory is still a recognizable location.
             (find "overlays"
                   (pathname-directory pathname)
                   :test #'equal)))
       (stringp (getf (rest record) :previous))
       (stringp (getf (rest record) :proposed))
       (or (null (getf (rest record) :base-commit))
           (non-empty-string-p (getf (rest record) :base-commit)))
       (member (getf (rest record) :result)
               '(:pending :installed :checked :source-written
                 :durable :failed :superseded)
               :test #'eq)
       t))

(-> durable-mutation-record-matches-p (durable-mutation list) boolean)
(defun durable-mutation-record-matches-p (mutation properties)
  "Return true when PROPERTIES preserve MUTATION's immutable identity."
  (and (string= (durable-mutation-target mutation)
                (getf properties :target))
       (string= (durable-mutation-pathname mutation)
                (getf properties :pathname))
       (string= (durable-mutation-previous-source mutation)
                (getf properties :previous))
       (string= (durable-mutation-proposed-source mutation)
                (getf properties :proposed))
       (equal (durable-mutation-base-commit mutation)
              (getf properties :base-commit))
       t))

(-> durable-mutations-load (configuration) hash-table)
(defun durable-mutations-load (configuration)
  "Reconstruct durable mutation state from CONFIGURATION's append-only journal."
  (clrhash *durable-mutations*)
  (dolist (record (mutation-journal-read-records configuration))
    (when (durable-mutation-journal-record-p record)
      (unless (durable-mutation-record-p configuration record)
        (error 'source-mutation-error
               :message "A durable mutation journal record is invalid."
               :tool-name "self.inspect"
               :pathname (configuration-journal-path configuration)))
      (let* ((properties (rest record))
             (identifier (getf properties :id))
             (phase (getf properties :result))
             (existing (gethash identifier *durable-mutations*)))
        (if existing
            (progn
              (unless (and (durable-mutation-record-matches-p existing properties)
                           (durable-mutation-transition-allowed-p
                            (durable-mutation-phase existing)
                            phase))
                (error 'source-mutation-error
                       :message "A durable mutation journal transition is invalid."
                       :tool-name "self.inspect"
                       :pathname (configuration-journal-path configuration)))
              (setf (durable-mutation-phase existing) phase
                    (durable-mutation-git-commit existing)
                    (getf properties :git-commit)))
            (progn
              (unless (eq phase :pending)
                (error 'source-mutation-error
                       :message "A durable mutation journal begins after its pending state."
                       :tool-name "self.inspect"
                       :pathname (configuration-journal-path configuration)))
              (let ((mutation
                      (make-instance 'durable-mutation
                                     :identifier identifier
                                     :target (getf properties :target)
                                     :pathname (getf properties :pathname)
                                     :previous-source (getf properties :previous)
                                     :proposed-source (getf properties :proposed)
                                     :base-commit (getf properties :base-commit)
                                     :phase phase)))
                (setf (gethash identifier *durable-mutations*) mutation)))))))
  (durable-mutations-reconcile configuration)
  *durable-mutations*)


(-> durable-mutation--fallback-source (configuration list) (option string))
(defun durable-mutation--fallback-source (configuration definition)
  "Return DEFINITION's tracked source for restoration when no overlay exists."
  (handler-case
      (loop for tracked in (self-tracked-definitions configuration
                                                     (second definition))
            for form = (source-form-form
                        (tracked-definition-source-form tracked))
            when (eq (first form) (first definition))
              return (tracked-definition-source tracked))
    (error ()
      nil)))

(defmethod tool-execute ((tool self-persist-definition-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install, check, and persist one journaled definition to the overlay.

The tracked source repository is never patched; the definition is written to
an overlay file under the data root and loaded again at every startup."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((definition-source
             (tool-argument arguments "definition" :required t))
           (configuration (tool-context-configuration context))
           (definition (self-read-form definition-source :read-eval nil)))
      (unless (definition-form-p definition)
        (error 'source-mutation-error
               :message "The durable source is not a supported complete definition."
               :tool-name "self.persist-definition"
               :pathname (configuration-overlay-root configuration)))
      (let* ((target (definition-key definition))
             (previous-source (or (overlay-read configuration target)
                                  (durable-mutation--fallback-source
                                   configuration
                                   definition)
                                  ""))
             (overlay (overlay-pathname configuration target))
             (mutation
               (durable-mutation-create configuration
                                        definition
                                        :relative-pathname
                                        (namestring overlay)
                                        :previous-source previous-source
                                        :proposed-source definition-source))
             (checker (tool-context-effective-mutation-checker context)))
        (handler-case
            (progn
              (self-call-with-restarts
               (lambda ()
                 (self--install-definition definition definition-source))
               :restart-name (tool-argument arguments "restart")
               :restart-value-source (tool-argument arguments "restart-value"))
              (durable-mutation-transition configuration mutation :installed)
              (mutation-checker-check-active checker
                                             configuration
                                             definition-source)
              (durable-mutation-transition configuration mutation :checked)
              (overlay-write configuration target definition-source)
              (durable-mutation-transition configuration mutation
                                           :source-written)
              (durable-mutation-transition configuration mutation :durable)
              (tool-success
               (format nil
                       "Mutation ~A installed, checked, and persisted to ~
                        overlay ~A. Overlays load automatically at startup; ~
                        the tracked source repository was not modified."
                       (durable-mutation-identifier mutation)
                       (namestring overlay))))
          (error (condition)
            (when (and (member (durable-mutation-phase mutation)
                               '(:pending :installed :checked)
                               :test #'eq)
                       (non-empty-string-p previous-source))
              (handler-case
                  (self-restore-definition previous-source condition)
                (active-image-corruption (corruption)
                  (durable-mutation-transition
                   configuration
                   mutation
                   :failed
                   :detail
                   (format nil "Mutation failed: ~A~%Restoration failed: ~A"
                           condition
                           (active-image-corruption-restoration-condition
                            corruption)))
                  (error corruption))))
            (unless (member (durable-mutation-phase mutation)
                            '(:failed :superseded)
                            :test #'eq)
              (durable-mutation-transition configuration
                                           mutation
                                           :failed
                                           :detail condition))
            (error condition)))))))


;;;; -- Git Operations --

(-> self-git-command (configuration list &key (:ignore-error-status boolean)) string)
(defun self-git-command (configuration arguments &key ignore-error-status)
  "Run Git ARGUMENTS in CONFIGURATION's source root and return combined output."
  (uiop:run-program
   (append (list "git" "-C"
                 (namestring (configuration-source-root configuration)))
           arguments)
   :output :string
   :error-output :output
   :ignore-error-status ignore-error-status))

(defmethod tool-execute ((tool self-diff-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the active Autolith source diff."
  (declare (ignore tool arguments))
  (let ((output (self-git-command
                 (tool-context-configuration context)
                 '("diff" "--"))))
    (tool-success (if (non-empty-string-p output)
                      output
                      "The tracked worktree has no unstaged diff."))))

(-> self-validate-commit-title (string) string)
(defun self-validate-commit-title (title)
  "Return valid commit TITLE or signal a tool error."
  (unless (and (non-empty-string-p title)
               (< (length title) 72)
               (null (find #\Newline title))
               (null (find #\Return title)))
    (error 'tool-error
           :message "A commit title must be one non-empty line under 72 characters."
           :tool-name "self.commit"))
  title)

(-> self-require-source-workspace (configuration) null)
(defun self-require-source-workspace (configuration)
  "Require Autolith's tracked source tree to contain the current workspace."
  (unless (uiop:subpathp (configuration-working-directory configuration)
                         (configuration-source-root configuration))
    (error 'tool-error
           :message
           "self.commit only commits user-directed changes to Autolith's own source repository while that repository is the current workspace. Use ordinary workspace Git commands for an unrelated repository."
           :tool-name "self.commit"))
  nil)

(-> self-validate-commit-paths (configuration vector) list)
(defun self-validate-commit-paths (configuration paths)
  "Return validated repository-relative PATHS beneath CONFIGURATION's source root."
  (unless (plusp (length paths))
    (error 'tool-error
           :message "self.commit requires at least one explicit path."
           :tool-name "self.commit"))
  (let* ((source-root (configuration-source-root configuration))
         (launcher-root (merge-pathnames "bin/" source-root))
         (recovery-root (merge-pathnames "recovery/" source-root)))
    (loop for path across paths
          for pathname = (and (non-empty-string-p path)
                              (merge-pathnames path source-root))
          do (unless (and pathname (uiop:subpathp pathname source-root))
               (error 'tool-error
                      :message (format nil "Commit path ~S is outside the repository." path)
                      :tool-name "self.commit"))
             (when (or (uiop:subpathp pathname launcher-root)
                       (uiop:subpathp pathname recovery-root)
                       (string= (enough-namestring pathname source-root)
                                "script/build-recovery"))
               (error 'tool-error
                      :message (format nil
                                       "Normal self tools cannot commit stable artifact ~S."
                                       path)
                      :tool-name "self.commit"))
          collect (enough-namestring pathname source-root))))

(defmethod tool-execute ((tool self-commit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Check, commit, and durably journal the explicit paths supplied in ARGUMENTS."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((configuration (tool-context-configuration context))
           (title (self-validate-commit-title
                   (tool-argument arguments "title" :required t)))
           (raw-paths (tool-argument arguments "paths" :required t)))
      (self-require-source-workspace configuration)
      (unless (vectorp raw-paths)
        (error 'tool-error
               :message "self.commit paths must be a JSON array."
               :tool-name "self.commit"))
      (let ((paths (self-validate-commit-paths configuration raw-paths)))
        (handler-case
            (progn
              (mutation-checker-check-source
               (tool-context-effective-mutation-checker context)
               configuration
               paths)
              (self-git-command configuration
                                (append '("diff" "--check" "--") paths))
              (let* ((output
                       (self-git-command
                        configuration
                        (append (list "commit" "-m" title "--only" "--") paths)))
                     (commit
                       (string-trim
                        '(#\Space #\Tab #\Newline #\Return)
                        (self-git-command configuration '("rev-parse" "HEAD"))))
                     (durable-mutations
                       (durable-mutation-mark-paths configuration paths commit)))
                (mutation-journal-append
                 configuration
                 (list :mutation
                       :kind :commit
                       :paths paths
                       :title title
                       :git-commit commit
                       :durable-mutations
                       (mapcar #'durable-mutation-identifier durable-mutations)
                       :result :committed))
                (tool-success output)))
          (error (condition)
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :commit
                   :paths paths
                   :title title
                   :result :failed
                   :condition (bounded-string condition :limit 2000)))
            (error condition)))))))
