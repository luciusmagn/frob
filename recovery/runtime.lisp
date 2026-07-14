(in-package #:autolith)

;;;; -- Recovery State --

(defconstant +recovery-image-protocol-version+ 2
  "The launcher handshake version implemented by this pristine recovery image.")

(defclass recovery-context ()
  ((source-root
    :initarg :source-root
    :reader recovery-context-source-root
    :type pathname
    :documentation "The stable Autolith source checkout containing Git history.")
   (generation-root
    :initarg :generation-root
    :reader recovery-context-generation-root
    :type pathname
    :documentation "The retained generation directory.")
   (worktree-root
    :initarg :worktree-root
    :reader recovery-context-worktree-root
    :type pathname
    :documentation "The directory for exact-commit recovery worktrees.")
   (state-root
    :initarg :state-root
    :reader recovery-context-state-root
    :type pathname
    :documentation "The Autolith state directory containing selection and journals.")
   (current-pathname
    :initarg :current-pathname
    :reader recovery-context-current-pathname
    :type pathname
    :documentation "The atomically selected generation record."))
  (:documentation "Stable paths available to the pristine recovery image."))

(defclass recovery-generation ()
  ((identifier
    :initarg :identifier
    :reader recovery-generation-identifier
    :type string
    :documentation "The validated retained generation identifier.")
   (core-pathname
    :initarg :core-pathname
    :reader recovery-generation-core-pathname
    :type pathname
    :documentation "The contained saved core pathname.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader recovery-generation-manifest-pathname
    :type pathname
    :documentation "The validated manifest pathname.")
   (reconstruction-pathname
    :initarg :reconstruction-pathname
    :initform nil
    :reader recovery-generation-reconstruction-pathname
    :type (or null pathname)
    :documentation "The contained base-image reconstruction script, when present.")
   (git-commit
    :initarg :git-commit
    :reader recovery-generation-git-commit
    :type string
    :documentation "The exact source revision paired with the core.")
   (sbcl-version
    :initarg :sbcl-version
    :reader recovery-generation-sbcl-version
    :type string
    :documentation "The SBCL version that wrote the core.")
   (operating-system
    :initarg :operating-system
    :reader recovery-generation-operating-system
    :type string
    :documentation "The operating system type that wrote the core.")
   (operating-system-version
    :initarg :operating-system-version
    :reader recovery-generation-operating-system-version
    :type string
    :documentation "The operating system build that wrote the core.")
   (architecture
    :initarg :architecture
    :reader recovery-generation-architecture
    :type string
    :documentation "The machine architecture that wrote the core.")
   (created-at
    :initarg :created-at
    :reader recovery-generation-created-at
    :type integer
    :documentation "The universal time at which the generation was created."))
  (:documentation "A minimally validated generation visible to recovery."))

(defclass recovery-terminal-state ()
  ((settings
    :initarg :settings
    :reader recovery-terminal-state-settings
    :type (or null string)
    :documentation "The trusted STTY settings captured before a retained core starts."))
  (:documentation "Terminal state restored between retained-generation attempts."))

(serapeum:-> recovery-context-create (pathname) recovery-context)
(defun recovery-context-create (source-root)
  "Return recovery context rooted at SOURCE-ROOT and XDG user directories."
  (let* ((home (user-homedir-pathname))
         (data-home
           (uiop:ensure-directory-pathname
            (or (uiop:getenv "XDG_DATA_HOME")
                (merge-pathnames ".local/share/" home))))
         (state-home
           (uiop:ensure-directory-pathname
            (or (uiop:getenv "XDG_STATE_HOME")
                (merge-pathnames ".local/state/" home))))
         (state-root (merge-pathnames "autolith/" state-home)))
    (make-instance
     'recovery-context
     :source-root (uiop:ensure-directory-pathname source-root)
     :generation-root (merge-pathnames "autolith/generations/" data-home)
     :worktree-root (merge-pathnames "autolith/recovery-worktrees/" data-home)
     :state-root state-root
     :current-pathname (merge-pathnames "current-generation.sexp" state-root))))

(serapeum:-> recovery-terminal-state-capture () recovery-terminal-state)
(defun recovery-terminal-state-capture ()
  "Capture the current terminal settings without failing on non-terminal input."
  (make-instance
   'recovery-terminal-state
   :settings
   (handler-case
       (let ((settings
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (uiop:run-program '("stty" "-g")
                                  :input :interactive
                                  :output :string
                                  :error-output :output))))
         (and (plusp (length settings)) settings))
     (error ()
       nil))))

(serapeum:-> recovery-terminal-state-restore (recovery-terminal-state) null)
(defun recovery-terminal-state-restore (state)
  "Restore trusted terminal STATE and disable presentation modes left by a failed core."
  (let ((settings (recovery-terminal-state-settings state)))
    (when settings
      (ignore-errors
        (uiop:run-program (list "stty" settings)
                          :input :interactive
                          :output nil
                          :error-output nil)))
    (ignore-errors
      (with-open-file (stream #P"/dev/tty"
                              :direction :output
                              :if-does-not-exist nil
                              :external-format :utf-8)
        (when stream
          (format stream "~C[?2004l~C[?25h~C[0m"
                  #\Escape #\Escape #\Escape)
          (finish-output stream)))))
  nil)


;;;; -- Safe Data and Presentation --

(serapeum:-> recovery-sanitize-text (t) string)
(defun recovery-sanitize-text (value)
  "Return VALUE as one terminal-safe line without C0, C1, or escape controls."
  (let ((text (if (stringp value) value (princ-to-string value))))
    (map 'string
         (lambda (character)
           (let ((code (char-code character)))
             (if (or (< code 32)
                     (= code 127)
                     (<= 128 code 159))
                 #\Space
                 character)))
         text)))

(serapeum:-> recovery-read-form (pathname) t)
(defun recovery-read-form (pathname)
  "Read exactly one portable form from PATHNAME with reader evaluation disabled."
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (end-marker (cons nil nil)))
      (let ((form (read stream t nil)))
        (unless (eq (read stream nil end-marker) end-marker)
          (error "Recovery record ~A contains trailing forms." pathname))
        form))))

(serapeum:-> recovery-identifier-p (t) boolean)
(defun recovery-identifier-p (value)
  "Return true for a bounded path-component-safe generation identifier."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              value)
       t))

(serapeum:-> recovery-git-commit-p (t) boolean)
(defun recovery-git-commit-p (value)
  "Return true when VALUE is one full hexadecimal Git object identifier."
  (and (stringp value)
       (= (length value) 40)
       (every (lambda (character) (digit-char-p character 16)) value)
       t))

(serapeum:-> recovery-read-journal-records (pathname) list)
(defun recovery-read-journal-records (pathname)
  "Read complete journal forms from PATHNAME, ignoring an incomplete final form."
  (if (probe-file pathname)
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (let ((*read-eval* nil)
              (end-marker (cons nil nil))
              (records nil))
          (handler-case
              (loop for record = (read stream nil end-marker)
                    until (eq record end-marker)
                    do (push record records))
            (end-of-file ()
              nil))
          (nreverse records)))
      nil))

(serapeum:-> recovery-report-mutations (recovery-context) null)
(defun recovery-report-mutations (context)
  "Print pending durable mutation identities from CONTEXT's journal."
  (let ((latest (make-hash-table :test #'equal))
        (pathname (merge-pathnames "mutations.sexp"
                                   (recovery-context-state-root context))))
    (handler-case
        (progn
          (dolist (record (recovery-read-journal-records pathname))
            (when (and (listp record)
                       (eq (first record) :mutation)
                       (eq (getf (rest record) :kind) :durable-definition)
                       (recovery-identifier-p (getf (rest record) :id)))
              (setf (gethash (getf (rest record) :id) latest) record)))
          (let ((pending
                  (loop for record being the hash-values of latest
                        unless (member (getf (rest record) :result)
                                       '(:durable :failed :superseded)
                                       :test #'eq)
                          collect record)))
            (when pending
              (format *error-output* "Pending durable mutations: ~D~%"
                      (length pending))
              (dolist (record pending)
                (format *error-output* "  ~A  ~A  ~A~%"
                        (recovery-sanitize-text (getf (rest record) :id))
                        (recovery-sanitize-text (getf (rest record) :result))
                        (recovery-sanitize-text (getf (rest record) :pathname)))))))
      (error (condition)
        (format *error-output* "Could not inspect the mutation journal: ~A~%"
                (recovery-sanitize-text condition)))))
  nil)


;;;; -- Generation Validation --

(serapeum:-> recovery-manifest-pathname
    (recovery-context string)
    pathname)
(defun recovery-manifest-pathname (context identifier)
  "Return IDENTIFIER's contained manifest pathname in CONTEXT."
  (unless (recovery-identifier-p identifier)
    (error "Invalid generation identifier ~A."
           (recovery-sanitize-text identifier)))
  (merge-pathnames
   "manifest.sexp"
   (merge-pathnames (format nil "~A/" identifier)
                    (recovery-context-generation-root context))))

(serapeum:-> recovery-load-generation
    (recovery-context pathname &key (:expected-identifier t))
    recovery-generation)
(defun recovery-load-generation (context pathname &key expected-identifier)
  "Load and validate one generation at PATHNAME inside CONTEXT."
  (unless (and (uiop:subpathp pathname
                              (recovery-context-generation-root context))
               (probe-file pathname))
    (error "Generation manifest is absent or outside the retained root."))
  (let* ((form (recovery-read-form pathname))
         (properties (and (listp form) (rest form)))
         (identifier (and properties (getf properties :id)))
         (core-value (and properties (getf properties :core)))
         (commit (and properties (getf properties :git-commit)))
         (version (and properties (getf properties :version)))
         (reconstruction-value
           (and properties (getf properties :reconstruction)))
         (directory (uiop:pathname-directory-pathname pathname))
         (core-pathname (and (stringp core-value) (pathname core-value)))
         (reconstruction-pathname
           (and (stringp reconstruction-value)
                (pathname reconstruction-value))))
    (unless (and (listp form)
                 (eq (first form) :generation)
                 (member version '(1 2))
                 (recovery-identifier-p identifier)
                 (or (null expected-identifier)
                     (string= identifier expected-identifier))
                 (string= identifier
                          (car (last (pathname-directory directory))))
                 core-pathname
                 (uiop:subpathp core-pathname directory)
                 (or (= version 1)
                     (and reconstruction-pathname
                          (uiop:subpathp reconstruction-pathname directory)
                          (probe-file reconstruction-pathname)))
                 (recovery-git-commit-p commit)
                 (stringp (getf properties :sbcl-version))
                 (stringp (getf properties :operating-system))
                 (stringp (getf properties :operating-system-version))
                 (stringp (getf properties :architecture))
                 (integerp (getf properties :created-at)))
      (error "Invalid retained generation manifest at ~A."
             (recovery-sanitize-text pathname)))
    (make-instance
     'recovery-generation
     :identifier identifier
     :core-pathname core-pathname
     :manifest-pathname pathname
     :reconstruction-pathname reconstruction-pathname
     :git-commit commit
     :sbcl-version (getf properties :sbcl-version)
     :operating-system (getf properties :operating-system)
     :operating-system-version (getf properties :operating-system-version)
     :architecture (getf properties :architecture)
     :created-at (getf properties :created-at))))

(serapeum:-> recovery-generation-compatible-p (recovery-generation) boolean)
(defun recovery-generation-compatible-p (generation)
  "Return true when GENERATION has a plausible core for this exact SBCL host."
  (handler-case
      (and (string= (recovery-generation-sbcl-version generation)
                    (lisp-implementation-version))
           (string= (recovery-generation-operating-system generation)
                    (software-type))
           (string= (recovery-generation-operating-system-version generation)
                    (software-version))
           (string= (recovery-generation-architecture generation)
                    (machine-type))
           (probe-file (recovery-generation-core-pathname generation))
           (with-open-file (stream (recovery-generation-core-pathname generation)
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
             (> (file-length stream) 1048576))
           t)
    (error ()
      nil)))

(serapeum:-> recovery-generation-list (recovery-context) list)
(defun recovery-generation-list (context)
  "Return valid retained generations in CONTEXT, newest first."
  (let ((generations nil)
        (root (recovery-context-generation-root context)))
    (when (probe-file root)
      (dolist (directory (uiop:subdirectories root))
        (let ((pathname (merge-pathnames "manifest.sexp" directory)))
          (when (probe-file pathname)
            (handler-case
                (push (recovery-load-generation context pathname) generations)
              (error (condition)
                (format *error-output* "Skipping invalid manifest ~A: ~A~%"
                        (recovery-sanitize-text pathname)
                        (recovery-sanitize-text condition))))))))
    (sort generations #'> :key #'recovery-generation-created-at)))

(serapeum:-> recovery-selected-generation (recovery-context) recovery-generation)
(defun recovery-selected-generation (context)
  "Return the generation named by CONTEXT's atomic selection record."
  (let ((pathname (recovery-context-current-pathname context)))
    (unless (probe-file pathname)
      (error "No retained generation is selected."))
    (let* ((record (recovery-read-form pathname))
           (identifier
             (and (listp record)
                  (eq (first record) :current-generation)
                  (getf (rest record) :id)))
           (manifest
             (and (listp record)
                  (eq (first record) :current-generation)
                  (getf (rest record) :manifest))))
      (unless (and (recovery-identifier-p identifier)
                   (stringp manifest)
                   (uiop:subpathp (pathname manifest)
                                  (recovery-context-generation-root context)))
        (error "The selected-generation record is invalid."))
      (recovery-load-generation context
                                (pathname manifest)
                                :expected-identifier identifier))))

(serapeum:-> recovery-newest-compatible-generation
    (recovery-context)
    recovery-generation)
(defun recovery-newest-compatible-generation (context)
  "Return CONTEXT's newest valid generation compatible with this host."
  (or (find-if #'recovery-generation-compatible-p
               (recovery-generation-list context))
      (error "No compatible retained generation is available.")))

(serapeum:-> recovery-selected-generation-or-fallback
    (recovery-context)
    recovery-generation)
(defun recovery-selected-generation-or-fallback (context)
  "Return CONTEXT's selected generation or its newest compatible fallback."
  (let ((selected
          (handler-case
              (recovery-selected-generation context)
            (error (condition)
              (format *error-output*
                      "Could not load the selected generation: ~A~%"
                      (recovery-sanitize-text condition))
              nil))))
    (if (and selected (recovery-generation-compatible-p selected))
        selected
        (progn
          (if selected
              (format *error-output*
                      "Selected generation ~A is incompatible or corrupt.~%"
                      (recovery-sanitize-text
                       (recovery-generation-identifier selected)))
              (format *error-output*
                      "The selected-generation record is unusable.~%"))
          (format *error-output*
                  "Using the newest compatible retained generation.~%")
          (recovery-newest-compatible-generation context)))))

(serapeum:-> recovery-print-generations (recovery-context) null)
(defun recovery-print-generations (context)
  "Print retained generation identifiers, revisions, and replay scripts."
  (let ((generations (recovery-generation-list context)))
    (if generations
        (dolist (generation generations)
          (format t "~A  ~A  source ~A~@[~%  replay ~A~]~%"
                  (recovery-sanitize-text
                   (recovery-generation-identifier generation))
                  (if (recovery-generation-compatible-p generation)
                      "compatible"
                      "incompatible")
                  (recovery-sanitize-text
                   (recovery-generation-git-commit generation))
                  (and (recovery-generation-reconstruction-pathname generation)
                       (recovery-sanitize-text
                        (namestring
                         (recovery-generation-reconstruction-pathname
                          generation))))))
        (format t "No retained generations exist.~%")))
  nil)


;;;; -- Crash Context --

(serapeum:-> recovery-clear-reconnection-environment () null)
(defun recovery-clear-reconnection-environment ()
  "Remove retained crash reconnection metadata from the recovery environment."
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_CONVERSATION_ID")
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
  nil)

(serapeum:-> recovery-report-crash-capsule
    (recovery-context t)
    (or null string))
(defun recovery-report-crash-capsule (context capsule)
  "Report and publish one valid CAPSULE, returning its normalized pathname."
  (when (and (stringp capsule) (plusp (length capsule)))
    (handler-case
        (let* ((capsule-pathname (pathname capsule))
               (crash-root (merge-pathnames "crashes/"
                                            (recovery-context-state-root context))))
          (unless (and (uiop:subpathp capsule-pathname crash-root)
                       (probe-file capsule-pathname))
            (error "The crash capsule is absent or outside private Autolith state."))
          (let* ((record (recovery-read-form capsule-pathname))
                 (properties (and (listp record) (rest record)))
                 (conversation-id (and properties
                                       (getf properties :conversation-id)))
                 (rendered-sequence (and properties
                                         (getf properties :rendered-sequence))))
            (unless (eq (first record) :crash)
              (error "The crash capsule has an invalid header."))
            (recovery-clear-reconnection-environment)
            (format *error-output*
                    "Crash capsule: ~A~%Condition: ~A~%Conversation: ~A~%"
                    (recovery-sanitize-text capsule-pathname)
                    (recovery-sanitize-text
                     (or (getf properties :condition) "unknown"))
                    (recovery-sanitize-text (or conversation-id "unknown")))
            (when (and (stringp conversation-id)
                       (recovery-identifier-p conversation-id))
              (sb-posix:setenv "AUTOLITH_RECOVERY_CONVERSATION_ID"
                               conversation-id
                               1))
            (when (and (integerp rendered-sequence)
                       (not (minusp rendered-sequence)))
              (sb-posix:setenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE"
                               (write-to-string rendered-sequence)
                               1))
            (namestring capsule-pathname)))
      (error (condition)
        (format *error-output* "Could not read crash capsule ~A: ~A~%"
                (recovery-sanitize-text capsule)
                (recovery-sanitize-text condition))
        nil))))

(serapeum:-> recovery-read-crash-pointer
    (recovery-context)
    (or null string))
(defun recovery-read-crash-pointer (context)
  "Return the contained capsule named by this launcher's current pointer."
  (let ((pointer-value (uiop:getenv "AUTOLITH_CRASH_POINTER")))
    (when (and (stringp pointer-value) (plusp (length pointer-value)))
      (let* ((pointer-pathname (pathname pointer-value))
             (pointer-root (merge-pathnames "crash-pointers/"
                                            (recovery-context-state-root context)))
             (crash-root (merge-pathnames "crashes/"
                                          (recovery-context-state-root context))))
        (unless (uiop:subpathp pointer-pathname pointer-root)
          (error "The crash pointer is outside private Autolith state."))
        (when (probe-file pointer-pathname)
          (with-open-file (stream pointer-pathname
                                  :direction :input
                                  :external-format :utf-8)
            (let ((capsule (read-line stream nil nil))
                  (trailing-line (read-line stream nil nil)))
              (unless (and (stringp capsule)
                           (plusp (length capsule))
                           (<= (length capsule) 4096)
                           (null trailing-line))
                (error "The crash pointer is not one bounded pathname line."))
              (let ((capsule-pathname (pathname capsule)))
                (unless (and (uiop:subpathp capsule-pathname crash-root)
                             (probe-file capsule-pathname))
                  (error "The crash pointer names an invalid capsule."))
                (namestring capsule-pathname)))))))))

(serapeum:-> recovery-refresh-crash-context
    (recovery-context (or null string))
    (or null string))
(defun recovery-refresh-crash-context (context current-capsule)
  "Publish a newer capsule from this launcher's pointer, when one exists."
  (handler-case
      (let ((pointer-capsule (recovery-read-crash-pointer context)))
        (cond
          ((null pointer-capsule)
           current-capsule)
          ((and current-capsule (string= pointer-capsule current-capsule))
           current-capsule)
          (t
           (format *error-output*
                   "Refreshing recovery context from the latest crash capsule.~%")
           (or (recovery-report-crash-capsule context pointer-capsule)
               current-capsule))))
    (error (condition)
      (format *error-output* "Could not refresh the crash pointer: ~A~%"
              (recovery-sanitize-text condition))
      current-capsule)))

(serapeum:-> recovery-report-crash
    (recovery-context
     &key (:status t) (:capsule t) (:original-arguments list))
    (or null string))
(defun recovery-report-crash
    (context &key status capsule (original-arguments nil))
  "Report bounded crash context and publish safe reconnection metadata."
  (recovery-clear-reconnection-environment)
  (when status
    (format *error-output* "Active Autolith exited with status ~A.~%"
            (recovery-sanitize-text status)))
  (let ((reported-capsule (recovery-report-crash-capsule context capsule)))
    (when original-arguments
      (format *error-output* "Original arguments: ~{~A~^ ~}~%"
              (mapcar #'recovery-sanitize-text original-arguments)))
    (recovery-report-mutations context)
    reported-capsule))


;;;; -- Exact Source and Core Boot --

(serapeum:-> recovery-source-worktree
    (recovery-context recovery-generation)
    pathname)
(defun recovery-source-worktree (context generation)
  "Return a clean detached worktree for GENERATION's exact source revision."
  (let* ((identifier (recovery-generation-identifier generation))
         (commit (recovery-generation-git-commit generation))
         (worktree
           (merge-pathnames (format nil "~A/" identifier)
                            (recovery-context-worktree-root context))))
    (if (probe-file worktree)
        (let ((actual
                (string-trim
                 '(#\Space #\Tab #\Newline #\Return)
                 (uiop:run-program
                  (list "git" "-C" (namestring worktree) "rev-parse" "HEAD")
                  :output :string
                  :error-output :output)))
              (status
                (uiop:run-program
                 (list "git" "-C" (namestring worktree) "status" "--porcelain")
                 :output :string
                 :error-output :output)))
          (unless (and (string= actual commit) (zerop (length status)))
            (error "Recovery worktree ~A is not clean at commit ~A."
                   (recovery-sanitize-text worktree)
                   (recovery-sanitize-text commit))))
        (progn
          (ensure-directories-exist
           (recovery-context-worktree-root context))
          (uiop:run-program
           (list "git"
                 "-C" (namestring (recovery-context-source-root context))
                 "worktree" "add" "--detach"
                 (namestring worktree)
                 commit)
           :output :string
           :error-output :output)))
    worktree))

(serapeum:-> recovery-boot-generation
    (recovery-context recovery-generation list)
    integer)
(defun recovery-boot-generation (context generation forwarded-arguments)
  "Boot GENERATION with FORWARDED-ARGUMENTS and return its process status."
  (unless (recovery-generation-compatible-p generation)
    (error "Generation ~A is incompatible with this SBCL runtime."
           (recovery-sanitize-text
            (recovery-generation-identifier generation))))
  (let* ((worktree (recovery-source-worktree context generation))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (sb-posix:setenv "AUTOLITH_SOURCE_ROOT" (namestring worktree) 1)
    (sb-posix:setenv "AUTOLITH_RECOVERED" "1" 1)
    (let ((process
            (uiop:launch-program
             (append
              (list sbcl-command
                    "--noinform"
                    "--core"
                    (namestring (recovery-generation-core-pathname generation))
                    "--end-runtime-options")
              forwarded-arguments)
             :directory worktree
             :input :interactive
             :output :interactive
             :error-output :interactive
             :wait nil)))
      (uiop:wait-process process))))

(serapeum:-> recovery-boot-with-fallback
    (recovery-context recovery-generation list
     &key (:capsule (or null string)))
    integer)
(defun recovery-boot-with-fallback
    (context selected forwarded-arguments &key capsule)
  "Boot SELECTED, falling back to other compatible generations after fatal exits."
  (let ((candidates
          (cons selected
                (remove (recovery-generation-identifier selected)
                        (recovery-generation-list context)
                        :key #'recovery-generation-identifier
                        :test #'string=)))
        (terminal-state (recovery-terminal-state-capture))
        (current-capsule
          (recovery-refresh-crash-context context capsule)))
    (loop for remaining on candidates
          for generation = (first remaining)
          do (if (recovery-generation-compatible-p generation)
                 (let ((status nil)
                       (completed-p nil))
                   (unwind-protect
                        (handler-case
                            (setf status
                                  (recovery-boot-generation
                                   context
                                   generation
                                   forwarded-arguments)
                                  completed-p t)
                          (error (condition)
                            (format *error-output*
                                    "Could not boot generation ~A: ~A~%"
                                    (recovery-sanitize-text
                                     (recovery-generation-identifier generation))
                                    (recovery-sanitize-text condition))))
                     (recovery-terminal-state-restore terminal-state))
                   (when (and completed-p
                              (member status '(0 64 130 143) :test #'=))
                     (return-from recovery-boot-with-fallback status))
                   (when completed-p
                     (format *error-output*
                             "Generation ~A exited with status ~D; trying fallback.~%"
                             (recovery-sanitize-text
                              (recovery-generation-identifier generation))
                             status))
                   (when (rest remaining)
                     (setf current-capsule
                           (recovery-refresh-crash-context context
                                                           current-capsule))))
                 (format *error-output*
                         "Skipping incompatible generation ~A.~%"
                         (recovery-sanitize-text
                          (recovery-generation-identifier generation)))))
    (error "No retained generation could be booted.")))


;;;; -- Argument Parsing and Entry --

(serapeum:-> recovery-parse-arguments (list) *)
(defun recovery-parse-arguments (arguments)
  "Return recovery options and forwarded application arguments from ARGUMENTS."
  (let ((generation nil)
        (list-p nil)
        (status nil)
        (capsule nil)
        (forwarded nil)
        (original-arguments nil)
        (remaining arguments))
    (loop while remaining
          for argument = (pop remaining)
          do (cond
               ((string= argument "--")
                (setf forwarded remaining
                      remaining nil))
               ((string= argument "--list")
                (setf list-p t))
               ((string= argument "--generation")
                (setf generation
                      (or (pop remaining)
                          (error "--generation requires an identifier."))))
               ((string= argument "--status")
                (setf status
                      (or (pop remaining)
                          (error "--status requires an exit code."))))
               ((string= argument "--capsule")
                (setf capsule
                      (or (pop remaining)
                          (error "--capsule requires a pathname."))))
               ((string= argument "--original-argument")
                (push (or (pop remaining)
                          (error "--original-argument requires a value."))
                      original-arguments))
               (t
                (setf forwarded (cons argument remaining)
                      remaining nil))))
    (values generation
            list-p
            status
            capsule
            forwarded
            (nreverse original-arguments))))

(serapeum:-> recovery-run (list) integer)
(defun recovery-run (arguments)
  "Run pristine recovery using complete command-line ARGUMENTS."
  (let ((source-root
          (uiop:ensure-directory-pathname
           (or (first arguments)
               (error "The recovery image needs the source root.")))))
    (if (equal (rest arguments) '("--probe"))
        (progn
          (let ((*print-readably* t))
            (prin1 (list :recovery-probe
                         :version +recovery-image-protocol-version+
                         :sbcl-version (lisp-implementation-version)
                         :operating-system (software-type)
                         :operating-system-version (software-version)
                         :architecture (machine-type)))
            (terpri)
            (finish-output))
          0)
        (let ((context (recovery-context-create source-root)))
          (multiple-value-bind
              (generation list-p status capsule forwarded original-arguments)
              (recovery-parse-arguments (rest arguments))
            (if list-p
                (progn
                  (recovery-print-generations context)
                  0)
                (let ((reported-capsule
                        (recovery-report-crash
                         context
                         :status status
                         :capsule capsule
                         :original-arguments original-arguments)))
                  (let ((selected
                          (if generation
                              (recovery-load-generation
                               context
                               (recovery-manifest-pathname context generation)
                               :expected-identifier generation)
                              (recovery-selected-generation-or-fallback
                               context))))
                    (recovery-boot-with-fallback context
                                                 selected
                                                 forwarded
                                                 :capsule reported-capsule)))))))))

(serapeum:-> recovery-main () null)
(defun recovery-main ()
  "Run the pristine recovery core and terminate with its explicit status."
  (sb-ext:disable-debugger)
  (restart-case
      (handler-case
          (uiop:quit (recovery-run (uiop:command-line-arguments)))
        (error (condition)
          (format *error-output* "Recovery could not continue: ~A~%"
                  (recovery-sanitize-text condition))
          (uiop:quit 1)))
    (abort ()
      :report "Exit the pristine Autolith recovery image."
      (uiop:quit 1)))
  nil)

(serapeum:-> recovery-image-save (pathname) null)
(defun recovery-image-save (pathname)
  "Save the current minimal image to PATHNAME with RECOVERY-MAIN as its entry."
  (ensure-directories-exist pathname)
  (sb-ext:save-lisp-and-die (namestring pathname)
                            :toplevel #'recovery-main
                            :executable nil
                            :purify t
                            :compression nil))
