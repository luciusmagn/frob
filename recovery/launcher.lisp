#!/usr/bin/env -S sbcl --script

(require :asdf)
(require :sb-posix)

(handler-case
    (let* ((arguments (uiop:command-line-arguments))
       (source-root (uiop:ensure-directory-pathname
                     (or (first arguments)
                         (error "The recovery launcher needs the source root."))))
       (recovery-arguments (rest arguments))
       (home (user-homedir-pathname))
       (data-home (uiop:ensure-directory-pathname
                   (or (uiop:getenv "XDG_DATA_HOME")
                       (merge-pathnames ".local/share/" home))))
       (state-home (uiop:ensure-directory-pathname
                    (or (uiop:getenv "XDG_STATE_HOME")
                        (merge-pathnames ".local/state/" home))))
       (generation-root (merge-pathnames "frob/generations/" data-home))
       (worktree-root (merge-pathnames "frob/recovery-worktrees/" data-home))
       (current-pathname
         (merge-pathnames "frob/current-generation.sexp" state-home)))
  (labels ((read-form (pathname)
             "Read one portable recovery form from PATHNAME."
             (with-open-file (stream pathname
                                     :direction :input
                                     :external-format :utf-8)
               (let ((*read-eval* nil))
                 (read stream t nil))))

           (manifest-pathname-for-id (identifier)
             "Return the retained manifest pathname for IDENTIFIER."
             (merge-pathnames
              "manifest.sexp"
              (merge-pathnames (format nil "~A/" identifier)
                               generation-root)))

           (selected-manifest-pathname ()
             "Return the manifest named by the stable current-generation record."
             (unless (probe-file current-pathname)
               (error "No retained generation is selected."))
             (let* ((record (read-form current-pathname))
                    (identifier (and (listp record)
                                     (eq (first record) :current-generation)
                                     (getf (rest record) :id)))
                    (manifest (and (listp record)
                                   (eq (first record) :current-generation)
                                   (getf (rest record) :manifest))))
               (unless (and (stringp identifier)
                            (stringp manifest)
                            (uiop:subpathp (pathname manifest) generation-root)
                            (probe-file manifest))
                 (error "The selected-generation record is invalid."))
               (unless (string= identifier
                                (getf (rest (load-manifest manifest)) :id))
                 (error "The selected generation identifier does not match its manifest."))
               (pathname manifest)))

           (load-manifest (pathname)
             "Load and minimally validate a retained generation manifest."
             (let ((manifest (read-form pathname)))
               (unless (and (listp manifest)
                            (eq (first manifest) :generation)
                            (= (or (getf (rest manifest) :version) 0) 1)
                            (stringp (getf (rest manifest) :id))
                            (stringp (getf (rest manifest) :core))
                            (stringp (getf (rest manifest) :git-commit)))
                 (error "Invalid retained generation manifest at ~A." pathname))
               manifest))

           (compatible-manifest-p (manifest)
             "Return true when MANIFEST matches the running SBCL platform."
             (and (string= (or (getf (rest manifest) :sbcl-version) "")
                           (lisp-implementation-version))
                  (string= (or (getf (rest manifest) :operating-system) "")
                           (software-type))
                  (string= (or (getf (rest manifest) :operating-system-version) "")
                           (software-version))
                  (string= (or (getf (rest manifest) :architecture) "")
                           (machine-type))
                  (probe-file (getf (rest manifest) :core))
                  t))

           (list-generations ()
             "Print retained generation identifiers, commits, and compatibility."
             (if (probe-file generation-root)
                 (dolist (directory (uiop:subdirectories generation-root))
                   (let ((pathname (merge-pathnames "manifest.sexp" directory)))
                     (when (probe-file pathname)
                       (handler-case
                           (let ((manifest (load-manifest pathname)))
                             (format t "~A  ~A  commit ~A~%"
                                     (getf (rest manifest) :id)
                                     (if (compatible-manifest-p manifest)
                                         "compatible"
                                         "incompatible")
                                     (getf (rest manifest) :git-commit)))
                         (error (condition)
                           (format *error-output*
                                   "Skipping invalid manifest ~A: ~A~%"
                                   pathname condition))))))
                 (format t "No retained generations exist.~%")))

           (ensure-source-worktree (manifest)
             "Return a detached source worktree at MANIFEST's exact Git revision."
             (let* ((identifier (getf (rest manifest) :id))
                    (commit (getf (rest manifest) :git-commit))
                    (worktree (merge-pathnames (format nil "~A/" identifier)
                                               worktree-root)))
               (if (probe-file worktree)
                   (let ((actual
                           (string-trim
                            '(#\Space #\Tab #\Newline #\Return)
                            (uiop:run-program
                             (list "git" "-C" (namestring worktree)
                                   "rev-parse" "HEAD")
                             :output :string
                             :error-output :output))))
                     (unless (string= actual commit)
                       (error "Recovery worktree ~A is not at commit ~A."
                              worktree commit))
                     (let ((status
                             (uiop:run-program
                              (list "git" "-C" (namestring worktree)
                                    "status" "--porcelain")
                              :output :string
                              :error-output :output)))
                       (when (plusp (length status))
                         (error "Recovery worktree ~A contains uncommitted files."
                                worktree))))
                   (progn
                     (ensure-directories-exist worktree-root)
                     (uiop:run-program
                      (list "git" "-C" (namestring source-root)
                            "worktree" "add" "--detach"
                            (namestring worktree) commit)
                      :output :string
                      :error-output :output)))
               worktree))

           (boot-manifest (manifest forwarded-arguments)
             "Boot MANIFEST's core against its exact source worktree."
             (unless (compatible-manifest-p manifest)
               (error "Generation ~A is incompatible with this SBCL runtime."
                      (getf (rest manifest) :id)))
             (let* ((worktree (ensure-source-worktree manifest))
                    (core (getf (rest manifest) :core))
                    (sbcl-command (or (uiop:getenv "FROB_SBCL") "sbcl")))
               (sb-posix:setenv "FROB_SOURCE_ROOT" (namestring worktree) 1)
               (let ((process
                       (uiop:launch-program
                        (append
                         (list sbcl-command
                               "--noinform"
                               "--core" core
                               "--end-runtime-options")
                         forwarded-arguments)
                        :directory worktree
                        :input :interactive
                        :output :interactive
                        :error-output :interactive
                        :wait nil)))
                 (uiop:quit (uiop:wait-process process)))))

           (parse-recovery-arguments (arguments)
             "Parse recovery-only options and return their values plus forwarded arguments."
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
                           (error "Unknown recovery option ~A. Put application arguments after --."
                                  argument))))
               (values generation
                       list-p
                       status
                       capsule
                       forwarded
                       (nreverse original-arguments))))

           (report-crash (status capsule original-arguments)
             "Print bounded crash context before booting a retained generation."
             (when status
               (format *error-output* "Active Frob exited with status ~A.~%" status))
             (when (and capsule (probe-file capsule))
               (handler-case
                   (let ((record (read-form capsule)))
                     (format *error-output*
                             "Crash capsule: ~A~%Condition: ~A~%Conversation: ~A~%"
                             capsule
                             (or (and (listp record)
                                      (getf (rest record) :condition))
                                 "unknown")
                             (or (and (listp record)
                                      (getf (rest record) :conversation-id))
                                 "unknown")))
                 (error (condition)
                   (format *error-output* "Could not read crash capsule ~A: ~A~%"
                           capsule condition))))
             (when original-arguments
               (format *error-output* "Original arguments: ~S~%"
                       original-arguments))
             nil))
    (multiple-value-bind
        (generation list-p status capsule forwarded original-arguments)
        (parse-recovery-arguments recovery-arguments)
      (if list-p
          (list-generations)
          (let ((manifest-pathname
                  (if generation
                      (manifest-pathname-for-id generation)
                      (selected-manifest-pathname))))
            (unless (probe-file manifest-pathname)
              (error "No retained generation manifest exists at ~A."
                     manifest-pathname))
            (report-crash status capsule original-arguments)
            (boot-manifest (load-manifest manifest-pathname) forwarded))))))
  (error (condition)
    (format *error-output* "Recovery could not continue: ~A~%" condition)
    (uiop:quit 1)))
