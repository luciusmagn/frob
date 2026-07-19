(in-package #:autolith)

;;;; -- Release Script Tests --

(define-constant +release-script-tests-version+ "0.11.0"
  :test #'string=
  :documentation "The semantic fixture version used by release boundary tests.")

(define-constant +release-script-tests-commit+
  "0123456789abcdef0123456789abcdef01234567"
  :test #'string=
  :documentation "The valid Git identity used by release boundary tests.")

(-> release-script-tests--write-file (pathname string) pathname)
(defun release-script-tests--write-file (pathname content)
  "Write CONTENT to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  pathname)

(-> release-script-tests--run
    (list &key (:directory (option pathname)) (:environment list)
               (:ignore-error-status boolean) (:output t))
    t)
(defun release-script-tests--run
    (command &key directory environment ignore-error-status (output ':string))
  "Run COMMAND with optional ENVIRONMENT assignments and return its output."
  (uiop:run-program
   (if environment
       (append (list "env") environment command)
       command)
   :directory directory
   :ignore-error-status ignore-error-status
   :output output
   :error-output (if ignore-error-status nil ':output)))

(-> release-script-tests--chmod (string pathname) null)
(defun release-script-tests--chmod (mode pathname)
  "Apply numeric or symbolic MODE to PATHNAME."
  (release-script-tests--run
   (list "chmod" mode (namestring pathname))
   :output nil)
  nil)

(-> release-script-tests--record (pathname string) pathname)
(defun release-script-tests--record (pathname tag)
  "Write a fixture release record with TAG to PATHNAME."
  (release-script-tests--write-file
   pathname
   (format nil "version=~A~%tag=~A~%commit=~A~%"
           +release-script-tests-version+
           tag
           +release-script-tests-commit+)))

(-> release-script-tests--fixture-curl () string)
(defun release-script-tests--fixture-curl ()
  "Return a curl substitute serving files from the test fixture root."
  (format nil
          "#!/bin/sh~%set -eu~%output=~%write_out=~%url=~%while [ \"$#\" -gt 0 ]; do~%  case $1 in~%    --output) output=$2; shift 2 ;;~%    --write-out) write_out=$2; shift 2 ;;~%    --retry|--proto|--max-time) shift 2 ;;~%    --*) shift ;;~%    *) url=$1; shift ;;~%  esac~%done~%case $url in~%  */latest)~%    [ -n \"$write_out\" ]~%    printf \"%s\" https://example.invalid/releases/v0.11.0~%    exit 0~%    ;;~%esac~%cp \"$AUTOLITH_TEST_RELEASE_FIXTURE/${url##*/}\" \"$output\"~%"))

(-> release-script-tests--readlink (pathname) string)
(defun release-script-tests--readlink (pathname)
  "Return the literal target stored in symbolic link PATHNAME."
  (string-trim
   '(#\Newline #\Return)
   (release-script-tests--run
    (list "readlink" (namestring pathname)))))

(-> release-script-tests--cleanup (pathname) null)
(defun release-script-tests--cleanup (root)
  "Remove test ROOT after restoring write permissions."
  (when (uiop:directory-exists-p root)
    (release-script-tests--run
     (list "chmod" "-R" "u+w" (namestring root))
     :ignore-error-status t
     :output nil)
    (uiop:delete-directory-tree root
                                :validate t
                                :if-does-not-exist :ignore))
  nil)

(-> release-script-tests--make-release (pathname pathname) pathname)
(defun release-script-tests--make-release (source-root release-root)
  "Create a minimal packaged release fixture below RELEASE-ROOT."
  (dolist (relative '("libexec/autolith/.qlot/setup.lisp"
                      "libexec/autolith/autolith.asd"
                      "libexec/sbcl-source/version.lisp-expr"
                      "lib/libfff_c.so"
                      "runtime/bin/sbcl"
                      "libexec/cl-exec-sandbox-helper"))
    (release-script-tests--write-file
     (merge-pathnames relative release-root)
     ""))
  (let ((launcher (merge-pathnames "bin/autolith" release-root)))
    (ensure-directories-exist launcher)
    (uiop:copy-file (merge-pathnames "bin/autolith-release" source-root)
                    launcher)
    (release-script-tests--chmod "755" launcher))
  (release-script-tests--chmod
   "755" (merge-pathnames "runtime/bin/sbcl" release-root))
  (release-script-tests--chmod
   "755" (merge-pathnames "libexec/cl-exec-sandbox-helper" release-root))
  (release-script-tests--record
   (merge-pathnames "RELEASE" release-root)
   (format nil "v~A" +release-script-tests-version+))
  release-root)

(-> release-script-tests--syntax (pathname) null)
(defun release-script-tests--syntax (source-root)
  "Check the remaining bootstrap shell boundaries and tracked Lisp programs."
  (dolist (relative '("bin/autolith"
                      "bin/autolith-release"
                      "script/build-release"
                      "script/build-release-runtime"
                      "server/build-in-container"))
    (release-script-tests--run
     (list "bash" "-n" (namestring (merge-pathnames relative source-root)))
     :output nil))
  (release-script-tests--run
   (list "sh" "-n"
         (namestring (merge-pathnames "script/install" source-root)))
   :output nil)
  (dolist (relative '("script/build-release-runtime.lisp"
                      "server/build-in-container.lisp"))
    (with-open-file (stream (merge-pathnames relative source-root)
                            :direction :input
                            :external-format :utf-8)
      (let ((*read-eval* nil))
        (loop while (read stream nil nil))))
    (test-assert (probe-file (merge-pathnames relative source-root))
                 (format nil "release program ~A is readable Lisp" relative)))
  (test-assert
   (probe-file (merge-pathnames "server/Containerfile" source-root))
   "the release container definition is readable")
  (test-assert
   (search "RUSTUP_TOOLCHAIN=1.97.1"
           (uiop:read-file-string
            (merge-pathnames "server/Containerfile" source-root)))
   "the release container selects its pinned Rust toolchain")
  nil)

(-> release-script-tests--source-launcher (pathname pathname) null)
(defun release-script-tests--source-launcher (source-root root)
  "Exercise source-image selection, bootstrap prompting, and forced source use."
  (let* ((fixture-root (merge-pathnames "source-launcher/" root))
         (bin-directory (merge-pathnames "bin/" fixture-root))
         (script-directory (merge-pathnames "script/" fixture-root))
         (recovery-directory (merge-pathnames "recovery/" fixture-root))
         (data-home (merge-pathnames "data/" fixture-root))
         (state-home (merge-pathnames "state/" fixture-root))
         (active-directory (merge-pathnames "autolith/active/" data-home))
         (active-core (merge-pathnames "autolith-active.core" active-directory))
         (active-manifest (merge-pathnames "manifest.sexp" active-directory))
         (launcher (merge-pathnames "autolith" bin-directory))
         (active-source (merge-pathnames "autolith-active" bin-directory))
         (bootstrap (merge-pathnames "bootstrap" script-directory))
         (recovery-source (merge-pathnames "launcher.lisp" recovery-directory))
         (fake-sbcl (merge-pathnames "fake-sbcl" fixture-root))
         (log (merge-pathnames "launcher.log" fixture-root)))
    (uiop:ensure-all-directories-exist
     (list bin-directory script-directory recovery-directory
           data-home state-home))
    (uiop:copy-file (merge-pathnames "bin/autolith" source-root) launcher)
    (release-script-tests--write-file active-source "")
    (release-script-tests--write-file recovery-source "")
    (release-script-tests--write-file
     (merge-pathnames "sbcl.version" fixture-root)
     "2.6.4\n")
    (release-script-tests--write-file
     (merge-pathnames "sbcl-source.sha256" fixture-root)
     (format nil "~A~%" (make-string 64 :initial-element #\0)))
    (release-script-tests--write-file
     fake-sbcl
     "#!/bin/sh
set -eu
printf 'SBCL %s\\n' \"$*\" >> \"$AUTOLITH_TEST_LOG\"
mode=UNKNOWN
probe=false
for argument in \"$@\"; do
  case $argument in
    --autolith-internal-active-image-probe) probe=true ;;
    */bin/autolith-active) mode=SOURCE ;;
    */autolith-active.core) mode=ACTIVE ;;
  esac
done
if [ \"$probe\" = true ]; then
  exit 0
fi
printf '%s %s\\n' \"$mode\" \"$*\"
")
    (release-script-tests--write-file
     bootstrap
     "#!/bin/sh
set -eu
printf 'BOOTSTRAP\\n' >> \"$AUTOLITH_TEST_LOG\"
active=$XDG_DATA_HOME/autolith/active
mkdir -p \"$active\"
: > \"$active/autolith-active.core\"
printf '(:ACTIVE-IMAGE :VERSION 1\\n)\\n' > \"$active/manifest.sexp\"
")
    (dolist (pathname (list launcher fake-sbcl bootstrap))
      (release-script-tests--chmod "755" pathname))
    (let* ((environment
             (list (format nil "XDG_DATA_HOME=~A" (namestring data-home))
                   (format nil "XDG_STATE_HOME=~A" (namestring state-home))
                   (format nil "AUTOLITH_SBCL=~A" (namestring fake-sbcl))
                   (format nil "AUTOLITH_TEST_LOG=~A" (namestring log))))
           (source-output
             (release-script-tests--run
              (list (namestring launcher) "--from-source" "fixture-argument")
              :environment environment)))
      (test-assert
       (and (search "SOURCE" source-output)
            (not (search "fast startup image" source-output))
            (not (search "--from-source" (uiop:read-file-string log))))
       "--from-source quietly bypasses images and is not forwarded")
      (release-script-tests--write-file log "")
      (let* ((command
               (format nil
                       "env ~{~A~^ ~} ~A fixture-argument"
                       (mapcar #'uiop:escape-shell-token environment)
                       (uiop:escape-shell-token (namestring launcher))))
             (output
               (with-input-from-string (input (format nil "y~%"))
                 (uiop:run-program
                  (list "script" "-q" "-e" "-c" command "/dev/null")
                  :input input
                  :output :string
                  :error-output ':output
                  :ignore-error-status t)))
             (events (uiop:read-file-string log)))
        (test-assert
         (and (search "fast startup image is missing or stale" output)
              (search "BOOTSTRAP" events)
              (search "ACTIVE" output)
              (not (search "SOURCE" output)))
         (format nil
                 "accepting the interactive prompt bootstraps and starts the new image:~%terminal: ~A~%events: ~A"
                 output events)))
      (dolist (pathname (list active-core active-manifest))
        (when (probe-file pathname)
          (delete-file pathname)))
      (release-script-tests--write-file log "")
      (let* ((command
               (format nil
                       "env ~{~A~^ ~} ~A fixture-argument"
                       (mapcar #'uiop:escape-shell-token environment)
                       (uiop:escape-shell-token (namestring launcher))))
             (output
               (with-input-from-string (input (format nil "n~%"))
                 (uiop:run-program
                  (list "script" "-q" "-e" "-c" command "/dev/null")
                  :input input
                  :output :string
                  :error-output ':output
                  :ignore-error-status t)))
             (events (uiop:read-file-string log)))
        (test-assert
         (and (search "fast startup image is missing or stale" output)
              (search "SOURCE" output)
              (not (search "BOOTSTRAP" events)))
         (format nil
                 "declining the interactive prompt loads source without bootstrapping: ~A"
                 output))))
  nil))

(-> release-script-tests--launcher (pathname pathname) null)
(defun release-script-tests--launcher (source-root root)
  "Exercise packaged launcher validation and its machine-readable probe."
  (let* ((release-root
           (merge-pathnames
            (format nil "autolith-v~A/" +release-script-tests-version+)
            root))
         (launcher
           (merge-pathnames "bin/autolith" release-root))
         (environment '("AUTOLITH_NO_UPDATE_CHECK=1")))
    (release-script-tests--make-release source-root release-root)
    (let ((output
            (release-script-tests--run
             (list (namestring launcher) "--autolith-release-probe")
             :environment environment)))
      (dolist (line
               (list
                (format nil "version=~A" +release-script-tests-version+)
                (format nil "tag=v~A" +release-script-tests-version+)
                (format nil "commit=~A" +release-script-tests-commit+)
                (format nil "source=~A"
                        (string-right-trim
                         "/"
                         (namestring
                          (merge-pathnames "libexec/autolith/" release-root))))
                (format nil "runtime=~A"
                        (namestring
                         (merge-pathnames "runtime/bin/sbcl" release-root)))))
        (test-assert (find line
                           (uiop:split-string output
                                              :separator '(#\Newline #\Return))
                           :test #'string=)
                     (format nil "release probe reports ~A" line))))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     "v0.12.0")
    (multiple-value-bind (output error-output status)
        (release-script-tests--run
         (list (namestring launcher) "--autolith-release-probe")
         :environment environment
         :ignore-error-status t
         :output nil)
      (declare (ignore output error-output))
      (test-assert (not (eql status 0))
                   "the release launcher rejects an inconsistent record"))
    (release-script-tests--record
     (merge-pathnames "RELEASE" release-root)
     (format nil "v~A" +release-script-tests-version+)))
  nil)

(-> release-script-tests--installer (pathname pathname) null)
(defun release-script-tests--installer (source-root root)
  "Exercise versioned installer publication, repeatability, and latest lookup."
  (let* ((version +release-script-tests-version+)
         (tag (format nil "v~A" version))
         (release-name (format nil "autolith-~A-x86_64-linux" tag))
         (release-root
           (merge-pathnames (format nil "autolith-v~A/" version) root))
         (fixture-root (merge-pathnames "fixture/" root))
         (fixture-source (merge-pathnames "fixture-source/" root))
         (fixture-bin (merge-pathnames "fixture-bin/" root))
         (fixture-release
           (merge-pathnames (format nil "~A/" release-name) fixture-source))
         (archive
           (merge-pathnames (format nil "~A.tar.gz" release-name) fixture-root))
         (checksum
           (merge-pathnames (format nil "~A.tar.gz.sha256" release-name)
                            fixture-root))
         (install-root (merge-pathnames "installation/" root))
         (bin-directory (merge-pathnames "bin/" root))
         (curl (merge-pathnames "curl" fixture-bin))
         (installer (merge-pathnames "script/install" source-root)))
    (release-script-tests--make-release source-root release-root)
    (uiop:ensure-all-directories-exist
     (list fixture-root fixture-source fixture-bin fixture-release))
    (release-script-tests--run
     (list "cp" "-a" (format nil "~A." (namestring release-root))
           (namestring fixture-release))
     :output nil)
    (release-script-tests--chmod "a-w" fixture-release)
    (release-script-tests--run
     (list "tar" "-czf" (namestring archive)
           "-C" (namestring fixture-source) release-name)
     :output nil)
    (release-script-tests--run
     (list "sha256sum" (file-namestring archive))
     :directory fixture-root
     :output checksum)
    (release-script-tests--write-file
     curl (release-script-tests--fixture-curl))
    (release-script-tests--chmod "755" curl)
    (let* ((path (format nil "~A:~A"
                         (string-right-trim "/" (namestring fixture-bin))
                         (or (uiop:getenv "PATH") "")))
           (base-environment
             (list
              (format nil "PATH=~A" path)
              (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A"
                      (namestring fixture-root))
              "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
              (format nil "AUTOLITH_INSTALL_ROOT=~A"
                      (string-right-trim "/" (namestring install-root)))
              (format nil "AUTOLITH_BIN_DIR=~A"
                      (string-right-trim "/" (namestring bin-directory))))))
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (test-assert
       (probe-file
        (merge-pathnames (format nil "releases/~A/bin/autolith" tag)
                         install-root))
       "the installer publishes the requested release")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "current" install-root))
                (format nil "releases/~A" tag))
       "the installer selects the requested version atomically")
      (test-assert
       (string= (release-script-tests--readlink
                 (merge-pathnames "autolith" bin-directory))
                (namestring (merge-pathnames "current/bin/autolith"
                                             install-root)))
       "the installer publishes the user command link")
      (release-script-tests--run
       (list (namestring installer) "--version" tag)
       :environment base-environment
       :output nil)
      (release-script-tests--run
       (list (namestring installer))
       :environment
       (append
        (remove "AUTOLITH_RELEASE_BASE_URL=https://example.invalid"
                base-environment :test #'string=)
        '("AUTOLITH_RELEASE_BASE_URL=https://example.invalid/releases"
          "AUTOLITH_RELEASE_LATEST_URL=https://example.invalid/releases/latest"))
       :output nil)))
  nil)

(-> test-release-scripts () null)
(defun test-release-scripts ()
  "Test shell bootstrap boundaries through Common Lisp fixtures."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-script-tests-~A/" (make-identifier))
             (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (release-script-tests--syntax source-root)
           (release-script-tests--source-launcher source-root root)
           (release-script-tests--launcher source-root root)
           (release-script-tests--installer source-root root))
      (release-script-tests--cleanup root)))
  nil)
