(require :asdf)

(let* ((arguments (uiop:command-line-arguments))
       (source-root
         (and (first arguments)
              (uiop:ensure-directory-pathname (first arguments))))
       (state-root
         (and (second arguments)
              (uiop:ensure-directory-pathname (second arguments))))
       (output-root
         (and (third arguments)
              (uiop:ensure-directory-pathname (third arguments))))
       (home (and state-root (merge-pathnames "home/" state-root)))
       (quicklisp-setup (and home (merge-pathnames "quicklisp/setup.lisp" home)))
       (quicklisp-installer
         (and state-root (merge-pathnames "quicklisp.lisp" state-root)))
       (runtime-command (uiop:getenv "AUTOLITH_SBCL")))
  (labels ((run (command &key directory)
             "Run one container build COMMAND with inherited terminal streams."
             (uiop:run-program command
                               :directory directory
                               :input ':interactive
                               :output ':interactive
                               :error-output ':interactive)))
    (handler-case
        (progn
          (unless (and (= (length arguments) 3)
                       source-root state-root output-root runtime-command)
            (error
             "usage: build-in-container.lisp SOURCE STATE OUTPUT with AUTOLITH_SBCL"))
          (unless (probe-file quicklisp-setup)
            (run
             (list "curl" "--fail" "--location" "--show-error" "--retry" "3"
                   "--proto" "=https" "--tlsv1.2"
                   "--output" (namestring quicklisp-installer)
                   "https://beta.quicklisp.org/quicklisp.lisp"))
            (run
             (list runtime-command "--noinform" "--non-interactive"
                   "--load" (namestring quicklisp-installer)
                   "--eval" "(quicklisp-quickstart:install)")))
          (run (list "./script/bootstrap") :directory source-root)
          (run (list "./script/check") :directory source-root)
          (run (list "./script/build-release" (namestring output-root))
               :directory source-root))
      (error (condition)
        (format *error-output* "~&Autolith container build failed: ~A~%"
                condition)
        (finish-output *error-output*)
        (uiop:quit 1)))))
