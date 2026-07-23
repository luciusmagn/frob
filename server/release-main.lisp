(in-package #:autolith)

;;;; -- Release Service Entry --

(-> release-server-main () null)
(defun release-server-main ()
  "Run the release service mode selected by the command line."
  (let ((arguments (uiop:command-line-arguments)))
    (cond
      ((or (null arguments) (equal arguments '("serve")))
       (release-server-serve (release-server-configuration-create)))
      ((equal arguments '("build"))
       (release-builder-run (release-builder-configuration-create)))
      ((equal arguments '("build-once"))
       (release-builder-build-pending
        (release-builder-configuration-create)))
      ((equal arguments '("update"))
       (release-updater-run (release-updater-configuration-create)))
      ((equal arguments '("update-once"))
       (release-updater-update-once
        (release-updater-configuration-create))
       nil)
      ((and (= (length arguments) 3)
            (string= (first arguments) "host-probe"))
       (release-updater-host-probe (second arguments) (third arguments)))
      ((and arguments
            (string= (first arguments) "archive")
            (null (rest (rest arguments))))
       (release-archive-build
        :source-root (asdf:system-source-directory :autolith)
        :output-directory
        (if (second arguments)
            (pathname (second arguments))
            (merge-pathnames "dist/"
                             (asdf:system-source-directory :autolith))))
       nil)
      (t
       (error 'configuration-error
              :message
              "Usage: server/run [serve|build|build-once|update|update-once|host-probe TAG COMMIT|archive [DIRECTORY]]"))))
  nil)
