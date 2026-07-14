(in-package #:autolith)

;;;; -- Global Preference Tests --

(-> test-preferences () null)
(defun test-preferences ()
  "Test atomic reasoning-summary preferences and malformed-file recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-preferences-path configuration)))
    (unwind-protect
         (progn
           (test-assert (not (preferences-reasoning-traces-p configuration))
                        "missing preferences default reasoning summaries to hidden")
           (preferences-set-reasoning-traces configuration t)
           (test-assert (preferences-reasoning-traces-p configuration)
                        "enabled reasoning summaries survive a preference reload")
           (let ((application (application-create configuration)))
             (test-assert (application-reasoning-traces-p application)
                          "new applications restore enabled reasoning summaries")
             (test-assert
              (provider-reasoning-summaries-p
               (application-provider application))
              "restored trace preferences opt provider requests into summaries"))
           (let ((mode (sb-posix:stat-mode
                        (sb-posix:stat (namestring pathname)))))
             (test-assert (= (logand mode #o777) #o600)
                          "global preferences remain private to the user"))
           (preferences-set-reasoning-traces configuration nil)
           (test-assert (not (preferences-reasoning-traces-p configuration))
                        "disabled reasoning summaries survive a preference reload")
           (let ((application (application-create configuration)))
             (test-assert (not (application-reasoning-traces-p application))
                          "new applications restore hidden reasoning summaries")
             (test-assert
              (not
               (provider-reasoning-summaries-p
                (application-provider application)))
              "restored hidden preferences omit provider summaries"))
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format :utf-8)
             (write-string "#.(error \"preference read evaluation escaped\")"
                           stream))
           (let ((warning nil))
             (handler-bind
                 ((preferences-load-warning
                    (lambda (condition)
                      (setf warning condition)
                      (muffle-warning condition))))
               (test-assert
                (not (preferences-reasoning-traces-p configuration))
                "malformed preferences fall back to hidden summaries"))
             (test-assert (typep warning 'preferences-load-warning)
                          "malformed preferences emit a typed warning")
             (test-assert (equal (preferences-load-warning-pathname warning)
                                 pathname)
                          "preference warnings identify the malformed file")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
