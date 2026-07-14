(in-package #:autolith)

;;;; -- Global Preference Tests --

(-> preferences-tests--without-model-environment (function) t)
(defun preferences-tests--without-model-environment (function)
  "Call FUNCTION while model and effort environment overrides are absent."
  (let ((previous-model (uiop:getenv "AUTOLITH_MODEL"))
        (previous-effort (uiop:getenv "AUTOLITH_REASONING_EFFORT")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_MODEL")
           (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT")
           (funcall function))
      (if previous-model
          (sb-posix:setenv "AUTOLITH_MODEL" previous-model 1)
          (sb-posix:unsetenv "AUTOLITH_MODEL"))
      (if previous-effort
          (sb-posix:setenv "AUTOLITH_REASONING_EFFORT" previous-effort 1)
          (sb-posix:unsetenv "AUTOLITH_REASONING_EFFORT")))))

(-> test-preferences () null)
(defun test-preferences ()
  "Test atomic global preferences, migration, and malformed-file recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-preferences-path configuration))
         (legacy-path
           (merge-pathnames "preferences.sexp"
                            (configuration-state-root configuration))))
    (unwind-protect
         (progn
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (not (preference-state-reasoning-traces-p preferences))
              "missing preferences default reasoning summaries to hidden")
             (test-assert (null (preference-state-model preferences))
                          "missing preferences have no saved model")
             (test-assert
              (null (preference-state-reasoning-effort preferences))
              "missing preferences have no saved reasoning effort")
             (test-assert
              (preference-state-compact-view-p preferences)
              "missing preferences default to compact tool presentation"))
           (ensure-directories-exist legacy-path)
           (with-open-file (stream legacy-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string "(:preferences :version 1 :reasoning-traces-p t)"
                           stream))
           (configuration-ensure-directories configuration)
           (test-assert (not (probe-file legacy-path))
                        "legacy preferences move out of the state root")
           (test-assert (probe-file pathname)
                        "preferences migrate into the config root")
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (prin1 '(:preferences
                      :version 1
                      :reasoning-traces-p t)
                    stream)
             (terpri stream))
           (let ((legacy (preferences-load configuration)))
             (test-assert
              (preference-state-reasoning-traces-p legacy)
              "version one reasoning-summary preferences remain readable")
             (test-assert (null (preference-state-model legacy))
                          "version one preferences have no saved model")
             (test-assert
              (preference-state-compact-view-p legacy)
              "version one preferences default to compact tool presentation"))
           (let* ((selected
                    (configuration-with-reasoning-effort
                     (configuration-with-model configuration "gpt-5.6-luna")
                     "high")))
             (preferences-set-model-selection selected)
             (let ((preferences (preferences-load configuration)))
               (test-assert
                (string= (preference-state-model preferences) "gpt-5.6-luna")
                "the selected model survives a preference reload")
               (test-assert
                (string= (preference-state-reasoning-effort preferences) "high")
                "the selected reasoning effort survives a preference reload")
               (test-assert
                (preference-state-reasoning-traces-p preferences)
                "saving model choices preserves the trace preference")
               (test-assert
                (preference-state-compact-view-p preferences)
                "saving model choices preserves compact presentation"))
             (preferences-tests--without-model-environment
              (lambda ()
                (let ((restored
                        (preferences-apply-model-selection configuration)))
                  (test-assert
                   (string= (configuration-model restored) "gpt-5.6-luna")
                   "saved models become startup defaults")
                  (test-assert
                   (string= (configuration-reasoning-effort restored) "high")
                   "saved efforts become startup defaults")))))
           (let ((mode (sb-posix:stat-mode
                        (sb-posix:stat (namestring pathname)))))
             (test-assert (= (logand mode #o777) #o600)
                          "global preferences remain private to the user"))
           (preferences-set-reasoning-traces configuration nil)
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (not (preference-state-reasoning-traces-p preferences))
              "disabled reasoning summaries survive a preference reload")
             (test-assert
              (string= (preference-state-model preferences) "gpt-5.6-luna")
              "changing traces preserves the selected model")
             (test-assert
              (string= (preference-state-reasoning-effort preferences) "high")
              "changing traces preserves the selected effort")
             (test-assert
              (preference-state-compact-view-p preferences)
              "changing traces preserves compact presentation"))
           (preferences-set-compact-view configuration nil)
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (not (preference-state-compact-view-p preferences))
              "expanded tool presentation survives a preference reload")
             (test-assert
              (string= (preference-state-model preferences) "gpt-5.6-luna")
              "changing compact presentation preserves the selected model")
             (test-assert
              (not (preference-state-reasoning-traces-p preferences))
              "changing compact presentation preserves trace mode"))
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
                (not (preference-state-reasoning-traces-p
                      (preferences-load configuration)))
                "malformed preferences fall back to safe defaults"))
             (test-assert (typep warning 'preferences-load-warning)
                          "malformed preferences emit a typed warning")
             (test-assert (equal (preferences-load-warning-pathname warning)
                                 pathname)
                          "preference warnings identify the malformed file")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
