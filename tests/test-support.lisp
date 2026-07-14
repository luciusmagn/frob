(in-package #:autolith)

;;;; -- Minimal Test Harness --

(defvar *test-count* 0
  "The number of assertions attempted by the current test run.")

(-> test-assert (t string) null)
(defun test-assert (value description)
  "Record one assertion and signal an error when VALUE is false."
  (incf *test-count*)
  (unless value
    (error "Test failed: ~A" description))
  nil)

(-> test-configuration () configuration)
(defun test-configuration ()
  "Return an isolated configuration rooted in a fresh temporary directory."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "autolith-tests-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (source-root (asdf:system-source-directory :autolith)))
    (uiop:ensure-all-directories-exist (list root))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :config-root (merge-pathnames "config/" root)
                   :data-root (merge-pathnames "data/" root)
                   :state-root (merge-pathnames "state/" root)
                   :cache-root (merge-pathnames "cache/" root)
                   :config-root (merge-pathnames "config/" root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" root)
                   :model +default-model+
                   :reasoning-effort +default-reasoning-effort+
                   :provider-endpoint +codex-responses-endpoint+)))
(-> test-configuration-root (configuration) pathname)
(defun test-configuration-root (configuration)
  "Return the common temporary root containing CONFIGURATION's data directory."
  (uiop:pathname-parent-directory-pathname
   (configuration-data-root configuration)))

(-> test-configuration-for-source-root (pathname) configuration)
(defun test-configuration-for-source-root (source-root)
  "Return an isolated configuration whose tracked source is SOURCE-ROOT."
  (let ((state-root (merge-pathnames ".autolith-test-state/" source-root)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :config-root (merge-pathnames "config/" state-root)
                   :data-root (merge-pathnames "data/" state-root)
                   :state-root (merge-pathnames "state/" state-root)
                   :cache-root (merge-pathnames "cache/" state-root)
                   :config-root (merge-pathnames "config/" state-root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" state-root)
                   :model +default-model+
                   :reasoning-effort +default-reasoning-effort+
                   :provider-endpoint +codex-responses-endpoint+)))
