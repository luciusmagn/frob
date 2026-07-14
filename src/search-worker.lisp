(in-package #:autolith)

;;;; -- Search Worker Protocol --

(define-constant +search-worker-protocol-version+ 1
  :documentation "The private protocol shared with the isolated fff process.")

(define-constant +search-worker-start-timeout-seconds+ 30
  :documentation "The maximum time allowed for the fff helper to start.")

(define-constant +search-worker-request-timeout-seconds+ 45
  :documentation "The maximum time allowed for one complete fff request.")

(-> search-worker--write-form (stream list) null)
(defun search-worker--write-form (stream form)
  "Write one portable protocol FORM to STREAM and flush it."
  (let ((*print-circle* nil)
        (*print-length* nil)
        (*print-level* nil)
        (*print-pretty* nil)
        (*print-readably* t))
    (write form :stream stream)
    (terpri stream)
    (finish-output stream))
  nil)

(-> search-worker--read-form (stream integer) t)
(defun search-worker--read-form (stream timeout-seconds)
  "Read one portable form from STREAM within TIMEOUT-SECONDS."
  (let ((*read-eval* nil)
        (end-marker (gensym "SEARCH-WORKER-END-")))
    (let ((form
            (sb-sys:with-deadline (:seconds timeout-seconds)
              (read stream nil end-marker))))
      (when (eq form end-marker)
        (error "The fff helper closed its response stream."))
      form)))

(-> search-worker--response-p (t integer) boolean)
(defun search-worker--response-p (value request-id)
  "Return true when VALUE is a complete response for REQUEST-ID."
  (and (listp value)
       (eq (first value) :search-response)
       (= (or (getf (rest value) :version) 0)
          +search-worker-protocol-version+)
       (= (or (getf (rest value) :id) 0) request-id)
       (member (getf (rest value) :status) '(:ok :error))
       (stringp (getf (rest value) :content))
       t))

(-> search-worker--request-form
    (integer keyword configuration list)
    list)
(defun search-worker--request-form (request-id operation configuration arguments)
  "Return one complete helper request for OPERATION and ARGUMENTS."
  (list :search-request
        :version +search-worker-protocol-version+
        :id request-id
        :operation operation
        :source-root (namestring (configuration-source-root configuration))
        :working-directory
        (namestring (configuration-working-directory configuration))
        :data-root (namestring (configuration-data-root configuration))
        :state-root (namestring (configuration-state-root configuration))
        :cache-root (namestring (configuration-cache-root configuration))
        :arguments arguments))

(-> search-worker--log-path (configuration) pathname)
(defun search-worker--log-path (configuration)
  "Return the private diagnostic log for CONFIGURATION's fff helper."
  (merge-pathnames "fff/worker.log"
                   (configuration-cache-root configuration)))


;;;; -- Parent-Side Lifecycle --

(-> search-worker--alive-p (search-worker) boolean)
(defun search-worker--alive-p (worker)
  "Return true when WORKER has a live helper process and open streams."
  (let ((process (search-worker-process worker)))
    (and process
         (uiop:process-alive-p process)
         (open-stream-p (search-worker-input worker))
         (open-stream-p (search-worker-output worker))
         t)))

(-> search-worker--detach-unlocked (search-worker) null)
(defun search-worker--detach-unlocked (worker)
  "Close inherited streams and forget WORKER without signaling its process."
  (dolist (stream (list (search-worker-input worker)
                        (search-worker-output worker)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  (setf (search-worker-process worker) nil
        (search-worker-input worker) nil
        (search-worker-output worker) nil
        (search-worker-source-root worker) nil
        (search-worker-next-request-id worker) 1)
  nil)

(-> search-worker--close-unlocked (search-worker) null)
(defun search-worker--close-unlocked (worker)
  "Stop and forget WORKER while its lifecycle lock is held."
  (let ((process (search-worker-process worker))
        (input (search-worker-input worker)))
    (when (and process (uiop:process-alive-p process))
      (when (and input (open-stream-p input))
        (ignore-errors
          (search-worker--write-form
           input
           (list :search-shutdown
                 :version +search-worker-protocol-version+))))
      (loop repeat 20
            while (uiop:process-alive-p process)
            do (sleep 0.01))
      (when (uiop:process-alive-p process)
        (ignore-errors (uiop:terminate-process process :urgent t))))
    (when process
      (ignore-errors (uiop:wait-process process))))
  (search-worker--detach-unlocked worker))

(-> search-worker-close (search-worker) null)
(defun search-worker-close (worker)
  "Stop WORKER and its isolated native fff index."
  (with-lock-held ((search-worker-lock worker))
    (search-worker--close-unlocked worker)))

(-> search-worker-detach (search-worker) null)
(defun search-worker-detach (worker)
  "Forget inherited WORKER streams without affecting the live parent process."
  (with-lock-held ((search-worker-lock worker))
    (search-worker--detach-unlocked worker)))

(-> search-worker--start-unlocked (search-worker configuration) search-worker)
(defun search-worker--start-unlocked (worker configuration)
  "Start WORKER for CONFIGURATION and validate its protocol handshake."
  (search--validated-library-path configuration)
  (let* ((source-root (truename (configuration-source-root configuration)))
         (script (merge-pathnames "bin/autolith-search-worker" source-root))
         (log-path (search-worker--log-path configuration))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (unless (probe-file script)
      (search--fail ':worker
                    (format nil "The private fff helper is missing at ~A." script)
                    :pathname script))
    (ensure-directories-exist log-path)
    (with-open-file (stream log-path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (declare (ignore stream)))
    (search-worker--close-unlocked worker)
    (let ((process
            (uiop:launch-program
             (list sbcl-command "--noinform" "--script" (namestring script))
             :input :stream
             :output :stream
             :error-output log-path
             :if-error-output-exists :append
             :wait nil)))
      (setf (search-worker-process worker) process
            (search-worker-input worker) (uiop:process-info-input process)
            (search-worker-output worker) (uiop:process-info-output process)
            (search-worker-source-root worker) source-root
            (search-worker-next-request-id worker) 1)
      (handler-case
          (let ((handshake
                  (search-worker--read-form
                   (search-worker-output worker)
                   +search-worker-start-timeout-seconds+)))
            (unless (equal handshake
                           (list :search-worker
                                 :version +search-worker-protocol-version+))
              (error "Invalid fff helper handshake: ~S" handshake)))
        (error (condition)
          (search-worker--close-unlocked worker)
          (search--fail ':worker
                        (format nil "Could not start the private fff helper: ~A"
                                condition)
                        :pathname script
                        :cause condition)))))
  worker)

(-> search-worker--ensure-unlocked
    (search-worker configuration)
    search-worker)
(defun search-worker--ensure-unlocked (worker configuration)
  "Return a live WORKER using CONFIGURATION's current source tree."
  (let ((source-root (truename (configuration-source-root configuration))))
    (unless (and (search-worker--alive-p worker)
                 (equal source-root (search-worker-source-root worker)))
      (search-worker--start-unlocked worker configuration)))
  worker)

(-> search-worker--exchange
    (search-worker configuration keyword list)
    (values (option string) (option string)))
(defun search-worker--exchange (worker configuration operation arguments)
  "Exchange one request with WORKER, returning content or a remote error."
  (search-worker--ensure-unlocked worker configuration)
  (let ((request-id (search-worker-next-request-id worker)))
    (incf (search-worker-next-request-id worker))
    (search-worker--write-form
     (search-worker-input worker)
     (search-worker--request-form request-id operation configuration arguments))
    (let ((response
            (search-worker--read-form
             (search-worker-output worker)
             +search-worker-request-timeout-seconds+)))
      (unless (search-worker--response-p response request-id)
        (error "Invalid fff helper response: ~S" response))
      (if (eq (getf (rest response) :status) :ok)
          (values (getf (rest response) :content) nil)
          (values nil (getf (rest response) :content))))))

(-> search-worker-request
    (search-worker configuration keyword list)
    string)
(defun search-worker-request (worker configuration operation arguments)
  "Run one read-only search, restarting WORKER once after process failure."
  (with-lock-held ((search-worker-lock worker))
    (loop for attempt from 1 to 2
          do (handler-case
                 (multiple-value-bind (content remote-error)
                     (search-worker--exchange worker
                                              configuration
                                              operation
                                              arguments)
                   (when remote-error
                     (search--fail operation remote-error
                                   :pathname
                                   (configuration-working-directory
                                    configuration)))
                   (return content))
               (search-error (condition)
                 (error condition))
               (error (condition)
                 (search-worker--close-unlocked worker)
                 (when (= attempt 2)
                   (search--fail
                    operation
                    (format nil
                            "The isolated fff helper failed twice: ~A Diagnostic log: ~A"
                            condition
                            (search-worker--log-path configuration))
                    :pathname (configuration-working-directory configuration)
                    :cause condition)))))))

(-> tool-registry--search-workers (tool-registry) list)
(defun tool-registry--search-workers (registry)
  "Return the distinct fff workers owned by REGISTRY."
  (let ((seen (make-hash-table :test #'eq))
        (workers nil))
    (dolist (tool (tool-registry-tools registry))
      (when (typep tool 'search-tool)
        (let ((worker (search-tool-engine tool)))
          (unless (gethash worker seen)
            (setf (gethash worker seen) t)
            (push worker workers)))))
    workers))

(-> tool-registry-close-search-state (tool-registry) null)
(defun tool-registry-close-search-state (registry)
  "Stop every isolated native search worker owned by REGISTRY."
  (dolist (worker (tool-registry--search-workers registry))
    (search-worker-close worker))
  nil)

(-> tool-registry-detach-search-state (tool-registry) null)
(defun tool-registry-detach-search-state (registry)
  "Detach inherited search-worker streams before saving a forked Lisp image."
  (dolist (worker (tool-registry--search-workers registry))
    (search-worker-detach worker))
  nil)


;;;; -- Child-Side Dispatch --

(-> search-worker--configuration (list) configuration)
(defun search-worker--configuration (request)
  "Reconstruct the path configuration carried by REQUEST."
  (make-instance 'configuration
                 :source-root
                 (uiop:ensure-directory-pathname
                  (pathname (getf (rest request) :source-root)))
                 :working-directory
                 (uiop:ensure-directory-pathname
                  (pathname (getf (rest request) :working-directory)))
                 :data-root
                 (uiop:ensure-directory-pathname
                  (pathname (getf (rest request) :data-root)))
                 :state-root
                 (uiop:ensure-directory-pathname
                  (pathname (getf (rest request) :state-root)))
                 :cache-root
                 (uiop:ensure-directory-pathname
                  (pathname (getf (rest request) :cache-root)))
                 :codex-auth-path #P"/dev/null"
                 :model +default-model+
                 :reasoning-effort +default-reasoning-effort+
                 :provider-endpoint +codex-responses-endpoint+))

(-> search-worker--dispatch (search-engine list) string)
(defun search-worker--dispatch (engine request)
  "Execute one validated native search REQUEST with ENGINE."
  (unless (and (listp request)
               (eq (first request) :search-request)
               (= (or (getf (rest request) :version) 0)
                  +search-worker-protocol-version+)
               (integerp (getf (rest request) :id))
               (keywordp (getf (rest request) :operation))
               (listp (getf (rest request) :arguments)))
    (error "Invalid search-worker request."))
  (let ((configuration (search-worker--configuration request))
        (arguments (getf (rest request) :arguments)))
    (case (getf (rest request) :operation)
      (:files
       (apply #'search-engine-search-files engine configuration arguments))
      (:content
       (apply #'search-engine-search-content engine configuration arguments))
      (:multi-content
       (apply #'search-engine-search-multi-content
              engine
              configuration
              arguments))
      (otherwise
       (error "Unknown search-worker operation ~S."
              (getf (rest request) :operation))))))

(-> search-worker--error-text (serious-condition) string)
(defun search-worker--error-text (condition)
  "Return bounded printable text for one worker CONDITION."
  (let ((text (princ-to-string condition)))
    (subseq text 0 (min (length text) 2000))))

(-> search-worker-main (stream) null)
(defun search-worker-main (protocol-output)
  "Serve private fff requests on standard input and PROTOCOL-OUTPUT."
  (sb-ext:disable-debugger)
  (let ((engine (make-instance 'search-engine))
        (*read-eval* nil))
    (unwind-protect
         (progn
           (search-worker--write-form
            protocol-output
            (list :search-worker :version +search-worker-protocol-version+))
           (loop for request = (read *standard-input* nil nil)
                 while request
                 do (when (and (listp request)
                               (eq (first request) :search-shutdown)
                               (= (or (getf (rest request) :version) 0)
                                  +search-worker-protocol-version+))
                      (return))
                    (let ((request-id (and (listp request)
                                           (getf (rest request) :id))))
                      (handler-case
                          (search-worker--write-form
                           protocol-output
                           (list :search-response
                                 :version +search-worker-protocol-version+
                                 :id request-id
                                 :status :ok
                                 :content
                                 (search-worker--dispatch engine request)))
                        (serious-condition (condition)
                          (search-worker--write-form
                           protocol-output
                           (list :search-response
                                 :version +search-worker-protocol-version+
                                 :id request-id
                                 :status :error
                                 :content
                                 (search-worker--error-text condition))))))))
      (ignore-errors (search-engine-close engine))))
  nil)


;;;; -- Tool Execution --

(defmethod tool-execute ((tool search-files-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Fuzzy-search workspace file paths with isolated fff."
  (let ((query (search-tool--string-argument tool arguments "query" :required t))
        (page (search-tool--bounded-integer arguments "page" 0 0 1000000))
        (maximum-results
          (search-tool--bounded-integer
           arguments
           "max-results"
           +search-default-result-limit+
           1
           +search-maximum-result-limit+)))
    (unless (non-empty-string-p query)
      (error 'tool-error
             :message "search.files requires a non-empty query."
             :tool-name "search.files"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      ':files
      (list query
            :page page
            :maximum-results maximum-results)))))

(defmethod tool-execute ((tool search-glob-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Filter workspace file paths by one literal fff glob."
  (let ((pattern
          (search-tool--string-argument tool arguments "pattern" :required t))
        (page (search-tool--bounded-integer arguments "page" 0 0 1000000))
        (maximum-results
          (search-tool--bounded-integer
           arguments
           "max-results"
           +search-default-result-limit+
           1
           +search-maximum-result-limit+)))
    (unless (non-empty-string-p pattern)
      (error 'tool-error
             :message "search.glob requires a non-empty pattern."
             :tool-name "search.glob"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      ':files
      (list pattern
            :glob-p t
            :page page
            :maximum-results maximum-results)))))

(defmethod tool-execute ((tool search-content-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search workspace contents with isolated fff matching."
  (let ((query (search-tool--string-argument tool arguments "query" :required t))
        (mode (string-downcase
               (search-tool--string-argument tool arguments "mode"
                                             :fallback "plain"))))
    (unless (non-empty-string-p query)
      (error 'tool-error
             :message "search.content requires a non-empty query."
             :tool-name "search.content"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      ':content
      (append (list query :mode mode)
              (search-tool--common-content-options arguments))))))

(defmethod tool-execute ((tool search-multi-content-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search workspace contents for several literal patterns with isolated fff."
  (let* ((value (tool-argument arguments "patterns" :required t))
         (patterns (and (vectorp value) (coerce value 'list)))
         (constraints
           (search-tool--string-argument tool arguments "constraints")))
    (unless (and patterns
                 (every (lambda (pattern)
                          (and (non-empty-string-p pattern)
                               (not (find #\Newline pattern))))
                        patterns))
      (error 'tool-error
             :message "search.multi-content requires non-empty literal patterns without newlines."
             :tool-name "search.multi-content"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      ':multi-content
      (append (list patterns constraints)
              (search-tool--common-content-options arguments))))))
