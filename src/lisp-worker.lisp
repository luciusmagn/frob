(in-package #:frob)

;;;; -- Worker Process --

(defclass lisp-worker ()
  ((configuration
    :initarg :configuration
    :reader lisp-worker-configuration
    :type configuration
    :documentation "The source and workspace paths inherited by the worker.")
   (process
    :initform nil
    :accessor lisp-worker-process
    :type t
    :documentation "The active UIOP process info, or NIL.")
   (input
    :initform nil
    :accessor lisp-worker-input
    :type (option stream)
    :documentation "The worker's request stream.")
   (output
    :initform nil
    :accessor lisp-worker-output
    :type (option stream)
    :documentation "The worker's response stream.")
   (next-request-id
    :initform 1
    :accessor lisp-worker-next-request-id
    :type integer
    :documentation "The next protocol request identifier.")
   (lock
    :initform (make-lock "Frob Lisp worker")
    :reader lisp-worker-lock
    :documentation "The lock serializing worker protocol requests."))
  (:documentation "A persistent but disposable, heap-isolated SBCL worker."))

(-> lisp-worker-create (configuration) lisp-worker)
(defun lisp-worker-create (configuration)
  "Create a stopped disposable worker manager for CONFIGURATION."
  (make-instance 'lisp-worker :configuration configuration))

(-> lisp-worker-running-p (lisp-worker) boolean)
(defun lisp-worker-running-p (worker)
  "Return true when WORKER has a live subprocess."
  (let ((process (lisp-worker-process worker)))
    (and process (uiop:process-alive-p process) t)))

(-> lisp-worker-start (lisp-worker) lisp-worker)
(defun lisp-worker-start (worker)
  "Start WORKER when necessary and verify its protocol handshake."
  (unless (lisp-worker-running-p worker)
    (let* ((configuration (lisp-worker-configuration worker))
           (launcher (merge-pathnames "bin/frob"
                                      (configuration-source-root configuration)))
           (process
             (uiop:launch-program
              (list (namestring launcher) "--worker")
              :directory (configuration-working-directory configuration)
              :input :stream
              :output :stream
              :error-output *error-output*
              :wait nil)))
      (setf (lisp-worker-process worker) process
            (lisp-worker-input worker) (uiop:process-info-input process)
            (lisp-worker-output worker) (uiop:process-info-output process))
      (loop for line = (read-line (lisp-worker-output worker) nil nil)
            do (unless line
                 (error 'worker-error
                        :message "The Lisp worker exited before its handshake."
                        :tool-name "lisp.worker"))
            until (string= line "(:FROB-WORKER :VERSION 1)"))))
  worker)

(-> lisp-worker-stop (lisp-worker) null)
(defun lisp-worker-stop (worker)
  "Terminate WORKER and discard all process streams and heap state."
  (let ((process (lisp-worker-process worker)))
    (when process
      (when (uiop:process-alive-p process)
        (ignore-errors (uiop:terminate-process process :urgent t)))
      (ignore-errors (uiop:wait-process process))))
  (dolist (stream (list (lisp-worker-input worker)
                        (lisp-worker-output worker)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  (setf (lisp-worker-process worker) nil
        (lisp-worker-input worker) nil
        (lisp-worker-output worker) nil
        (lisp-worker-next-request-id worker) 1)
  nil)

(-> lisp-worker-reset (lisp-worker) lisp-worker)
(defun lisp-worker-reset (worker)
  "Discard WORKER's process and start a pristine replacement."
  (lisp-worker-stop worker)
  (lisp-worker-start worker))

(-> lisp-worker-request (lisp-worker keyword list) list)
(defun lisp-worker-request (worker operation arguments)
  "Send OPERATION and portable ARGUMENTS to WORKER and return its response."
  (with-lock-held ((lisp-worker-lock worker))
    (lisp-worker-start worker)
    (let* ((request-id (lisp-worker-next-request-id worker))
           (request (list :request
                          :id request-id
                          :operation operation
                          :arguments arguments)))
      (incf (lisp-worker-next-request-id worker))
      (handler-case
          (progn
            (let ((*print-readably* t)
                  (*print-circle* t))
              (prin1 request (lisp-worker-input worker))
              (terpri (lisp-worker-input worker))
              (finish-output (lisp-worker-input worker)))
            (let ((*read-eval* nil)
                  (response (read (lisp-worker-output worker) t nil)))
              (unless (and (listp response)
                           (eq (first response) :response)
                           (= (or (getf (rest response) :id) -1) request-id))
                (error 'worker-error
                       :message "The Lisp worker returned an invalid response."
                       :tool-name (format nil "lisp.~(~A~)" operation)))
              response))
        (error (condition)
          (lisp-worker-stop worker)
          (if (typep condition 'worker-error)
              (error condition)
              (error 'worker-error
                     :message (format nil "The Lisp worker protocol failed: ~A" condition)
                     :tool-name (format nil "lisp.~(~A~)" operation))))))))

(-> worker-response-tool-result (list) tool-result)
(defun worker-response-tool-result (response)
  "Convert a portable worker RESPONSE into a bounded tool result."
  (let* ((properties (rest response))
         (status (getf properties :status))
         (output (getf properties :output))
         (result-values (getf properties :values))
         (message (getf properties :message))
         (backtrace (getf properties :backtrace)))
    (if (eq status :ok)
        (tool-success
         (with-output-to-string (stream)
           (when (non-empty-string-p output)
             (format stream "Output:~%~A~%" output))
           (format stream "Values:~%~{~A~%~}" result-values)))
        (tool-failure
         (with-output-to-string (stream)
           (format stream "~A" (or message "Worker operation failed."))
           (when (non-empty-string-p backtrace)
             (format stream "~%~%Backtrace:~%~A" backtrace)))))))


;;;; -- Lisp Tool Methods --

(defmethod tool-execute ((tool lisp-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate the required form through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (tool-context-worker context)
    :eval
    (list :form (tool-argument arguments "form" :required t)))))

(defmethod tool-execute ((tool lisp-compile-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Compile and execute the required form through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (tool-context-worker context)
    :compile
    (list :form (tool-argument arguments "form" :required t)))))

(defmethod tool-execute ((tool lisp-load-system-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Load the required system through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (tool-context-worker context)
    :load-system
    (list :system (tool-argument arguments "system" :required t)))))

(defmethod tool-execute ((tool lisp-describe-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Describe the required designator through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (tool-context-worker context)
    :describe
    (list :designator (tool-argument arguments "designator" :required t)))))

(defmethod tool-execute ((tool lisp-run-tests-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run the required system's tests through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (tool-context-worker context)
    :run-tests
    (list :system (tool-argument arguments "system" :required t)))))

(defmethod tool-execute ((tool lisp-reset-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Discard CONTEXT's worker and start a pristine replacement."
  (declare (ignore tool arguments))
  (lisp-worker-reset (tool-context-worker context))
  (tool-success "The disposable Lisp worker was reset."))


;;;; -- Worker Runtime --

(-> worker-read-form (string) t)
(defun worker-read-form (source)
  "Read exactly one executable Common Lisp form from SOURCE."
  (let ((*read-eval* t)
        (*package* (find-package '#:frob))
        (end-marker (cons nil nil)))
    (multiple-value-bind (form position)
        (read-from-string source t nil)
      (let ((remainder (read-from-string source nil end-marker :start position)))
        (unless (eq remainder end-marker)
          (error "Expected exactly one Common Lisp form.")))
      form)))

(-> worker-render-value (t) string)
(defun worker-render-value (value)
  "Return a bounded readable representation of worker VALUE."
  (bounded-string
   (write-to-string value
                    :readably nil
                    :circle t
                    :level 10
                    :length 100)))

(-> worker-capture-evaluation (function) (values list string))
(defun worker-capture-evaluation (function)
  "Call FUNCTION while capturing output, returning rendered values and output."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*debug-io* stream)
                    (*package* (find-package '#:frob)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'worker-render-value result-values) output))))

(-> worker-dispatch (keyword list) (values list string))
(defun worker-dispatch (operation arguments)
  "Execute worker OPERATION with portable ARGUMENTS."
  (ecase operation
    (:eval
     (worker-capture-evaluation
      (lambda ()
        (eval (worker-read-form (getf arguments :form))))))
    (:compile
     (worker-capture-evaluation
      (lambda ()
        (funcall
         (compile nil
                  `(lambda ()
                     ,(worker-read-form (getf arguments :form))))))))
    (:load-system
     (worker-capture-evaluation
      (lambda ()
        (uiop:symbol-call '#:ql '#:quickload (getf arguments :system)))))
    (:describe
     (worker-capture-evaluation
      (lambda ()
        (describe (worker-read-form (getf arguments :designator)))
        (values))))
    (:run-tests
     (worker-capture-evaluation
      (lambda ()
        (asdf:test-system (getf arguments :system)))))))

(-> worker-condition-backtrace () string)
(defun worker-condition-backtrace ()
  "Return a bounded SBCL backtrace for the current worker condition."
  (bounded-string
   (with-output-to-string (stream)
     (sb-debug:print-backtrace :stream stream :count 20))
   :limit 6000))

(-> worker-handle-request (list) list)
(defun worker-handle-request (request)
  "Execute one portable worker REQUEST and return a protocol response."
  (let ((request-id (getf (rest request) :id))
        (operation (getf (rest request) :operation))
        (arguments (getf (rest request) :arguments)))
    (handler-case
        (multiple-value-bind (result-values output)
            (worker-dispatch operation arguments)
          (list :response
                :id request-id
                :status :ok
                :values result-values
                :output output))
      (error (condition)
        (list :response
              :id request-id
              :status :error
              :condition-type (string (type-of condition))
              :message (princ-to-string condition)
              :backtrace (worker-condition-backtrace))))))

(-> worker-main () null)
(defun worker-main ()
  "Run the isolated worker's line-oriented S-expression protocol until EOF."
  (let ((*package* (find-package '#:frob))
        (*read-eval* nil)
        (*print-readably* t)
        (*print-circle* t))
    (prin1 '(:frob-worker :version 1))
    (terpri)
    (finish-output)
    (loop for request = (read *standard-input* nil :end)
          until (eq request :end)
          do (let ((response
                     (if (and (listp request) (eq (first request) :request))
                         (worker-handle-request request)
                         (list :response
                               :id nil
                               :status :error
                               :message "Malformed worker request."
                               :backtrace ""))))
               (prin1 response)
               (terpri)
               (finish-output))))
  nil)
