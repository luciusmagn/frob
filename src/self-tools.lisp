(in-package #:autolith)

;;;; -- Active Image Inspection --

(defvar *exploratory-definitions* (make-hash-table :test #'equal)
  "Complete source forms installed exploratorily in the active image.")

(-> self-resolve-package ((option string)) package)
(defun self-resolve-package (name)
  "Return existing package NAME, defaulting to the AUTOLITH package."
  (let ((package (if (non-empty-string-p name)
                     (find-package name)
                     (find-package '#:autolith))))
    (unless package
      (error 'source-mutation-error
             :message (format nil "No active Common Lisp package is named ~S."
                              name)
             :tool-name "self.redefine"
             :pathname nil))
    package))

(-> self-call-with-package-unlocked (package function) t)
(defun self-call-with-package-unlocked (package thunk)
  "Call THUNK with PACKAGE temporarily unlocked, restoring its lock afterward."
  (let ((locked-p (sb-ext:package-locked-p package)))
    (unwind-protect
         (progn
           (when locked-p
             (sb-ext:unlock-package package))
           (funcall thunk))
      (when (and locked-p (find-package (package-name package)))
        (sb-ext:lock-package package)))))

(-> self-read-form
    (string &key (:read-eval boolean) (:package package))
    t)
(defun self-read-form
    (source &key (read-eval t) (package (find-package '#:autolith)))
  "Read exactly one Common Lisp form from SOURCE relative to PACKAGE."
  (self-call-with-package-unlocked
   package
   (lambda ()
     (let ((*read-eval* read-eval)
           (*package* package)
           (end-marker (cons nil nil)))
       (multiple-value-bind (form position)
           (read-from-string source t nil)
         (multiple-value-bind (extra ignored-position)
             (read-from-string source nil end-marker :start position)
           (declare (ignore ignored-position))
           (unless (eq extra end-marker)
             (error "Expected exactly one Common Lisp form.")))
         form)))))

(-> self-resolve-symbol (string &key (:package package)) symbol)
(defun self-resolve-symbol (name &key (package (find-package '#:autolith)))
  "Resolve readable symbol NAME relative to PACKAGE."
  (let ((value (self-read-form name :read-eval nil :package package)))
    (unless (symbolp value)
      (error "~S does not name a symbol." name))
    value))

(-> self-symbol-lambda-list (symbol) t)
(defun self-symbol-lambda-list (symbol)
  "Return SYMBOL's function lambda list when introspection can recover it."
  (when (fboundp symbol)
    (handler-case
        (let ((function (symbol-function symbol)))
          (if (typep function 'generic-function)
              (closer-mop:generic-function-lambda-list function)
              (multiple-value-bind (expression closure-p lexical-name)
                  (function-lambda-expression function)
                (declare (ignore closure-p lexical-name))
                (and (consp expression)
                     (eq (first expression) 'lambda)
                     (second expression)))))
      (error ()
        nil))))

(-> self-inspect-symbol (symbol) string)
(defun self-inspect-symbol (symbol)
  "Return structured documentation and description for active SYMBOL."
  (with-output-to-string (stream)
    (format stream "Symbol: ~S~%Package: ~A~%"
            symbol
            (or (and (symbol-package symbol)
                     (package-name (symbol-package symbol)))
                "uninterned"))
    (when (fboundp symbol)
      (format stream "Function binding: yes~%Lambda list: ~S~%Documentation: ~A~%"
              (self-symbol-lambda-list symbol)
              (or (documentation symbol 'function) "none")))
    (when (boundp symbol)
      (format stream "Value binding: yes~%Value: ~A~%Documentation: ~A~%"
              (bounded-string (symbol-value symbol) :limit 2000)
              (or (documentation symbol 'variable) "none")))
    (let ((class (find-class symbol nil)))
      (when class
        (closer-mop:finalize-inheritance class)
        (format stream "Class binding: yes~%Class documentation: ~A~%Slots:~%"
                (or (documentation symbol 'type) "none"))
        (dolist (slot (closer-mop:class-slots class))
          (format stream "  ~S~@[ - ~A~]~%"
                  (closer-mop:slot-definition-name slot)
                  (documentation slot t)))))
    (format stream "~%Describe:~%")
    (describe symbol stream)))

(defmethod tool-execute ((tool self-inspect-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Inspect one required symbol in CONTEXT's active image."
  (declare (ignore tool context))
  (tool-success
   (self-inspect-symbol
    (self-resolve-symbol
     (tool-argument arguments "symbol" :required t)))))


;;;; -- Mutation Journal --

(defvar *live-mutation-lock* (make-recursive-lock "Autolith live mutation")
  "The process-wide lock serializing active-image and durable mutations.")

(defvar *active-image-lineage-identifier* nil
  "The journal lineage receiving mutations from the running image branch.")

(defvar *image-state-initialized-p* nil
  "True after startup selected the image commit represented by this heap.")

(defvar *active-image-commit-identifier* nil
  "The private image commit represented by the running image, or NIL for base.")

(defvar *active-image-history-commit* nil
  "The private Git commit backing the running image commit, or NIL for legacy state.")

(defmacro with-live-mutation (&body body)
  "Evaluate BODY while excluding checkpoints and other live mutations."
  `(with-recursive-lock-held (*live-mutation-lock*)
     ,@body))

(-> mutation-journal-append (configuration list) list)
(defun mutation-journal-append (configuration record)
  "Append portable mutation RECORD to CONFIGURATION's journal."
  (let ((pathname (configuration-journal-path configuration))
        (entry (list* (first record)
                      :time (get-universal-time)
                      (rest record))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-circle* t)
            (*print-readably* t))
        (prin1 entry stream)
        (terpri stream)
        (finish-output stream)))
    entry))


;;;; -- Exploratory Evaluation --

(-> self-capture-evaluation (function) (values list string))
(defun self-capture-evaluation (function)
  "Call FUNCTION in the active image while capturing output and rendered values."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*package* (find-package '#:autolith)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'worker-render-value result-values) output))))

(-> self-evaluation-result (list string) string)
(defun self-evaluation-result (result-values output)
  "Render active-image RESULT-VALUES and captured OUTPUT for a tool result."
  (with-output-to-string (stream)
    (when (non-empty-string-p output)
      (format stream "Output:~%~A~%" output))
    (format stream "Values:~%~{~A~%~}" result-values)))


;;;; -- Restart Selection --

(-> self--selectable-restarts (condition) list)
(defun self--selectable-restarts (condition)
  "Return (NAME . REPORT) pairs for CONDITION's invokable restarts.

The ABORT restart is excluded because invoking it would unwind Autolith's own
event loop instead of correcting the failed operation."
  (loop for restart in (compute-restarts condition)
        for name = (restart-name restart)
        when (and name (not (eq name 'abort)))
          collect (cons (symbol-name name)
                        (princ-to-string restart))))

(-> self--find-selected-restart (condition string) t)
(defun self--find-selected-restart (condition name)
  "Return CONDITION's first non-ABORT restart named NAME, or NIL."
  (find-if (lambda (restart)
             (let ((restart-name (restart-name restart)))
               (and restart-name
                    (not (eq restart-name 'abort))
                    (string-equal (symbol-name restart-name) name))))
           (compute-restarts condition)))

(-> self--correctable-message (condition list) string)
(defun self--correctable-message (condition restarts)
  "Describe CONDITION and its RESTARTS together with retry instructions."
  (format nil
          "~A~2%Available restarts:~%~{~A~%~}~
           Retry the identical call adding \"restart\": \"NAME\" to invoke ~
           one, and add \"restart-value\" with a value form when the ~
           restart consumes a value."
          condition
          (loop for (name . report) in restarts
                collect (format nil "  ~A  ~A" name report))))

(-> self-call-with-restarts
    (function &key (:restart-name (option string))
              (:restart-value-source (option string)))
    t)
(defun self-call-with-restarts (thunk &key restart-name restart-value-source)
  "Call THUNK, invoking the chosen restart or describing the available ones.

With RESTART-NAME, the first matching non-ABORT restart is invoked while the
signaling operation is still live, optionally passing the evaluated
RESTART-VALUE-SOURCE. Without a match, a condition that offers selectable
restarts becomes a SELF-CORRECTABLE-ERROR whose report teaches the retry
protocol."
  (handler-bind
      ((error
         (lambda (condition)
           (unless (typep condition 'self-correctable-error)
             (let ((restarts (self--selectable-restarts condition)))
               (when restarts
                 (error 'self-correctable-error
                        :message (self--correctable-message condition
                                                            restarts)
                        :restart-names (mapcar #'first restarts))))))))
    (if (non-empty-string-p restart-name)
        (handler-bind
            ((error
               (lambda (condition)
                 (let ((restart (self--find-selected-restart condition
                                                             restart-name)))
                   (when restart
                     (if (non-empty-string-p restart-value-source)
                         (invoke-restart restart
                                         (eval (self-read-form
                                                restart-value-source)))
                         (invoke-restart restart)))))))
          (funcall thunk))
        (funcall thunk))))

(defmethod tool-execute ((tool self-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate one exploratory form in CONTEXT's active image and journal it."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((source (tool-argument arguments "form" :required t))
           (restart-name (tool-argument arguments "restart"))
           (restart-value-source (tool-argument arguments "restart-value"))
           (configuration (tool-context-configuration context)))
      (mutation-journal-append
       configuration
       (list :mutation :kind :eval :proposed source :result :pending))
      (handler-case
          (multiple-value-bind (result-values output)
              (self-capture-evaluation
               (lambda ()
                 (self-call-with-restarts
                  (lambda ()
                    (eval (self-read-form source)))
                  :restart-name restart-name
                  :restart-value-source restart-value-source)))
            (mutation-journal-append
             configuration
             (list :mutation :kind :eval :proposed source :result :installed))
            (tool-success (self-evaluation-result result-values output)))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :eval
                 :proposed source
                 :result :failed
                 :condition (princ-to-string condition)))
          (error condition))))))


;;;; -- Definition Installation --

(defparameter +definition-operators+
  '(defun defgeneric defmethod defmacro defclass defstruct define-condition
    deftype define-compiler-macro defvar defparameter define-constant)
  "Top-level defining operators accepted by self.redefine and source persistence.")

(-> definition-name-p (t) boolean)
(defun definition-name-p (value)
  "Return true when VALUE is a symbol or a two-part SETF function name."
  (or (symbolp value)
      (and (listp value)
           (= (length value) 2)
           (eq (first value) 'setf)
           (symbolp (second value)))))

(-> definition-form-p (t) boolean)
(defun definition-form-p (form)
  "Return true when FORM is one supported complete top-level definition."
  (and (consp form)
       (symbolp (first form))
       (member (first form) +definition-operators+ :test #'eq)
       (definition-name-p (second form))))

(-> method-specializers (list) list)
(defun method-specializers (specialized-lambda-list)
  "Return required SPECIALIZED-LAMBDA-LIST specializers without parameter names."
  (loop for parameter in specialized-lambda-list
        until (member parameter lambda-list-keywords :test #'eq)
        collect (if (consp parameter)
                    (second parameter)
                    t)))

(-> definition-signature (list) list)
(defun definition-signature (definition)
  "Return the semantic source identity of one top-level DEFINITION."
  (if (eq (first definition) 'defmethod)
      (let* ((tail (cddr definition))
             (lambda-position (position-if #'listp tail)))
        (unless lambda-position
          (error "DEFMETHOD has no lambda list."))
        (let ((qualifiers (subseq tail 0 lambda-position))
              (specialized-lambda-list (nth lambda-position tail)))
          (list (first definition)
                (second definition)
                qualifiers
                (method-specializers specialized-lambda-list))))
      (list (first definition) (second definition))))

(-> definition-key (list) string)
(defun definition-key (definition)
  "Return a stable readable key for DEFINITION's semantic signature."
  (write-to-string (definition-signature definition)
                   :readably t
                   :case :downcase))

(-> self-previous-definition (list) t)
(defun self-previous-definition (definition)
  "Return the best recoverable active representation preceding DEFINITION."
  (let ((name (second definition)))
    (or (gethash (definition-key definition) *exploratory-definitions*)
        (when (and (member (first definition)
                           '(defun defgeneric defmethod defmacro define-compiler-macro)
                           :test #'eq)
                   (fboundp name))
          (multiple-value-bind (lambda-expression closure-p lexical-name)
              (function-lambda-expression (fdefinition name))
            (declare (ignore closure-p lexical-name))
            (and lambda-expression
                 (write-to-string lambda-expression
                                  :circle t
                                  :level 10
                                  :length 100)))))))

(-> self--install-definition (list string &key (:package package)) t)
(defun self--install-definition
    (definition source &key (package (find-package '#:autolith)))
  "Compile and install parsed DEFINITION in PACKAGE, retaining complete SOURCE."
  (self-call-with-package-unlocked
   package
   (lambda ()
     (let* ((*package* package)
            (result (eval definition)))
       (setf (gethash (definition-key definition) *exploratory-definitions*)
             source)
       result))))

(-> self-replay-definition (string string) t)
(defun self-replay-definition (package-name source)
  "Read and install persisted SOURCE in PACKAGE-NAME during image reconstruction."
  (let* ((package (self-resolve-package package-name))
         (definition
           (self-read-form source :read-eval nil :package package)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "A private image commit contains an invalid definition."
             :tool-name "self.commit"
             :pathname nil))
    (if (overlay--constant-target-p (definition-key definition))
        (handler-bind
            ((error
               (lambda (condition)
                 (let ((restart (find-restart 'continue condition)))
                   (when restart
                     (invoke-restart restart))))))
          (self--install-definition definition source :package package))
        (self--install-definition definition source :package package))))

(-> self-restore-definition
    (string serious-condition
     &key (:installer function) (:package (option package)))
    t)
(defun self-restore-definition
    (previous-source original-condition
     &key (installer #'self--install-definition) package)
  "Restore PREVIOUS-SOURCE or signal compound active-image corruption."
  (handler-case
      (if package
          (funcall installer
                   (self-read-form previous-source
                                   :read-eval nil
                                   :package package)
                   previous-source
                   :package package)
          (funcall installer
                   (self-read-form previous-source :read-eval nil)
                   previous-source))
    (error (restoration-condition)
      (error 'active-image-corruption
             :message
             "A failed definition mutation could not restore the active image."
             :original-condition original-condition
             :restoration-condition restoration-condition))))

(-> self-install-definition
    (configuration string &key (:package package))
    t)
(defun self-install-definition
    (configuration source &key (package (find-package '#:autolith)))
  "Compile and install one exploratory SOURCE definition in PACKAGE."
  (with-live-mutation
    (let ((definition (self-read-form source :package package))
          (package-name (package-name package)))
      (unless (definition-form-p definition)
        (error 'source-mutation-error
               :message "self.redefine accepts one complete supported definition."
               :tool-name "self.redefine"
               :pathname nil))
      (let ((identifier (make-identifier))
            (key (definition-key definition))
            (previous (self-previous-definition definition)))
        (mutation-journal-append
         configuration
         (list :mutation
               :kind :definition
               :id identifier
               :lineage *active-image-lineage-identifier*
               :target key
               :package package-name
               :previous previous
               :proposed source
               :result :pending))
        (handler-case
            (let ((result (self--install-definition definition
                                                    source
                                                    :package package)))
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind :definition
                     :id identifier
                     :lineage *active-image-lineage-identifier*
                     :target key
                     :package package-name
                     :previous previous
                     :proposed source
                     :result :installed))
              result)
          (error (condition)
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :definition
                   :id identifier
                   :lineage *active-image-lineage-identifier*
                   :target key
                   :package package-name
                   :previous previous
                   :proposed source
                   :result :failed
                   :condition (princ-to-string condition)))
            (error condition)))))))

(defmethod tool-execute ((tool self-redefine-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install one exploratory top-level definition in CONTEXT's active image."
  (declare (ignore tool))
  (let ((source (tool-argument arguments "definition" :required t))
        (package
          (self-resolve-package (tool-argument arguments "package"))))
    (self-call-with-restarts
     (lambda ()
       (self-install-definition (tool-context-configuration context)
                                source
                                :package package))
     :restart-name (tool-argument arguments "restart")
     :restart-value-source (tool-argument arguments "restart-value"))
    (tool-success
     (format nil "The definition was compiled and installed in package ~A."
             (package-name package)))))

(defmethod tool-execute ((tool self-set-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Set one active global binding after journaling its previous value."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((identifier (make-identifier))
           (symbol (self-resolve-symbol
                    (tool-argument arguments "symbol" :required t)))
           (value-source (tool-argument arguments "value" :required t))
           (configuration (tool-context-configuration context))
           (previous (and (boundp symbol)
                          (worker-render-value (symbol-value symbol)))))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :set
             :id identifier
             :lineage *active-image-lineage-identifier*
             :target (write-to-string symbol)
             :previous previous
             :proposed value-source
             :result :pending))
      (handler-case
          (let ((value (self-call-with-restarts
                        (lambda ()
                          (let ((evaluated (eval (self-read-form value-source))))
                            (setf (symbol-value symbol) evaluated)
                            evaluated))
                        :restart-name (tool-argument arguments "restart")
                        :restart-value-source (tool-argument arguments
                                                             "restart-value"))))
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :set
                   :id identifier
                   :lineage *active-image-lineage-identifier*
                   :target (write-to-string symbol)
                   :previous previous
                   :proposed value-source
                   :result :installed))
            (tool-success
             (format nil "~S is now ~A." symbol (worker-render-value value))))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :set
                 :id identifier
                 :lineage *active-image-lineage-identifier*
                 :target (write-to-string symbol)
                 :previous previous
                 :proposed value-source
                 :result :failed
                 :condition (bounded-string condition :limit 2000)))
          (error condition))))))


;;;; -- Form-Aware Source Persistence --

(defclass source-form ()
  ((form
    :initarg :form
    :reader source-form-form
    :type t
    :documentation "The parsed top-level form.")
   (start
    :initarg :start
    :reader source-form-start
    :type integer
    :documentation "The character offset at which the form begins.")
   (end
    :initarg :end
    :reader source-form-end
    :type integer
    :documentation "The character offset immediately after the form."))
  (:documentation "One parsed top-level form and its exact source span."))

(defclass tracked-definition ()
  ((relative-pathname
    :initarg :relative-pathname
    :reader tracked-definition-relative-pathname
    :type non-empty-string
    :documentation "The definition file relative to Autolith's source root.")
   (source-form
    :initarg :source-form
    :reader tracked-definition-source-form
    :type source-form
    :documentation "The parsed definition form and its exact source span.")
   (source
    :initarg :source
    :reader tracked-definition-source
    :type string
    :documentation "The complete tracked source text of the definition."))
  (:documentation "One tracked top-level definition exposed for safe self inspection."))

(-> source--skip-block-comment (string integer) integer)
(defun source--skip-block-comment (source start)
  "Return the first offset after the nested block comment at START."
  (let ((position (+ start 2))
        (depth 1)
        (length (length source)))
    (loop while (and (< position length) (plusp depth))
          do (cond
               ((and (< (1+ position) length)
                     (char= (char source position) #\#)
                     (char= (char source (1+ position)) #\|))
                (incf depth)
                (incf position 2))
               ((and (< (1+ position) length)
                     (char= (char source position) #\|)
                     (char= (char source (1+ position)) #\#))
                (decf depth)
                (incf position 2))
               (t
                (incf position))))
    (when (plusp depth)
      (error "Unterminated block comment in source file."))
    position))

(-> source--next-form-start (string integer) integer)
(defun source--next-form-start (source start)
  "Skip whitespace and comments in SOURCE beginning at START."
  (let ((position start)
        (length (length source)))
    (loop
      (loop while (and (< position length)
                       (find (char source position)
                             '(#\Space #\Tab #\Newline #\Return #\Page)))
            do (incf position))
      (cond
        ((and (< position length) (char= (char source position) #\;))
         (let ((newline (position #\Newline source :start position)))
           (setf position (if newline (1+ newline) length))))
        ((and (< (1+ position) length)
              (char= (char source position) #\#)
              (char= (char source (1+ position)) #\|))
         (setf position (source--skip-block-comment source position)))
        (t
         (return position))))))

(-> source-read-forms (string) list)
(defun source-read-forms (source)
  "Read complete top-level forms and exact spans from SOURCE."
  (let ((stream (make-string-input-stream source))
        (position 0)
        (forms nil)
        (*read-eval* nil)
        (*package* (find-package '#:autolith)))
    (loop
      (setf position (source--next-form-start source position))
      (when (>= position (length source))
        (return (nreverse forms)))
      (file-position stream position)
      (let ((form (read stream t nil))
            (start position)
            (end (file-position stream)))
        (push (make-instance 'source-form :form form :start start :end end)
              forms)
        (setf position end)))))

(-> self-tracked-definitions (configuration symbol) list)
(defun self-tracked-definitions (configuration symbol)
  "Return complete tracked top-level definitions whose name is SYMBOL."
  (let* ((source-root (configuration-source-root configuration))
         (editable-root (merge-pathnames "src/" source-root)))
    (loop for pathname in (sort (uiop:directory-files editable-root "*.lisp")
                                #'string<
                                :key #'namestring)
          for source = (uiop:read-file-string pathname)
          append
          (loop for source-form in (source-read-forms source)
                for form = (source-form-form source-form)
                when (and (definition-form-p form)
                          (equal (second form) symbol))
                  collect
                  (make-instance
                   'tracked-definition
                   :relative-pathname (enough-namestring pathname source-root)
                   :source-form source-form
                   :source (subseq source
                                   (source-form-start source-form)
                                   (source-form-end source-form)))))))

(-> self-render-tracked-definitions (list symbol) string)
(defun self-render-tracked-definitions (definitions symbol)
  "Render complete DEFINITIONS for model inspection of SYMBOL."
  (unless definitions
    (error 'source-mutation-error
           :message (format nil "No tracked top-level definition names ~S." symbol)
           :tool-name "self.source"
           :pathname nil))
  (with-output-to-string (stream)
    (loop for definition in definitions
          for first-p = t then nil
          unless first-p
            do (format stream "~%~%")
          do (format stream
                     "~A~%~A"
                     (tracked-definition-relative-pathname definition)
                     (tracked-definition-source definition)))))

(defmethod tool-execute ((tool self-source-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return tracked source definitions for one symbol in CONTEXT."
  (declare (ignore tool))
  (let* ((package
           (self-resolve-package (tool-argument arguments "package")))
         (symbol (self-resolve-symbol
                  (tool-argument arguments "symbol" :required t)
                  :package package))
         (definitions
           (self-tracked-definitions
            (tool-context-configuration context)
            symbol)))
    (if definitions
        (tool-success (self-render-tracked-definitions definitions symbol))
        (multiple-value-bind (values output)
            (worker-source (write-to-string symbol :readably t)
                           (tool-argument arguments "kind"))
          (declare (ignore values))
          (tool-success output)))))

(-> source-definition-match-p (source-form list) boolean)
(defun source-definition-match-p (source-form definition)
  "Return true when SOURCE-FORM defines the same operator and name as DEFINITION."
  (let ((form (source-form-form source-form)))
    (and (definition-form-p form)
         (equal (definition-signature form)
                (definition-signature definition)))))

(-> source-find-definition (pathname list) (values source-form string))
(defun source-find-definition (pathname definition)
  "Return DEFINITION's parsed source form and complete file text from PATHNAME."
  (let* ((source (uiop:read-file-string pathname))
         (match (find-if
                 (lambda (source-form)
                   (source-definition-match-p source-form definition))
                 (source-read-forms source))))
    (unless match
      (error 'source-mutation-error
             :message (format nil "No matching definition exists in ~A." pathname)
             :tool-name "self.persist-definition"
             :pathname pathname))
    (values match source)))

(-> self-source-pathname (configuration string) pathname)
(defun self-source-pathname (configuration relative-name)
  "Resolve RELATIVE-NAME to an existing editable file beneath Autolith's src directory."
  (let* ((source-root (configuration-source-root configuration))
         (editable-root (merge-pathnames "src/" source-root))
         (pathname (merge-pathnames relative-name source-root)))
    (unless (and (uiop:subpathp pathname editable-root)
                 (probe-file pathname))
      (error 'source-mutation-error
             :message "Durable self modification is limited to existing files under src/."
             :tool-name "self.persist-definition"
             :pathname pathname))
    pathname))

(-> source--atomic-write (pathname string) pathname)
(defun source--atomic-write (pathname content)
  "Atomically replace PATHNAME with CONTENT through a sibling temporary file."
  (let ((temporary
          (merge-pathnames
           (format nil ".~A.~D.tmp"
                   (pathname-name pathname)
                   (sb-posix:getpid))
           (uiop:pathname-directory-pathname pathname))))
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string content stream)
      (finish-output stream))
    (uiop:rename-file-overwriting-target temporary pathname)
    pathname))

(-> source-replace-definition (pathname string) (values string string))
(defun source-replace-definition (pathname definition-source)
  "Replace one complete definition and return updated and preceding source text."
  (let ((definition (self-read-form definition-source :read-eval nil)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "The durable source is not a supported complete definition."
             :tool-name "self.persist-definition"
             :pathname pathname))
    (multiple-value-bind (match source)
        (source-find-definition pathname definition)
      (let ((previous-definition
              (subseq source
                      (source-form-start match)
                      (source-form-end match)))
            (updated
              (concatenate 'string
                           (subseq source 0 (source-form-start match))
                           definition-source
                           (subseq source (source-form-end match)))))
        (source--atomic-write pathname updated)
        (values updated previous-definition)))))
