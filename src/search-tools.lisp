(in-package #:autolith)

;;;; -- fff C ABI --

(define-constant +fff-source-commit+
  "44a5b259570730a4236ecbf06673d43ef7b2263e"
  :test #'string=
  :documentation "The reviewed fff source revision built by Autolith bootstrap.")

(define-constant +fff-create-options-version+ 2
  :documentation "The fff create-options ABI version bound by Autolith.")

(define-constant +search-default-result-limit+ 20
  :documentation "The default number of fff results returned to the model.")

(define-constant +search-maximum-result-limit+ 100
  :documentation "The largest fff result page returned to the model.")

(define-constant +search-default-time-budget-milliseconds+ 3000
  :documentation "The default fff content-search wall-clock budget.")

(define-constant +search-maximum-time-budget-milliseconds+ 10000
  :documentation "The largest fff content-search wall-clock budget.")

(defcstruct fff-result
  (success :uint8)
  (error :pointer)
  (handle :pointer)
  (int-value :int64))

(defcstruct fff-create-options
  (version :uint32)
  (base-path :pointer)
  (frecency-db-path :pointer)
  (history-db-path :pointer)
  (enable-mmap-cache :uint8)
  (enable-content-indexing :uint8)
  (watch :uint8)
  (ai-mode :uint8)
  (log-file-path :pointer)
  (log-level :pointer)
  (cache-budget-max-files :uint64)
  (cache-budget-max-bytes :uint64)
  (cache-budget-max-file-size :uint64)
  (enable-fs-root-scanning :uint8)
  (enable-home-dir-scanning :uint8)
  (follow-symlinks :uint8))

(defcfun ("fff_create_instance_with" fff--create-instance-with) :pointer
  (options :pointer))
(defcfun ("fff_destroy" fff--destroy) :void
  (handle :pointer))
(defcfun ("fff_wait_for_scan" fff--wait-for-scan) :pointer
  (handle :pointer)
  (timeout-milliseconds :uint64))
(defcfun ("fff_search" fff--search) :pointer
  (handle :pointer)
  (query :pointer)
  (current-file :pointer)
  (max-threads :uint32)
  (page-index :uint32)
  (page-size :uint32)
  (combo-boost-multiplier :int32)
  (minimum-combo-count :uint32))
(defcfun ("fff_glob" fff--glob) :pointer
  (handle :pointer)
  (pattern :pointer)
  (current-file :pointer)
  (max-threads :uint32)
  (page-index :uint32)
  (page-size :uint32))
(defcfun ("fff_live_grep" fff--live-grep) :pointer
  (handle :pointer)
  (query :pointer)
  (mode :uint8)
  (maximum-file-size :uint64)
  (maximum-matches-per-file :uint32)
  (smart-case :uint8)
  (file-offset :uint32)
  (page-limit :uint32)
  (time-budget-milliseconds :uint64)
  (before-context :uint32)
  (after-context :uint32)
  (classify-definitions :uint8))
(defcfun ("fff_multi_grep" fff--multi-grep) :pointer
  (handle :pointer)
  (patterns :pointer)
  (constraints :pointer)
  (maximum-file-size :uint64)
  (maximum-matches-per-file :uint32)
  (smart-case :uint8)
  (file-offset :uint32)
  (page-limit :uint32)
  (time-budget-milliseconds :uint64)
  (before-context :uint32)
  (after-context :uint32)
  (classify-definitions :uint8))
(defcfun ("fff_free_result" fff--free-result) :void
  (result :pointer))
(defcfun ("fff_free_search_result" fff--free-search-result) :void
  (result :pointer))
(defcfun ("fff_free_grep_result" fff--free-grep-result) :void
  (result :pointer))

(defcfun ("fff_search_result_get_count" fff--search-result-count) :uint32
  (result :pointer))
(defcfun ("fff_search_result_get_total_matched"
          fff--search-result-total-matched) :uint32
  (result :pointer))
(defcfun ("fff_search_result_get_total_files"
          fff--search-result-total-files) :uint32
  (result :pointer))
(defcfun ("fff_search_result_get_item" fff--search-result-item) :pointer
  (result :pointer)
  (index :uint32))
(defcfun ("fff_file_item_get_relative_path" fff--file-item-relative-path) :pointer
  (item :pointer))
(defcfun ("fff_file_item_get_git_status" fff--file-item-git-status) :pointer
  (item :pointer))
(defcfun ("fff_file_item_get_size" fff--file-item-size) :uint64
  (item :pointer))
(defcfun ("fff_file_item_get_total_frecency_score"
          fff--file-item-frecency) :int64
  (item :pointer))
(defcfun ("fff_file_item_get_is_binary" fff--file-item-binary-p) :uint8
  (item :pointer))

(defcfun ("fff_grep_result_get_count" fff--grep-result-count) :uint32
  (result :pointer))
(defcfun ("fff_grep_result_get_total_files_searched"
          fff--grep-result-total-files-searched) :uint32
  (result :pointer))
(defcfun ("fff_grep_result_get_total_files"
          fff--grep-result-total-files) :uint32
  (result :pointer))
(defcfun ("fff_grep_result_get_filtered_file_count"
          fff--grep-result-filtered-file-count) :uint32
  (result :pointer))
(defcfun ("fff_grep_result_get_next_file_offset"
          fff--grep-result-next-file-offset) :uint32
  (result :pointer))
(defcfun ("fff_grep_result_get_regex_fallback_error"
          fff--grep-result-regex-fallback-error) :pointer
  (result :pointer))
(defcfun ("fff_grep_result_get_match" fff--grep-result-match) :pointer
  (result :pointer)
  (index :uint32))
(defcfun ("fff_grep_match_get_relative_path"
          fff--grep-match-relative-path) :pointer
  (match :pointer))
(defcfun ("fff_grep_match_get_git_status" fff--grep-match-git-status) :pointer
  (match :pointer))
(defcfun ("fff_grep_match_get_line_content"
          fff--grep-match-line-content) :pointer
  (match :pointer))
(defcfun ("fff_grep_match_get_line_number"
          fff--grep-match-line-number) :uint64
  (match :pointer))
(defcfun ("fff_grep_match_get_col" fff--grep-match-column) :uint32
  (match :pointer))
(defcfun ("fff_grep_match_get_context_before_count"
          fff--grep-match-context-before-count) :uint32
  (match :pointer))
(defcfun ("fff_grep_match_get_context_before"
          fff--grep-match-context-before) :pointer
  (match :pointer)
  (index :uint32))
(defcfun ("fff_grep_match_get_context_after_count"
          fff--grep-match-context-after-count) :uint32
  (match :pointer))
(defcfun ("fff_grep_match_get_context_after"
          fff--grep-match-context-after) :pointer
  (match :pointer)
  (index :uint32))
(defcfun ("fff_grep_match_get_fuzzy_score"
          fff--grep-match-fuzzy-score) :uint16
  (match :pointer))
(defcfun ("fff_grep_match_get_has_fuzzy_score"
          fff--grep-match-has-fuzzy-score-p) :uint8
  (match :pointer))
(defcfun ("fff_grep_match_get_is_definition"
          fff--grep-match-definition-p) :uint8
  (match :pointer))
(defcfun ("fff_grep_match_get_is_binary" fff--grep-match-binary-p) :uint8
  (match :pointer))


;;;; -- Search Engine --

(defclass search-engine ()
  ((handle
    :initform nil
    :accessor search-engine-handle
    :type t
    :documentation "The opaque live fff instance pointer, or NIL.")
   (base-path
    :initform nil
    :accessor search-engine-base-path
    :type (option pathname)
    :documentation "The workspace currently indexed by HANDLE.")
   (lock
    :initform (make-lock "Autolith fff search engine")
    :reader search-engine-lock
    :type t
    :documentation "Serializes fff lifecycle and query calls."))
  (:documentation "One lazily initialized, workspace-aware in-process fff index."))

(defclass search-worker ()
  ((process
    :initform nil
    :accessor search-worker-process
    :type t
    :documentation "The isolated fff helper process, or NIL.")
   (input
    :initform nil
    :accessor search-worker-input
    :type t
    :documentation "The request stream connected to the helper process.")
   (output
    :initform nil
    :accessor search-worker-output
    :type t
    :documentation "The response stream connected to the helper process.")
   (next-request-id
    :initform 1
    :accessor search-worker-next-request-id
    :type (integer 1)
    :documentation "The next request identifier sent to the helper process.")
   (source-root
    :initform nil
    :accessor search-worker-source-root
    :type (option pathname)
    :documentation "The Autolith source tree used by the running helper.")
   (lock
    :initform (make-lock "Autolith fff search worker")
    :reader search-worker-lock
    :type t
    :documentation "Serializes helper lifecycle and request exchange."))
  (:documentation "One restartable process containing the native fff index and watcher."))

(defclass search-tool (workspace-tool)
  ((engine
    :initarg :engine
    :reader search-tool-engine
    :type search-worker
    :documentation "The isolated index worker shared by every search tool in one registry."))
  (:documentation "A workspace search operation backed by an isolated private fff library."))

(defclass search-files-tool (search-tool)
  ()
  (:documentation "Fuzzy-search indexed workspace file paths."))

(defclass search-glob-tool (search-tool)
  ()
  (:documentation "Filter indexed workspace file paths by one literal glob."))

(defclass search-content-tool (search-tool)
  ()
  (:documentation "Search indexed workspace file contents."))

(defclass search-multi-content-tool (search-tool)
  ()
  (:documentation "Search indexed contents for any of several literal patterns."))

(defvar *fff-library* nil
  "The process-global CFFI handle for Autolith's private fff library.")

(defvar *fff-library-path* nil
  "The pathname from which *FFF-LIBRARY* was loaded.")

(defvar *fff-library-lock* (make-lock "Autolith fff foreign library")
  "Serializes process-global loading of the private fff library.")

(-> search--fail
    (keyword string &key (:pathname (option pathname)) (:cause t))
    nil)
(defun search--fail (operation message &key pathname cause)
  "Signal a structured native search failure for OPERATION."
  (error 'search-error
         :message message
         :operation operation
         :pathname pathname
         :cause cause))

(-> search--library-path (configuration) (values pathname boolean))
(defun search--library-path (configuration)
  "Return the fff library path and whether it is an explicit override."
  (let ((override (uiop:getenv "AUTOLITH_FFF_LIBRARY")))
    (if (non-empty-string-p override)
        (values (pathname override) t)
        (values (merge-pathnames "native/fff/libfff_c.so"
                                 (configuration-data-root configuration))
                nil))))

(-> search--installed-manifest-valid-p (pathname) boolean)
(defun search--installed-manifest-valid-p (library)
  "Return true when LIBRARY's private manifest matches the bound fff ABI."
  (let ((manifest (merge-pathnames
                   "manifest.sexp"
                   (uiop:pathname-directory-pathname library))))
    (and (probe-file manifest)
         (handler-case
             (with-open-file (stream manifest
                                     :direction :input
                                     :external-format :utf-8)
               (let ((*read-eval* nil)
                     (expected (list :fff-library
                                     :version 1
                                     :commit +fff-source-commit+)))
                 (equal (read stream nil nil) expected)))
           (error ()
             nil)))))

(-> search--validated-library-path (configuration) pathname)
(defun search--validated-library-path (configuration)
  "Return CONFIGURATION's existing private fff library after identity checks."
  (multiple-value-bind (library override-p)
      (search--library-path configuration)
    (let ((library (or (probe-file library) library)))
      (unless (probe-file library)
        (search--fail
         ':load
         (format nil "The private fff library is missing at ~A; run ~A."
                 library
                 (merge-pathnames "script/bootstrap"
                                  (configuration-source-root configuration)))
         :pathname library))
      (unless (or override-p (search--installed-manifest-valid-p library))
        (search--fail
         ':load
         (format nil "The private fff library at ~A does not match revision ~A; run ~A."
                 library
                 +fff-source-commit+
                 (merge-pathnames "script/bootstrap"
                                  (configuration-source-root configuration)))
         :pathname library))
      library)))

(-> search--load-library (configuration) t)
(defun search--load-library (configuration)
  "Load and return the pinned private fff library for CONFIGURATION."
  (let ((library (search--validated-library-path configuration)))
    (with-lock-held (*fff-library-lock*)
      (cond
        ((and *fff-library* (equal library *fff-library-path*))
         *fff-library*)
        (*fff-library*
         (search--fail
          ':load
          (format nil "fff is already loaded from ~A instead of ~A."
                  *fff-library-path*
                  library)
          :pathname library))
        (t
         (handler-case
             (setf *fff-library* (load-foreign-library library)
                   *fff-library-path* library)
           (error (cause)
             (search--fail
              ':load
              (format nil "Could not load the private fff library at ~A: ~A"
                      library
                      cause)
              :pathname library
              :cause cause))))))))

(-> fff--foreign-string (t) (option string))
(defun fff--foreign-string (pointer)
  "Copy POINTER's UTF-8 C string, returning NIL for a null pointer."
  (unless (or (null pointer) (null-pointer-p pointer))
    (foreign-string-to-lisp pointer :encoding :utf-8)))

(-> fff--take-handle-result (t keyword &key (:pathname (option pathname))) t)
(defun fff--take-handle-result (result operation &key pathname)
  "Free RESULT's envelope and return its successful non-null handle."
  (when (or (null result) (null-pointer-p result))
    (search--fail operation "fff returned a null result." :pathname pathname))
  (let ((success-p nil)
        (message nil)
        (handle nil))
    (unwind-protect
         (setf success-p
               (not (zerop (foreign-slot-value result
                                               '(:struct fff-result)
                                               'success)))
               message
               (fff--foreign-string
                (foreign-slot-value result '(:struct fff-result) 'error))
               handle
               (foreign-slot-value result '(:struct fff-result) 'handle))
      (fff--free-result result))
    (unless success-p
      (search--fail operation
                    (or message "fff reported an unknown failure.")
                    :pathname pathname))
    (when (null-pointer-p handle)
      (search--fail operation
                    "fff returned a successful result without a payload."
                    :pathname pathname))
    handle))

(-> fff--take-integer-result (t keyword &key (:pathname (option pathname))) integer)
(defun fff--take-integer-result (result operation &key pathname)
  "Free RESULT's envelope and return its successful integer payload."
  (when (or (null result) (null-pointer-p result))
    (search--fail operation "fff returned a null result." :pathname pathname))
  (let ((success-p nil)
        (message nil)
        (value 0))
    (unwind-protect
         (setf success-p
               (not (zerop (foreign-slot-value result
                                               '(:struct fff-result)
                                               'success)))
               message
               (fff--foreign-string
                (foreign-slot-value result '(:struct fff-result) 'error))
               value
               (foreign-slot-value result '(:struct fff-result) 'int-value))
      (fff--free-result result))
    (unless success-p
      (search--fail operation
                    (or message "fff reported an unknown failure.")
                    :pathname pathname))
    value))

(-> fff--initialize-create-options
    (t &key (:base-path t) (:frecency-path t) (:history-path t))
    null)
(defun fff--initialize-create-options
    (options &key base-path frecency-path history-path)
  "Initialize OPTIONS for one watched AI-oriented workspace index."
  (dotimes (index (foreign-type-size '(:struct fff-create-options)))
    (setf (mem-aref options :uint8 index) 0))
  (setf (foreign-slot-value options '(:struct fff-create-options) 'version)
        +fff-create-options-version+
        (foreign-slot-value options '(:struct fff-create-options) 'base-path)
        base-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'frecency-db-path)
        frecency-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'history-db-path)
        history-path
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'enable-mmap-cache)
        1
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'enable-content-indexing)
        1
        (foreign-slot-value options '(:struct fff-create-options) 'watch)
        1
        (foreign-slot-value options '(:struct fff-create-options) 'ai-mode)
        1
        (foreign-slot-value options
                            '(:struct fff-create-options)
                            'log-file-path)
        (null-pointer)
        (foreign-slot-value options '(:struct fff-create-options) 'log-level)
        (null-pointer))
  nil)

(-> search-engine--close-unlocked (search-engine) null)
(defun search-engine--close-unlocked (engine)
  "Destroy ENGINE's live native instance while its lock is held."
  (let ((handle (search-engine-handle engine)))
    (when (and handle (not (null-pointer-p handle)))
      (fff--destroy handle)))
  (setf (search-engine-handle engine) nil
        (search-engine-base-path engine) nil)
  nil)

(-> search-engine-close (search-engine) null)
(defun search-engine-close (engine)
  "Destroy ENGINE's live native instance and watcher."
  (with-lock-held ((search-engine-lock engine))
    (search-engine--close-unlocked engine)))

(-> search-engine-detach (search-engine) null)
(defun search-engine-detach (engine)
  "Forget ENGINE's inherited pointer without touching native state after a fork."
  (setf (search-engine-handle engine) nil
        (search-engine-base-path engine) nil)
  nil)

(-> search-engine--ensure-unlocked (search-engine configuration) t)
(defun search-engine--ensure-unlocked (engine configuration)
  "Return ENGINE's index for CONFIGURATION while its lock is held."
  (search--load-library configuration)
  (let ((workspace (truename (configuration-working-directory configuration))))
    (when (and (search-engine-handle engine)
               (not (equal workspace (search-engine-base-path engine))))
      (search-engine--close-unlocked engine))
    (unless (search-engine-handle engine)
      (let* ((database-root (merge-pathnames "fff/"
                                             (configuration-cache-root
                                              configuration)))
             (frecency-path (merge-pathnames "frecency" database-root))
             (history-path (merge-pathnames "history" database-root)))
        (ensure-directories-exist database-root)
        (with-foreign-string (base-pointer (namestring workspace))
          (with-foreign-string (frecency-pointer (namestring frecency-path))
            (with-foreign-string (history-pointer (namestring history-path))
              (with-foreign-object (options '(:struct fff-create-options))
                (fff--initialize-create-options
                 options
                 :base-path base-pointer
                 :frecency-path frecency-pointer
                 :history-path history-pointer)
                (setf (search-engine-handle engine)
                      (fff--take-handle-result
                       (fff--create-instance-with options)
                       ':initialize
                       :pathname workspace)
                      (search-engine-base-path engine) workspace)))))
        (unless (plusp
                 (fff--take-integer-result
                  (fff--wait-for-scan (search-engine-handle engine) 30000)
                  ':scan
                  :pathname workspace))
          (search-engine--close-unlocked engine)
          (search--fail ':scan
                        (format nil "fff did not finish indexing ~A within 30 seconds."
                                workspace)
                        :pathname workspace))))
    (search-engine-handle engine)))

;;;; -- Result Rendering --

(-> search--annotation (string integer boolean &key (:definition-p boolean)
                        (:fuzzy-score (option integer)))
    string)
(defun search--annotation
    (git-status frecency binary-p &key definition-p fuzzy-score)
  "Return compact fff metadata annotations for one result."
  (format nil "~@[ [git:~A]~]~:[~; [definition]~]~:[~; [binary]~]~@[ [fuzzy:~D]~]~:[~; [frecency:~D]~]"
          (and (non-empty-string-p git-status) git-status)
          definition-p
          binary-p
          fuzzy-score
          (not (zerop frecency))
          frecency))

(-> search--render-file-result (t integer integer) string)
(defun search--render-file-result (result page maximum-results)
  "Render one paginated fff file or glob RESULT."
  (let* ((count (fff--search-result-count result))
         (total-matched (fff--search-result-total-matched result))
         (total-files (fff--search-result-total-files result))
         (next-page (and (< (* (1+ page) maximum-results) total-matched)
                         (1+ page))))
    (with-output-to-string (stream)
      (format stream "~:D result~:P shown, ~:D matched, ~:D indexed; page ~D.~%"
              count total-matched total-files page)
      (dotimes (index count)
        (let* ((item (fff--search-result-item result index))
               (path (or (fff--foreign-string
                          (fff--file-item-relative-path item))
                         "<unknown>"))
               (git-status (or (fff--foreign-string
                                (fff--file-item-git-status item))
                               ""))
               (frecency (fff--file-item-frecency item))
               (binary-p (not (zerop (fff--file-item-binary-p item)))))
          (format stream "~A~A [~:D bytes]~%"
                  path
                  (search--annotation git-status frecency binary-p)
                  (fff--file-item-size item))))
      (when next-page
        (format stream "next-page: ~D~%" next-page)))))

(-> search--render-context-lines (stream t integer) null)
(defun search--render-context-lines (stream match line-number)
  "Render MATCH's optional context around LINE-NUMBER to STREAM."
  (let ((before-count (fff--grep-match-context-before-count match))
        (after-count (fff--grep-match-context-after-count match)))
    (dotimes (index before-count)
      (format stream "  | ~D  ~A~%"
              (+ (- line-number before-count) index)
              (or (fff--foreign-string
                   (fff--grep-match-context-before match index))
                  "")))
    (format stream "  > ~D  ~A~%"
            line-number
            (or (fff--foreign-string (fff--grep-match-line-content match)) ""))
    (dotimes (index after-count)
      (format stream "  | ~D  ~A~%"
              (+ line-number index 1)
              (or (fff--foreign-string
                   (fff--grep-match-context-after match index))
                  ""))))
  nil)

(-> search--render-grep-result (t) string)
(defun search--render-grep-result (result)
  "Render one fff content-search RESULT with its pagination cursor."
  (let ((count (fff--grep-result-count result))
        (searched (fff--grep-result-total-files-searched result))
        (eligible (fff--grep-result-filtered-file-count result))
        (total-files (fff--grep-result-total-files result))
        (next-offset (fff--grep-result-next-file-offset result))
        (regex-error
          (fff--foreign-string
           (fff--grep-result-regex-fallback-error result))))
    (with-output-to-string (stream)
      (format stream "~:D match~:P; searched ~:D of ~:D eligible files, ~:D indexed.~%"
              count searched eligible total-files)
      (when regex-error
        (format stream "regex fallback: ~A~%" regex-error))
      (dotimes (index count)
        (let* ((match (fff--grep-result-match result index))
               (path (or (fff--foreign-string
                          (fff--grep-match-relative-path match))
                         "<unknown>"))
               (line-number (fff--grep-match-line-number match))
               (git-status (or (fff--foreign-string
                                (fff--grep-match-git-status match))
                               ""))
               (definition-p
                 (not (zerop (fff--grep-match-definition-p match))))
               (binary-p (not (zerop (fff--grep-match-binary-p match))))
               (fuzzy-score
                 (and (not (zerop (fff--grep-match-has-fuzzy-score-p match)))
                      (fff--grep-match-fuzzy-score match))))
          (format stream "~A:~D:~D~A~%"
                  path
                  line-number
                  (fff--grep-match-column match)
                  (search--annotation git-status 0 binary-p
                                      :definition-p definition-p
                                      :fuzzy-score fuzzy-score))
          (search--render-context-lines stream match line-number)))
      (when (plusp next-offset)
        (format stream "next-file-offset: ~D~%" next-offset)))))


;;;; -- Native Searches --

(-> search-engine-search-files
    (search-engine configuration string
     &key (:glob-p boolean) (:page integer) (:maximum-results integer))
    string)
(defun search-engine-search-files
    (engine configuration query &key glob-p page maximum-results)
  "Search ENGINE's workspace paths for QUERY and return a bounded page."
  (with-lock-held ((search-engine-lock engine))
    (let ((handle (search-engine--ensure-unlocked engine configuration)))
      (with-foreign-string (query-pointer query)
        (let* ((result
                 (if glob-p
                     (fff--glob handle
                                query-pointer
                                (null-pointer)
                                0
                                page
                                maximum-results)
                     (fff--search handle
                                  query-pointer
                                  (null-pointer)
                                  0
                                  page
                                  maximum-results
                                  100
                                  3)))
               (payload (fff--take-handle-result result ':files
                                                 :pathname
                                                 (search-engine-base-path engine))))
          (unwind-protect
               (search--render-file-result payload page maximum-results)
            (fff--free-search-result payload)))))))

(-> search--grep-mode (string) integer)
(defun search--grep-mode (mode)
  "Return fff's numeric content-search mode for MODE."
  (cond
    ((string= mode "plain")
     0)
    ((string= mode "regex")
     1)
    ((string= mode "fuzzy")
     2)
    (t
     (error 'tool-error
            :message "search.content mode must be plain, regex, or fuzzy."
            :tool-name "search.content"))))

(-> search-engine-search-content
    (search-engine configuration string
     &key (:mode string) (:file-offset integer) (:maximum-results integer)
     (:maximum-matches-per-file integer) (:time-budget-milliseconds integer)
     (:context-lines integer))
    string)
(defun search-engine-search-content
    (engine configuration query
     &key mode file-offset maximum-results maximum-matches-per-file
       time-budget-milliseconds context-lines)
  "Search ENGINE's workspace contents for QUERY and return one result page."
  (with-lock-held ((search-engine-lock engine))
    (let ((handle (search-engine--ensure-unlocked engine configuration)))
      (with-foreign-string (query-pointer query)
        (let* ((result
                 (fff--live-grep handle
                                 query-pointer
                                 (search--grep-mode mode)
                                 (* 10 1024 1024)
                                 maximum-matches-per-file
                                 1
                                 file-offset
                                 maximum-results
                                 time-budget-milliseconds
                                 context-lines
                                 context-lines
                                 1))
               (payload (fff--take-handle-result result ':content
                                                 :pathname
                                                 (search-engine-base-path engine))))
          (unwind-protect
               (search--render-grep-result payload)
            (fff--free-grep-result payload)))))))

(-> search-engine-search-multi-content
    (search-engine configuration list string
     &key (:file-offset integer) (:maximum-results integer)
     (:maximum-matches-per-file integer) (:time-budget-milliseconds integer)
     (:context-lines integer))
    string)
(defun search-engine-search-multi-content
    (engine configuration patterns constraints
     &key file-offset maximum-results maximum-matches-per-file
       time-budget-milliseconds context-lines)
  "Search ENGINE for lines matching any literal PATTERNS under CONSTRAINTS."
  (with-lock-held ((search-engine-lock engine))
    (let ((handle (search-engine--ensure-unlocked engine configuration))
          (joined (format nil "~{~A~^~%~}" patterns)))
      (with-foreign-string (patterns-pointer joined)
        (with-foreign-string (constraints-pointer constraints)
          (let* ((result
                   (fff--multi-grep handle
                                    patterns-pointer
                                    constraints-pointer
                                    (* 10 1024 1024)
                                    maximum-matches-per-file
                                    1
                                    file-offset
                                    maximum-results
                                    time-budget-milliseconds
                                    context-lines
                                    context-lines
                                    1))
                 (payload
                   (fff--take-handle-result
                    result
                    ':multi-content
                    :pathname (search-engine-base-path engine))))
            (unwind-protect
                 (search--render-grep-result payload)
              (fff--free-grep-result payload))))))))


;;;; -- Tool Arguments and Execution --

(-> search-tool--string-argument
    (tool json-object string &key (:required boolean) (:fallback string))
    string)
(defun search-tool--string-argument
    (tool arguments name &key required (fallback ""))
  "Return string argument NAME or signal a typed TOOL failure."
  (let ((value (tool-argument arguments name :required required)))
    (cond
      ((null value)
       fallback)
      ((stringp value)
       value)
      (t
       (error 'tool-error
              :message (format nil "~A requires string argument ~S."
                               (tool-canonical-name tool)
                               name)
              :tool-name (tool-canonical-name tool))))))

(-> search-tool--bounded-integer
    (json-object string integer integer integer)
    integer)
(defun search-tool--bounded-integer (arguments name fallback minimum maximum)
  "Return integer argument NAME clamped between MINIMUM and MAXIMUM."
  (min maximum
       (max minimum
            (or (workspace-tool-integer-argument arguments name)
                fallback))))

(-> search-tool--common-content-options (json-object) list)
(defun search-tool--common-content-options (arguments)
  "Return validated keyword options shared by content search tools."
  (list :file-offset
        (search-tool--bounded-integer arguments "file-offset" 0 0 #xffffffff)
        :maximum-results
        (search-tool--bounded-integer
         arguments
         "max-results"
         +search-default-result-limit+
         1
         +search-maximum-result-limit+)
        :maximum-matches-per-file
        (search-tool--bounded-integer arguments "max-matches-per-file" 20 1 100)
        :time-budget-milliseconds
        (search-tool--bounded-integer
         arguments
         "time-budget-ms"
         +search-default-time-budget-milliseconds+
         1
         +search-maximum-time-budget-milliseconds+)
        :context-lines
        (search-tool--bounded-integer arguments "context" 0 0 10)))
