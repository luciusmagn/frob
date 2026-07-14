(defpackage #:autolith
  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:cl-base64
                #:base64-string-to-string)
  (:import-from #:cffi
                #:defcstruct
                #:defcfun
                #:foreign-slot-value
                #:foreign-string-to-lisp
                #:foreign-type-size
                #:load-foreign-library
                #:mem-aref
                #:null-pointer
                #:null-pointer-p
                #:with-foreign-object
                #:with-foreign-string)
  (:import-from #:clinedi
                #:line-editor
                #:line-editor-text
                #:line-editor-cursor
                #:line-editor-history
                #:line-editor-set-text
                #:line-editor-clear
                #:line-editor-handle-event
                #:line-editor-create
                #:selector
                #:make-selector
                #:selector-items
                #:selector-selection
                #:selector-set-items
                #:selector-arrange
                #:selector-handle-event
                #:read-event
                #:sanitize-text
                #:text-cell-width
                #:text-cell-prefix
                #:wrap-text
                #:screen-position
                #:live-region
                #:make-live-region
                #:live-region-maximum-rows
                #:live-region-row-count
                #:live-region-cursor-row
                #:live-region-cursor-column
                #:live-region-cursor-visible-p
                #:live-region-set-cursor-visible
                #:live-region-present
                #:live-region-append
                #:live-region-suspend
                #:live-region-dismiss
                #:live-region-resize
                #:call-with-live-region-suspended)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:current-thread
                #:interrupt-thread
                #:join-thread
                #:make-lock
                #:make-condition-variable
                #:make-recursive-lock
                #:make-thread
                #:thread-alive-p
                #:with-recursive-lock-held
                #:with-lock-held)
  (:import-from #:dexador.error
                #:http-request-failed
                #:response-body
                #:response-headers
                #:response-status)
  (:import-from #:quri
                #:url-encode-params)
  (:import-from #:serapeum
                #:->)
  (:import-from #:yason
                #:false)
  (:export #:main
           #:run-tests
           #:search-worker-main
           #:worker-main))

(in-package #:autolith)
