(defpackage #:autolith
  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:cl-base64
                #:base64-string-to-usb8-array
                #:base64-string-to-string
                #:usb8-array-to-base64-string)
  (:import-from #:cl-exec-sandbox
                #:external-sandbox-policy
                #:run-sandboxed
                #:sandbox-result-exit-code
                #:sandbox-result-output
                #:sandbox-result-timed-out-p
                #:workspace-write-sandbox-policy)
  (:import-from #:clifff
                #:clifff-error
                #:clifff-error-cause
                #:clifff-error-operation
                #:clifff-error-pathname
                #:make-worker
                #:worker
                #:worker-close
                #:worker-detach
                #:worker-process
                #:worker-request)
  (:import-from #:clinedi
                #:line-editor
                #:line-editor-text
                #:line-editor-cursor
                #:line-editor-history
                #:line-editor-set-text
                #:line-editor-clear
                #:line-editor-handle-event
                #:line-editor-move-vertical
                #:line-editor-create
                #:selector
                #:make-selector
                #:selector-items
                #:selector-selection
                #:selector-set-items
                #:selector-arrange
                #:selector-handle-event
                #:enable-keyboard-enhancement
                #:disable-keyboard-enhancement
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
                #:live-region-append-and-present
                #:live-region-append
                #:live-region-suspend
                #:live-region-dismiss
                #:live-region-resize)
  (:import-from #:colorlisp
                #:colorlisp-error
                #:highlight-segments
                #:language
                #:language-detect
                #:language-find
                #:native-library-path
                #:segment-category
                #:segment-text)
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
  (:import-from #:opticl
                #:8-bit-gray-alpha-image
                #:8-bit-gray-image
                #:8-bit-rgb-image
                #:8-bit-rgba-image
                #:coerce-image
                #:gray-alpha-image
                #:gray-image
                #:read-image-stream
                #:resize-image
                #:rgb-image
                #:rgba-image
                #:write-png-file)
  (:import-from #:quri
                #:url-decode
                #:url-encode-params)
  (:import-from #:serapeum
                #:->)
  (:import-from #:sbcl-workers
                #:+minimum-sbcl-worker-core-size+
                #:+pristine-sbcl-worker-image-identifier+
                #:sbcl-worker
                #:sbcl-worker-change-working-directory
                #:sbcl-worker-create
                #:sbcl-worker-environment
                #:sbcl-worker-environment-context
                #:sbcl-worker-environment-create
                #:sbcl-worker-error
                #:sbcl-worker-error-message
                #:sbcl-worker-error-operation
                #:sbcl-worker-error-pathname
                #:sbcl-worker-error-stage
                #:sbcl-worker-handle-request
                #:sbcl-worker-image
                #:sbcl-worker-image-compatible-p
                #:sbcl-worker-image-core-pathname
                #:sbcl-worker-image-error
                #:sbcl-worker-image-identifier
                #:sbcl-worker-image-load
                #:sbcl-worker-image-manifest-pathname
                #:sbcl-worker-image-note
                #:sbcl-worker-image-parent-identifier
                #:sbcl-worker-image-plausible-core-p
                #:sbcl-worker-image-publish-manifest
                #:sbcl-worker-image-scan
                #:sbcl-worker-image-staging-directory
                #:sbcl-worker-image-validate-identifier
                #:sbcl-worker-main
                #:sbcl-worker-manager-detach-inherited-processes
                #:sbcl-worker-name
                #:sbcl-worker-pool
                #:sbcl-worker-pool-change-working-directory
                #:sbcl-worker-pool-create
                #:sbcl-worker-pool-environment
                #:sbcl-worker-pool-render
                #:sbcl-worker-pool-reset
                #:sbcl-worker-pool-start
                #:sbcl-worker-pool-stop
                #:sbcl-worker-pool-stop-all
                #:sbcl-worker-pool-worker
                #:sbcl-worker-request
                #:sbcl-worker-render-value
                #:sbcl-worker-reset
                #:sbcl-worker-running-p
                #:sbcl-worker-runtime-configure
                #:sbcl-worker-save-image
                #:sbcl-worker-source
                #:sbcl-worker-start
                #:sbcl-worker-stop
                #:sbcl-worker-used-image-identifier)
  (:import-from #:sexp-store
                #:log-append
                #:log-read
                #:snapshot-read
                #:snapshot-write)
  (:import-from #:yason
                #:false)
  (:export #:main
           #:context-contributor-registrations
           #:context-status
           #:define-context-contributor
           #:make-context-contribution
           #:register-context-contributor
           #:request-context-compaction-p
           #:request-context-configuration
           #:request-context-conversation
           #:request-context-goal-context
           #:request-context-latest-user-text
           #:request-context-tool-namespaces
           #:release-server-main
           #:run-tests
           #:unregister-context-contributor
           #:worker-main))

(in-package #:autolith)
