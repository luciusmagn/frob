(in-package #:frob)

;;;; -- Base Conditions --

(define-condition frob-error (error)
  ((message
    :initarg :message
    :reader frob-error-message
    :type string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected Frob failures.")
  (:report (lambda (condition stream)
             (write-string (frob-error-message condition) stream))))

(define-condition configuration-error (frob-error)
  ()
  (:documentation "A failure caused by invalid or unavailable configuration."))


;;;; -- Authentication and Provider Conditions --

(define-condition authentication-error (frob-error)
  ()
  (:documentation "The base condition for authentication failures."))

(define-condition credentials-unavailable (authentication-error)
  ((searched-paths
    :initarg :searched-paths
    :reader credentials-unavailable-searched-paths
    :type list
    :documentation "Credential pathnames inspected before the failure."))
  (:documentation "No usable model-provider credentials were found."))

(define-condition token-refresh-failed (authentication-error)
  ((status
    :initarg :status
    :reader token-refresh-failed-status
    :type (option integer)
    :documentation "The HTTP status returned by the OAuth server, if known.")
   (response
    :initarg :response
    :reader token-refresh-failed-response
    :type (option string)
    :documentation "A bounded, non-secret OAuth error response."))
  (:documentation "Refreshing a ChatGPT OAuth access token failed."))

(define-condition provider-error (frob-error)
  ((status
    :initarg :status
    :reader provider-error-status
    :type (option integer)
    :documentation "The provider HTTP status, if a response was received.")
   (request-id
    :initarg :request-id
    :reader provider-error-request-id
    :type (option string)
    :documentation "The provider request identifier, if supplied.")
   (response
    :initarg :response
    :reader provider-error-response
    :type (option string)
    :documentation "A bounded provider response safe for display."))
  (:documentation "A model-provider request failed."))

(define-condition response-stream-error (provider-error)
  ()
  (:documentation "A provider stream ended without a valid terminal event."))


;;;; -- Persistence and Tool Conditions --

(define-condition conversation-error (frob-error)
  ((pathname
    :initarg :pathname
    :reader conversation-error-pathname
    :type pathname
    :documentation "The conversation file being processed.")
   (sequence
    :initarg :sequence
    :reader conversation-error-sequence
    :type (option integer)
    :documentation "The nearest record sequence number, if known."))
  (:documentation "A conversation file is corrupt or cannot be persisted."))

(define-condition tool-error (frob-error)
  ((tool-name
    :initarg :tool-name
    :reader tool-error-tool-name
    :type string
    :documentation "The canonical dotted tool name."))
  (:documentation "A tool call could not be validated or executed."))

(define-condition worker-error (tool-error)
  ()
  (:documentation "The disposable Lisp worker failed or violated its protocol."))

(define-condition source-mutation-error (tool-error)
  ((pathname
    :initarg :pathname
    :reader source-mutation-error-pathname
    :type (option pathname)
    :documentation "The source file involved in the failed mutation."))
  (:documentation "An active-image or durable source mutation failed."))

(define-condition checkpoint-error (frob-error)
  ((stage
    :initarg :stage
    :reader checkpoint-error-stage
    :type keyword
    :documentation "The checkpoint stage that failed.")
   (pathname
    :initarg :pathname
    :reader checkpoint-error-pathname
    :type (option pathname)
    :documentation "The checkpoint artifact involved in the failure, if any."))
  (:documentation "A generation could not be validated, saved, or published."))
