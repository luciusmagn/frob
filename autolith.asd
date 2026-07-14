(asdf:defsystem #:autolith
  :description "A live, self-modifying Common Lisp agent."
  :author "Lukáš Hozda"
  :version "0.9.9"
  :serial t
  :depends-on (#:alexandria
               #:cl-base64
               #:clinedi
               #:closer-mop
               #:dexador
               #:bordeaux-threads
               #:quri
               #:serapeum
               #:sb-posix
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "conditions")
                             (:file "json")
                             (:file "configuration")
                             (:file "preferences")
                             (:file "authentication")
                             (:file "lisp-images")
                             (:file "device-authentication")
                             (:file "conversation")
                             (:file "memories")
                             (:file "prompt")
                             (:file "provider")
                             (:file "tools")
                             (:file "memory-tools")
                             (:file "workspace-tools")
                             (:file "lisp-worker")
                             (:file "self-tools")
                             (:file "overlays")
                             (:file "durable-mutations")
                             (:file "image-commits")
                             (:file "generations")
                             (:file "agent")
                             (:file "terminal")
                             (:file "terminal-style")
                             (:file "markdown")
                             (:file "stream-terminal")
                             (:file "terminal-ui")
                             (:file "application")
                             (:file "tool-presentation")
                             (:file "application-recovery")
                             (:file "commands")
                             (:file "responsive-input")
                             (:file "main")
                             (:file "active-image"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:autolith/tests))))

(asdf:defsystem #:autolith/tests
  :description "Tests for Autolith."
  :depends-on (#:autolith)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "memory-tests")
                             (:file "preferences-tests")
                             (:file "prompt-tests")
                             (:file "conversation-tests")
                             (:file "authentication-tests")
                             (:file "provider-tests")
                             (:file "tool-tests")
                             (:file "generation-tests")
                             (:file "active-image-tests")
                             (:file "lisp-worker-tests")
                             (:file "self-tool-tests")
                             (:file "device-authentication-tests")
                             (:file "agent-tests")
                             (:file "terminal-tests")
                             (:file "markdown-tests")
                             (:file "application-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:autolith '#:run-tests)))
