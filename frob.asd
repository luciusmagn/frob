(asdf:defsystem #:frob
  :description "A live, self-modifying Common Lisp agent."
  :author "Lukáš Hozda"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:cl-base64
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
                             (:file "authentication")
                             (:file "device-authentication")
                             (:file "conversation")
                             (:file "prompt")
                             (:file "provider")
                             (:file "tools")
                             (:file "lisp-worker")
                             (:file "self-tools")
                             (:file "durable-mutations")
                             (:file "generations")
                             (:file "agent")
                             (:file "terminal")
                             (:file "terminal-style")
                             (:file "line-editor")
                             (:file "stream-terminal")
                             (:file "terminal-ui")
                             (:file "application")
                             (:file "application-recovery")
                             (:file "commands")
                             (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:frob/tests))))

(asdf:defsystem #:frob/tests
  :description "Tests for Frob."
  :depends-on (#:frob)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "conversation-tests")
                             (:file "authentication-tests")
                             (:file "provider-tests")
                             (:file "tool-tests")
                             (:file "generation-tests")
                             (:file "lisp-worker-tests")
                             (:file "self-tool-tests")
                             (:file "device-authentication-tests")
                             (:file "agent-tests")
                             (:file "terminal-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:frob '#:run-tests)))
