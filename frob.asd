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
                             (:file "generations")
                             (:file "agent")
                             (:file "terminal")
                             (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:frob/tests))))

(asdf:defsystem #:frob/tests
  :description "Tests for Frob."
  :depends-on (#:frob)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests")
                             (:file "device-authentication-tests")
                             (:file "agent-tests")
                             (:file "terminal-tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:frob '#:run-tests)))
