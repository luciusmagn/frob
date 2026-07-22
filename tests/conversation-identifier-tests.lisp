(in-package #:autolith)

;;;; -- Conversation Identifier Tests --

(-> test-conversation-identifier-format () null)
(defun test-conversation-identifier-format ()
  "Test deterministic derivation, parsing, display, and timestamp reduction."
  (let ((cases '((0  . "13VNGTr")
                 (1  . "25eRAfG")
                 (10 . "B4JFq84")
                 (31 . "Y65Hc5f")
                 (57 . "z7435Cs"))))
    (dolist (case cases)
      (test-assert
       (string= (conversation-identifier-from-seed 3994000000 (first case))
                (rest case))
       "seed parameter derivation has stable portable vectors")))
  (test-assert
   (string= (conversation-identifier-from-seed
             (+ 3994000000 +conversation-identifier-modulus+)
             10)
            "B4JFq84")
   "Universal Time is reduced modulo 2^32")
  (test-assert (string= (conversation-identifier-normalize "K-8vQ2mp")
                        "K8vQ2mp")
               "the displayed identifier normalizes to stored form")
  (test-assert (string= (conversation-identifier-normalize "K8vQ2mp")
                        "K8vQ2mp")
               "the stored identifier normalizes unchanged")
  (test-assert (string= (conversation-identifier-display "K8vQ2mp")
                        "K-8vQ2mp")
               "stored identifiers display with one visual hyphen")
  (dolist (invalid '("K-8vQ2m" "K08vQ2m" "K-8vQ2m0" "k-8vq2m0" 42))
    (test-assert
     (handler-case
         (progn (conversation-identifier-normalize invalid) nil)
       (conversation-identifier-error ()
         t))
     "malformed or non-Base58 identifiers are rejected structurally"))
  nil)

(-> test-conversation-identifier-allocation () null)
(defun test-conversation-identifier-allocation ()
  "Test random first seeds, collision probing, and structured exhaustion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (storage (configuration-conversation-root configuration))
         (timestamp 3994000000)
         (*conversation-identifier-reservations* (make-hash-table :test #'equal))
         (*conversation-identifier-random-index-function* (lambda (limit)
                                                            (declare (ignore limit))
                                                            10)))
    (unwind-protect
         (let* ((first (conversation-identifier-generate
                        storage :timestamp timestamp))
                (second (conversation-identifier-generate
                         storage :timestamp timestamp)))
           (test-assert (string= first "B4JFq84")
                        "allocation begins at the random seed")
           (test-assert
            (string= second
                     (conversation-identifier-from-seed timestamp 11))
            "allocation probes the next seed after a collision")
           (let ((reserved
                   (loop for seed below +conversation-identifier-base+
                         collect (conversation-identifier-from-seed
                                  timestamp seed))))
             (test-assert
              (handler-case
                  (progn
                    (conversation-identifier-generate
                     storage
                     :timestamp timestamp
                     :reserved-identifiers reserved)
                    nil)
                (conversation-identifier-space-exhausted (condition)
                  (= (conversation-identifier-space-exhausted-timestamp
                      condition)
                     timestamp)))
              "occupying all 58 seeds signals structured exhaustion")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-identifiers () null)
(defun test-conversation-identifiers ()
  "Test the complete human-friendly conversation identifier subsystem."
  (test-conversation-identifier-format)
  (test-conversation-identifier-allocation)
  nil)
