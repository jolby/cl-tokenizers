(asdf:defsystem tokenizers
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :description "Tokenizers for embeddings, encodings, LLMs, etc. in Common Lisp."
  :depends-on (:alexandria :cl-ppcre :cl-ppcre-unicode :drakma)
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "tiktoken"))))
  :in-order-to ((test-op (load-op "tokenizers/tests")))
  :perform (asdf:test-op (op c)
                         (unless
                             (uiop:symbol-call
                              :fiveam :run!
                              (uiop:find-symbol* :tokenizers-suite
                                                 :tokenizers-test.tokenizers-tests))
                           (error "test failure"))))

(asdf:defsystem tokenizers/tests
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :depends-on (:tokenizers :fiveam :cl-csv)
  :serial t
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "tokenizers-tests")
                             (:file "tiktoken-tests"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call
                          :fiveam :run!
                          (uiop:find-symbol* :tokenizers-suite
                                             :tokenizers-test.tokenizers-tests))))

(asdf:defsystem tokenizers/tiktoken-bin
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :description "The tiktoken binary command line app."
  :depends-on (:clingon)
  :serial t
  :components ((:module "src"
                :components ((:module "cli"
                              :components
                              ((:file "package")
                               (:file "tiktoken"))))))
  :build-operation "program-op"
  :build-pathname "bin/tiktoken"
  :entry-point "tokenizers.cli.tiktoken:main")

(defun %this-file ()
  (asdf:system-relative-pathname
   (asdf:find-system :tokenizers) "tokenizers.asd"))
;; (%this-file)

(defun dump-system-executable (system &key (lisp-exe "sbcl"))
  (let* (;; (system (find-system system))
         (this-file (%this-file))
         (cmd (format nil "~a --load '~a' --eval '~a' --eval '~a' --eval '~a'"
                      lisp-exe
                      (namestring this-file)
                      (format nil "(ql:quickload :~a)" system)
                      (format nil "(asdf:make :~a)" system)
                      "(uiop:quit)")))
    (format t "Dumping ~a executable from ~a~%" system this-file)
    (format t "Running ~a~%" cmd)
    (uiop:run-program cmd)))

#+(or) (ql:quickload '(:tokenizers))
#+(or) (ql:quickload '(:tokenizers :tokenizers/tests :tokenizers/tiktoken-bin))
#+(or)(asdf:test-system :tokenizers)
;; (tokenizers.cli.tiktoken:main)
;; (dump-system-executable :tokenizers/tiktoken-bin)
