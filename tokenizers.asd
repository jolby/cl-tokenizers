(in-package :cl-user)

(defpackage tokenizers-asd
  (:use :cl :asdf))
(in-package :tokenizers-asd)

(defsystem tokenizers
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :description "Tokenizers for embeddings, encodings, LLMs, etc. in Common Lisp."
  :depends-on (:alexandria :serapeum :log4cl :cl-ppcre :cl-ppcre-unicode :drakma :clingon)
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "protocols")
                             (:file "tiktokken"))))
  :in-order-to ((test-op (load-op "tokenizers/tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :tokenizers-suite
                                            :tokenizers-test.tokenizers-tests))
                      (error "test failure"))))

(defsystem tokenizers/tests
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :depends-on (:tokenizers :fiveam :cl-csv)
  :serial t
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "tokenizers-tests")
                             (:file "tiktokken-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :tokenizers-suite
                                        :tokenizers-test.tokenizers-tests))))

(defsystem tokenizers/tiktokken-bin
  :author "Joel Boehland <jboehland@gmail.com>"
  :version "0.1.0"
  :description "The tiktokken binary command line app."
  :depends-on (:clingon)
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:module "cli"
                              :components
                              ((:file "tiktokken"))))))
  :build-operation "program-op"
  :build-pathname "bin/tiktokken"
  :entry-point "tokenizers.cli.tiktokken:main")

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
#+(or) (ql:quickload '(:tokenizers :tokenizers/tests :tokenizers/tiktokken-bin))
#+(or)(asdf:test-system :tokenizers)
;; (tokenizers.cli.tiktokken:main)
;; (dump-system-executable :tokenizers/tiktokken-bin)
