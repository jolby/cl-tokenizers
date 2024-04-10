(defpackage :tokenizers
  (:use :cl)
  (:export
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode))

(defpackage :tokenizers.tiktoken
  (:use :cl :alexandria)
  (:import-from #:tokenizers
                #:get-encoder
                #:get-encoder-for-model
                #:encode
                #:decode)
  (:export
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode))

(defpackage :tokenizers.cli.tiktoken
  (:use :cl :clingon)
  (:import-from #:tokenizers.tiktoken
                #:get-encoder
                #:get-encoder-for-model
                #:encode
                #:decode)
  (:export :main))
