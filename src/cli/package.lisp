(defpackage :tokenizers.cli.tiktoken
  (:use :cl :clingon)
  (:import-from
   #:tokenizers.tiktoken
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode)
  (:export :main))
