(defpackage :tokenizers
  (:use :cl)
  (:export
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode))

(defpackage :tokenizers.tiktoken
  (:use :cl)
  (:import-from
   :alexandria
   :define-constant
   :if-let
   :when-let
   :alist-hash-table)
  (:import-from
   #:tokenizers
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode)
  (:export
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode))

