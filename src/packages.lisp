(defpackage :tokenizers
  (:use :cl)
  (:export
   #:get-encoder
   #:get-encoder-for-model
   #:encode
   #:decode))

(defpackage :tokenizers.tiktokken
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

(defpackage :tokenizers.cli.tiktokken
  (:use :cl :clingon)
  (:import-from #:tokenizers.tiktokken
                #:get-encoder
                #:get-encoder-for-model
                #:encode
                #:decode)
  (:export :main))
