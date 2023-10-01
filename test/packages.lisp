(defpackage :tokenizers-test.tokenizers-tests
  (:use :cl :fiveam)
  (:export
   :tokenizers-suite
   :tokenizers-suite-exists))

(defpackage :tokenizers-test.tiktokken-tests
  (:use :cl :fiveam)
  (:import-from #:tokenizers
                #:get-encoder
                #:get-encoder-for-model
                #:encode
                #:decode)
  (:import-from
   :tokenizers-test.tokenizers-tests
   :tokenizers-suite)
  (:export
   :tiktokken-suite-exists))
