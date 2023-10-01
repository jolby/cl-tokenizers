(in-package :tokenizers-test.tiktokken-tests)

(def-suite tiktokken-suite :in tokenizers-suite)

(in-suite tiktokken-suite)

(defparameter *cl100k-base-encoder* (get-encoder :tiktokken "cl100k_base"))
(defparameter *test-strings*
  (list "This is a test string."
        "This is another test string."
        "This is a third test string."))

(test tiktokken-suite-exists
  (is-true t))

(test tiktokken-cl100k-base-encoder-exists
  (is-true (not (null  *cl100k-base-encoder*))))

(test tiktokken-simple-known-encoding-value-equal
  (let ((known-encoding (encode *cl100k-base-encoder* "hello world")))
    (is (equalp known-encoding #(15339 1917)))))

(test simple-round-trip-testing-1
  (loop :for string :in *test-strings*
        :do (is (string= string (decode *cl100k-base-encoder* (encode *cl100k-base-encoder* string))))))

;; (run! 'tokenizers-test.tiktokken-tests:tiktokken-suite-exists)
;; (run! 'tokenizers-test.tiktokken-tests::tiktokken-cl100k-base-encoder-exists)
;; (run! 'tokenizers-test.tiktokken-tests::tiktokken-simple-known-encoding-value-equal)
;; (run! 'tokenizers-test.tiktokken-tests::simple-round-trip-testing-1)
;; (encode *cl100k-base-encoder* "hello world")
 ; => #(15339 1917)
