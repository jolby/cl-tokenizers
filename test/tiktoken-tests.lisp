(in-package :tokenizers-test.tiktoken-tests)

(def-suite tiktoken-suite :in tokenizers-suite)

(in-suite tiktoken-suite)

(defparameter *test-files-directory* (asdf:system-relative-pathname :tokenizers "data/test/"))

(defparameter *cl100k-base-encoder* (get-encoder :tiktoken "cl100k_base"))
(defparameter *o200k-base-encoder* (get-encoder :tiktoken "o200k_base"))

(defparameter *test-strings*
  (list "This is a test string."
        "This is another test string."
        "This is a third test string."))

(defun get-model-test-file-path (model-name)
  (merge-pathnames (make-pathname :name model-name :type "csv" :defaults *test-files-directory*)
                   *test-files-directory*))
;; (get-model-test-file "cl100k_base_encodings")
;; (defparameter *cl100k-csv-data* (cl-csv:read-csv (get-model-test-file-path "cl100k_base_encodings")))
(defun parse-token-list-string (token-list-string)
  "Parse a bracketed list of tokens into a list of integers.
\"[1,2,3,4,5,6, 7,8,9, 10]\" => (1 2 3 4 5 6 7 8 9 10)"
  (let ((token-list-string (string-trim '(#\] #\[) token-list-string)))
    (coerce (mapcar #'parse-integer (split-sequence:split-sequence #\, token-list-string))
            'simple-vector)))
;; (parse-token-list-string "[1,2,3,4,5, 6,7,8,9, 10]")

(defun test-encoder-from-file (encoder-name file-name)
  (let ((idx 0)
        (encoder (get-encoder :tiktoken encoder-name))
        (csv-file-path (get-model-test-file-path file-name)))
    (assert (probe-file csv-file-path))
    (cl-csv:do-csv (row csv-file-path)
      (when (> idx 0)
        (let ((text (first row))
              (token-list (parse-token-list-string (second row)))
              (truncated-token-list (parse-token-list-string (third row))))
          (declare (ignore truncated-token-list))
          (is (equalp token-list (encode encoder text)))
          (is (string= text (decode encoder token-list)))))
      (incf idx))))
;; (test-encoder-from-file "cl100k_base" "cl100k_base_encodings")
;; (assert (probe-file (get-model-test-file-path "cl100k_base_encodings")))
(test tiktoken-suite-exists
  (is-true t))

(test tiktoken-cl100k-base-encoder-exists
  (is-true (not (null  *cl100k-base-encoder*))))

(test tiktoken-simple-known-encoding-value-equal
  (let ((known-encoding (encode *cl100k-base-encoder* "hello world")))
    (is (equalp known-encoding #(15339 1917)))))

(test simple-round-trip-testing-1
  (loop :for string :in *test-strings*
        :do (is (string= string (decode *cl100k-base-encoder* (encode *cl100k-base-encoder* string))))))

(test cl100k-csv-round-trip-testing
  (test-encoder-from-file "cl100k_base" "cl100k_base_encodings")
  (test-encoder-from-file "o200k_base" "o200k_base_encodings"))

;; (ql:quickload '(:tokenizers :tokenizers/tests))
;; (run! 'tokenizers-test.tiktoken-tests:tiktoken-suite-exists)
;; (run! 'tokenizers-test.tiktoken-tests::tiktoken-cl100k-base-encoder-exists)
;; (run! 'tokenizers-test.tiktoken-tests::tiktoken-simple-known-encoding-value-equal)
;; (run! 'tokenizers-test.tiktoken-tests::simple-round-trip-testing-1)
;; (run! 'tokenizers-test.tiktoken-tests::cl100k-csv-round-trip-testing)
;; (encode *cl100k-base-encoder* "hello world")
 ; => #(15339 1917)
