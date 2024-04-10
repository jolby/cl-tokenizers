(in-package :tokenizers.tiktoken)

;; (ql:quickload '(:cl-ppcre :log4cl :cl-base64 :drakma))
(define-constant +endoftext+ "<|endoftext|>" :test #'string=)
(define-constant +fim-prefix+ "<|fim_prefix|>" :test #'string=)
(define-constant +fim-middle+ "<|fim_middle|>" :test #'string=)
(define-constant +fim-suffix+ "<|fim_suffix|>" :test #'string=)
(define-constant +endofprompt+ "<|endofprompt|>" :test #'string=)

(defparameter *mergable-ranks-blob-cache-dir* (uiop:xdg-cache-home "mergable-ranks-blobs"))

(defparameter *encoding-to-model*
  '(("cl100k_base" . ("gpt-4" "gpt-3.5-turbo" "text-embedding-ada-002"))
    ("r50k_base" . ("ada" "babbage" "code-search-ada-code-001" "code-search-babbage-code-001"
                    "curie" "davinci" "text-ada-001" "text-babbage-001" "text-curie-001" "text-davinci-001"
                    "text-similarity-ada-001" "text-similarity-babbage-001" "text-similarity-curie-001"
                    "text-similarity-davinci-001" "text-search-ada-doc-001" "text-search-babbage-doc-001"
                    "text-search-curie-doc-001" "text-search-davinci-doc-001"))
    ("p50k_base" . ("code-cushman-001" "code-cushman-002" "code-davinci-001" "code-davinci-002"
                    "cushman-codex" "davinci-codex" "text-davinci-002" "text-davinci-003"))
    ("p50k_edit" . ("code-davinci-edit-001" "text-davinci-edit-001"))
    ("gpt2" . ("gpt2"))))

(defparameter *codec-configs*
  '((:name "cl100k_base"
     :pat-str "(?i:'s|'t|'re|'ve|'m|'ll|'d)|[^\\r\\n\\p{L}\\p{N}]?\\p{L}+|\\p{N}{1,3}| ?[^\\s\\p{L}\\p{N}]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+"
     :mergeable-ranks-blob-url "https://openaipublic.blob.core.windows.net/encodings/cl100k_base.tiktoken"
     :special-tokens ((+endoftext+ . 100257)
                      (+fim-prefix+ . 100258)
                      (+fim-middle+ . 100259)
                      (+fim-suffix+ . 100260)
                      (+endofprompt+ . 100276)))
    (:name "p50k_edit"
     :pat-str "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"
     :mergeable-ranks-blob-url "https://openaipublic.blob.core.windows.net/encodings/p50k_base.tiktoken"
     :special-tokens '((+endoftext+ 50256)
                       (+fim-prefix+ 50281)
                       (+fim-middle+ 50282)
                       (+fim-suffix+ 50283)))
    (:name "p50k_base"
     :explicit-n-vocab 50281
     :pat-str "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"
     :mergeable-ranks-blob-url "https://openaipublic.blob.core.windows.net/encodings/p50k_base.tiktoken"
     :special-tokens '((+endoftext+ 50256)))
    (:name "r50k_base"
     :explicit-n-vocab 50257
     :pat-str "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"
     :mergable-ranks-blob-url "https://openaipublic.blob.core.windows.net/encodings/r50k_base.tiktoken"
     :special-tokens  '((+endoftext+ 50256)))))

(defun get-config-for-encoding (encoding)
  "Find the config for the encoding, signaling error if not found"
  (loop for config in *codec-configs*
        when (string= encoding (getf config :name))
        return config
        finally (error "No config found for encoding ~a" encoding)))

(defun get-config-for-model (model)
  "Using the *encoding-to-model* alist, find the encoding for the model
  signaling error if not found"
  (let ((encoding (loop for (enc . models) in *encoding-to-model*
                        when (member model models :test #'string=)
                        return enc
                        finally (error "No encoding found for model ~a" model))))
    (get-config-for-encoding encoding)))

(defun %mergable-ranks->hashtable (blobstring)
  (with-input-from-string (s blobstring)
      (let ((hash-table (make-hash-table :test 'equalp)))
        (loop for line = (read-line s nil)
              while line
              do (let* ((split-line (cl-ppcre:split "\\s+" line))
                        (token-bytes (cl-base64:base64-string-to-usb8-array (first split-line)))
                        (rank (parse-integer (second split-line))))
                   (setf (gethash token-bytes hash-table) rank)))

        hash-table)))

(defun %maybe-load-cached-tiktoken-mergeable-ranks (encoder-name)
  (let ((cache-file-path (merge-pathnames (format nil "~a.tiktoken" encoder-name) *mergable-ranks-blob-cache-dir*)))
    (if (probe-file cache-file-path)
        (let ((cache-file-string (uiop:read-file-string cache-file-path)))
          (%mergable-ranks->hashtable cache-file-string))
        nil)))

(defun load-tiktoken-mergable-ranks-blob-from-url (encoder-name url)
  (let* ((cache-file-path (merge-pathnames (format nil "~a.tiktoken" encoder-name) *mergable-ranks-blob-cache-dir*))
         (response (drakma:http-request url))
         (response-string (flexi-streams:octets-to-string response)))
    (uiop:ensure-pathname cache-file-path :ensure-directories-exist t)
    (with-open-file (f cache-file-path :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (write-sequence response-string f))
    (%mergable-ranks->hashtable response-string)))

(defun load-tiktoken-bpe (encoder-name url)
  (or (%maybe-load-cached-tiktoken-mergeable-ranks encoder-name)
      (load-tiktoken-mergable-ranks-blob-from-url encoder-name url)))

(defun %ensure-string (sym-or-kw-or-string)
  (etypecase sym-or-kw-or-string
    (string sym-or-kw-or-string)
    (symbol (symbol-name sym-or-kw-or-string))
    (keyword (keyword-name sym-or-kw-or-string))))

(defun %reverse-hashtable (hashtable)
  (let ((new-hashtable (make-hash-table
                        :test (hash-table-test hashtable)
                        :size (hash-table-size hashtable))))
    (maphash (lambda (k v) (setf (gethash v new-hashtable) k)) hashtable)
    new-hashtable))

(defclass basic-bpe-encoder ()
  ((name :reader name :initarg :name :initform (error "Must provide name"))
   (vocabulary :accessor vocabulary :initarg :vocabulary :initform (error "Must provide vocabulary"))
   (reverse-vocabulary :accessor reverse-vocabulary :initarg :reverse-vocabulary :initform (error "Must provide reverse-vocabulary"))
   (special-tokens :accessor special-tokens :initarg :special-tokens :initform (make-hash-table :test 'equalp))
   (split-regexp-str :accessor split-regexp-str :initarg :split-regexp-str :initform (error "must provide split-regexp-str"))
   (split-regexp :accessor split-regexp :initarg :split-regexp :initform (error "must provide split-regexp"))))

(defun %get-encoder (encoder-name)
  (let* ((config (get-config-for-encoding (%ensure-string encoder-name)))
         (compiled-regex (cl-ppcre:create-scanner (getf config :pat-str)))

         ;; mergable ranks maps utf8 bytes to the mergeable rank
         (mergeable-ranks (load-tiktoken-bpe encoder-name (getf config :mergeable-ranks-blob-url)))
         ;; Make reverse lookup hashtable mapping mergeable rank to utf8 bytes
         (reverse-mergeable-ranks (%reverse-hashtable mergeable-ranks))
         (special-tokens (when (getf config :special-tokens)
                           (alist-hash-table (getf config :special-tokens)))))
    (make-instance 'basic-bpe-encoder :name (getf config :name)
                                      :vocabulary mergeable-ranks
                                      :reverse-vocabulary reverse-mergeable-ranks
                                      :special-tokens special-tokens
                                      :split-regexp-str (getf config :pat-str)
                                      :split-regexp compiled-regex)))

(defun %get-encoder-for-model (model-name)
  (let  ((config (get-config-for-model (%ensure-string model-name))))
    (get-encoder (getf config :name))))

(defmethod get-encoder ((provider (eql :tiktoken)) (model-name string))
  (%get-encoder model-name))

(defmethod get-encoder-for-model ((provider (eql :tiktoken)) (model-name string))
  (%get-encoder-for-model model-name))

(defun bpe-encode (mergeable-ranks input token-buffer)
  "Perform Byte Pair Encoding on the given INPUT using MERGEABLE-RANKS hash-table."
  ;; Ensure the inputs are the correct types
  (assert (hash-table-p mergeable-ranks))
  (flet ((merge-parts (parts i)
           "Merge the parts at index I and I+1."
           (concatenate 'simple-vector
                        (subseq parts 0 i)
                        (vector (concatenate 'simple-vector (aref parts i) (aref parts (1+ i))))
                        (subseq parts (+ i 2))))
         (write-tokens-to-buffer (parts)
           (loop :for part :across parts
                 :do (if-let ((token (gethash part mergeable-ranks)))
                       (progn
                         (vector-push-extend token token-buffer))
                       (log:info "part not found: ~a" part)))))
    ;; Transformation of input
    (let ((parts (map 'simple-vector (lambda (b) (vector b)) input))
          (min-rank nil)
          (min-idx nil))
      ;; Iterative search and merging for pairs
      (loop :while t :do
        (setf min-idx nil)
        (setf min-rank nil)
        ;; Search for the best pair
        (loop :for i :below (1- (length parts)) :do
          (let* ((pair (concatenate 'simple-vector (aref parts i) (aref parts (1+ i))))
                 (rank (gethash pair mergeable-ranks)))
            ;; Comparison of the rank with the current minimum (if any)
            (when (and rank (or (not min-rank) (< rank min-rank)))
              (setf min-idx i)
              (setf min-rank rank))))
        ;; If no pair found, terminate the loop
        (when (not min-rank) (return))
        ;; Merge the lowest rank pair
        (setf parts (merge-parts parts min-idx)))
      ;; Convert parts into ranks and return
      (write-tokens-to-buffer parts))))

(defmethod encode ((encoder basic-bpe-encoder) text)
  ;;Use the regex to split the text into (approximately) words
  (let* ((parts (cl-ppcre:all-matches-as-strings (split-regexp encoder) text))
         (parts-as-utf8-bytes (mapcar #'(lambda (part) (babel:string-to-octets part :encoding :utf-8)) parts)))
    (assert (every #'string= parts (mapcar (lambda (bytes) (babel:octets-to-string bytes :encoding :utf-8)) parts-as-utf8-bytes)))
    (log:info "parts: ~a" parts)
    ;; XXX--TODO: make token buffer size informed by the size of the input
    (let ((token-buffer (make-array 1024 :element-type 'integer :adjustable t :fill-pointer 0)))
      (loop :for part :in parts-as-utf8-bytes
            :do (if-let ((special-token (gethash part (special-tokens encoder))))
                  (vector-push-extend special-token token-buffer)
                  (bpe-encode (vocabulary encoder) part token-buffer)))
      token-buffer)))

(defmethod decode ((encoder basic-bpe-encoder) tokens)
  (let ((output-bytes (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((decode-token-to-bytes (token)
             (if-let ((special-token (gethash token (special-tokens encoder))))
               special-token
               (gethash token (reverse-vocabulary encoder)))))
      (loop :for token :across tokens
            :do (let* ((token-bytes (decode-token-to-bytes token)))
                       (loop :for tkbyte :across token-bytes
                             :do (vector-push-extend tkbyte output-bytes)))))
      (babel:octets-to-string output-bytes :encoding :utf-8)))

