(in-package :tokenizers.cli.tiktoken)

;;;; CLI ----------------------------------------------------------------------
(defun tiktoken/options ()
  "Returns the options for the `tiktoken' command"
  (list
   (clingon:make-option
    :counter
    :description "verbosity level"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)
   (clingon:make-option
    :string
    :description "encode [STRING]. Encode string to tokens."
    :short-name #\e
    :long-name "encode"
    :key :encode)
   (clingon:make-option
    :string
    :description "decode [TOKENS]. Decode tokens to string."
    :short-name #\d
    :long-name "decode"
    :key :decode)
   ))

(defun tiktoken/handler (cmd)
  "Handler for the `tiktoken' command"
  (let ((encode (clingon:getopt cmd :encode))
        (decode (clingon:getopt cmd :decode)))
    (format t "Got encode: ~a, and decode: ~a ~%" encode decode)))

(defparameter *examples*
  '(("Encode String:" . "tiktoken -e \"Hello World\"")
    ("Decode tokens:" . "tiktoken -d 917 2343")))

(defun tiktoken/command ()
  "Encode strings to tokens or decode tokens to strings using the opeai BPE encoding."
  (clingon:make-command
   :name "tiktoken"
   :description "tiktoken is a command line tool for encoding strings to tokens and decoding tokens to strings using the opeai BPE encoding."
   :version "0.1.0"
   :authors '("Joel Boehland <jboehland@gmail.com>")
   :license "BSD 2-Clause"
   :options (tiktoken/options)
   :examples *examples*
   :handler #'tiktoken/handler))

(defun main ()
  "The main entrypoint of our CLI program"
  (let ((app (tiktoken/command)))
    (clingon:run app)))
