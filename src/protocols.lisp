(in-package :tokenizers)

(defgeneric get-encoder (provider-designator encoder-designator)
  (:documentation "Get an encoder for the given provider and encoder designators"))

(defgeneric get-encoder-for-model (provider-designator model-designator)
  (:documentation "Get an encoder for the given provider and model designators"))

(defgeneric encode (encoder thing-to-encode)
  (:documentation "Encode something (string, file, bytes etc.) using the given encoder"))

(defgeneric decode (encoder thing-to-decode)
  (:documentation "Decode something (string, file, array of bytes etc.) using the given encoder"))
