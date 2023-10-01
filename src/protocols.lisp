(in-package :tokenizers)

(defgeneric get-encoder (provider-designator encoder-designator)
  (:documentation "Get an encoder for the given provider and encoder designators"))

(defgeneric get-encoder-for-model (provider-designator model-designator)
  (:documentation "Get an encoder for the given provider and model designators"))

(defgeneric encode (encoder text)
  (:documentation "Encode a piece of text using the given encoder"))

(defgeneric decode (encoder tokens)
  (:documentation "Decode a list of tokens using the given encoder"))
