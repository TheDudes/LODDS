;;;; cl-code-core.lisp

(in-package #:cl-code-core)

(defun sha-256 (stream)
  "generates sha-256 sum out of given stream"
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-stream :sha256 stream)))
