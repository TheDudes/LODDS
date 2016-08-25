;;;; cl-code.lisp

(in-package #:cl-code)

;;; "cl-code" goes here. Hacks and glory await!

(defun sha-256 (stream)
  "generates sha-256 sum out of given stream"
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-stream :sha256 stream)))
