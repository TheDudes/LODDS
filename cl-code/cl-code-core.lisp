;;;; cl-code.lisp

(in-package #:cl-code-core)

;;; "cl-code" goes here. Hacks and glory await!

(defun sha-256 (stream)
  "generates sha-256 sum out of given stream"
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-stream :sha256 stream)))

(defun copy-stream (stream-from stream-to &optional size)
  "will read from stream-from and write to stream-to size bytes"
  (loop
     ;; TODO: buffer-size to config
     :with written = 0
     :with buffer-size = 8192
     :with buffer = (make-array (list buffer-size)
                                :element-type '(unsigned-byte 8))
     :for read = (read-sequence buffer stream-from)
     :until (zerop read)
     :do (progn
           (if (and size
                    (> (+ read written)
                       size))
               (progn
                 (write-sequence buffer stream-to :end (- size written))
                 ;; TODO: error handling
                 (return))
               (progn
                 (write-sequence buffer stream-to :end read)
                 (incf written read)))
           ;; TODO: error handling
           (when (< read buffer-size) (return)))))
