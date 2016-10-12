;;;; core.lisp

(in-package #:lodds.core)

;;; "lodds.core" goes here. Hacks and glory await!

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

;; got code from lisptips http://lisptips.com/page/4 modified it to my needs :)

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0)
  "used by get-timestamp to convert universal-time to unix")

(defun get-timestamp ()
  "returns a number representing the unix-timestamp"
  (- (get-universal-time) *unix-epoch-difference*))

(defmacro str-case (string-form &body cases)
  "'case' for strings, expands to cond clause with string=
   as compare function"
  (let ((result (gensym "result")))
    `(let ((,result ,string-form))
       (declare (ignorable ,result))
       (cond
         ,@(loop
              :for (str form) :in cases
              :collect `((string= ,result ,str) ,form))))))
