(in-package :cl-user)

(defpackage test-low-level-api
  (:use :cl :prove :lodds.low-level-api))
(in-package :test-low-level-api)

(plan nil)

(subtest "scanner test"
  (ok (cl-ppcre:scan lodds.low-level-api::*advertise-scanner*
                     (format nil "s0me_crazy-name@192.168.2.105:12345 0 0~C"
                             #\linefeed))
      "advertise")

  (ok (cl-ppcre:scan lodds.low-level-api::*get-scanner*
                     (format nil "get file BA6049D2A79CE5B205D930DD2920E71A72B0EDB4 0 1000~C"
                             #\linefeed))
      "get file")

  (ok (cl-ppcre:scan lodds.low-level-api::*get-scanner*
                     (format nil "get info 42~C"
                             #\linefeed))
      "get info")

  (ok (cl-ppcre:scan lodds.low-level-api::*get-scanner*
                     (format nil "get send-permission 42 24 some crazy filename.txt~C"
                             #\linefeed))
      "get send-permission")

  (ok (cl-ppcre:scan lodds.low-level-api::*info-head-scanner*
                     (format nil "all 123123 321~C"
                             #\linefeed))
      "info head all")

  (ok (cl-ppcre:scan lodds.low-level-api::*info-head-scanner*
                     (format nil "upd 321 123454~C"
                             #\linefeed))
      "info head upd")

  (ok (cl-ppcre:scan lodds.low-level-api::*info-body-scanner*
                     (format nil "add BA6049D2A79CE5B205D930DD2920E71A72B0EDB4 123454 some crazy filename~C"
                             #\linefeed))
      "info body add")

  (ok (cl-ppcre:scan lodds.low-level-api::*info-body-scanner*
                     (format nil "del BA6049D2A79CE5B205D930DD2920E71A72B0EDB4 123454 some crazy filename~C"
                             #\linefeed))
      "info body del"))

(subtest "write-string-to-stream"
  (let ((stream (flex:make-in-memory-output-stream))
        (data "sounds good"))
    (lodds.low-level-api::write-string-to-stream stream data)
    (is (flex:get-output-stream-sequence stream)
        (map 'vector #'char-code data)
        :test #'equalp
        "basic test")))

(subtest "read-line-from-stream")

(finalize)
