;;;; package.lisp

(defpackage #:lodds
  (:use #:cl))

(defpackage #:lodds-core
  (:use #:cl)
  (:export #:sha-256
           #:copy-stream
           #:get-timestamp
           #:str-case))

(defpackage #:lodds-low-level-api
  (:use #:cl
        #:lodds-core)

  (:export
   ;; broadcast
   #:send-advertise
   #:read-advertise
   ;; parses incomming requests
   #:parse-request
   ;; get family
   #:get-info
   #:get-file
   #:get-send-permission
   ;; respond family
   #:respond-info
   #:respond-file
   #:respond-send-permission
   ;; handle family
   #:handle-info
   #:handle-file
   #:handle-send-permission))
