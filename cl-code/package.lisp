;;;; package.lisp

(defpackage #:cl-code
  (:use #:cl))

(defpackage #:cl-code-core
  (:use #:cl)
  (:export #:sha-256
           #:copy-stream))

(defpackage #:cl-code-low-level-api
  (:use #:cl
        #:cl-code-core)

  (:export
   ;; broadcast
   #:send-advertise
   #:read-advertise
   ;; get family
   #:get-info-all
   #:get-info-up
   #:get-file
   #:get-send-permission
   ;; respond family
   #:respond-info-all
   #:respond-info-up
   #:respond-file
   #:respond-send-permission
   ;; handle family
   #:handle-info-all
   #:handle-info-up
   #:handle-file
   #:handle-send-permission))
