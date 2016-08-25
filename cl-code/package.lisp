;;;; package.lisp

(defpackage #:cl-code
  (:use #:cl))

(defpackage #:cl-code-core
  (:use #:cl)
  (:export #:sha-256))

(defpackage #:cl-code-low-level-api
  (:use #:cl #:cl-code-core)
  (:export

   #:send-advertise
   #:read-advertise

   #:get-info-all
   #:get-info-up
   #:get-info-load
   #:get-file
   #:get-send-permission

   #:respond-info-all
   #:respond-info-up
   #:respond-info-load
   #:respond-file
   #:respond-send-permission

   #:handle-info-all
   #:handle-info-up
   #:handle-info-load
   #:handle-file
   #:handle-send-permission))
