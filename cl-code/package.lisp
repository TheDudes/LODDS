;;;; package.lisp

(defpackage #:lodds.core
  (:use #:cl)
  (:export #:sha-256
           #:copy-stream
           #:get-timestamp
           #:str-case
           #:split-directory))

(defpackage #:lodds.low-level-api
  (:use #:cl
        #:lodds.core)
  (:export ;; broadcast
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

(defpackage #:lodds.subsystem
  (:use #:cl)
  (:export #:shutdown-condition
           #:subsystem
           ;; accessors
           #:name
           #:thread
           #:alive-p
           #:init-fn
           #:init-args
           #:event-queue
           ;; function/methos on subsystem + server
           #:start
           #:stop))

(defpackage #:lodds.watcher
  (:use #:cl)
  (:export #:watcher
           #:dir-watchers
           #:list-of-changes
           ;; #:file-table-name
           ;; #:file-table-hash
           ;; #:stop-watcher
           #:get-all-tracked-file-infos
           #:get-file-info
           #:get-shared-folders
           #:share-folder
           #:unshare-folder))

(defpackage #:lodds.event
  (:use #:cl)
  (:export #:event-queue
           #:add-callback
           #:remove-callback
           #:push-event
           #:run))

(defpackage #:lodds.listener
  (:use #:cl)
  (:export #:run))

(defpackage #:lodds.advertiser
  (:use #:cl)
  (:export #:run))

(defpackage #:lodds.handler
  (:use #:cl)
  (:export #:run))

(defpackage #:lodds
  (:use #:cl
        #:lodds.core
        #:lodds.low-level-api)
  (:export ;; global variables and macros
           #:*server*
           #:*event-queue*
           #:with-server
           ;; general functions
           #:get-interfaces
           #:get-interface-info
           #:get-broadcast-address
           #:get-ip-address
           ;; classes
           #:subsystem
           #:lodds-server
           ;; lodds-server reader/accessors
           #:name
           #:broadcast-port
           #:handler-port
           #:client-timeout
           #:interface
           #:clients
           #:current-load
           #:list-of-changes
           #:advertise-timeout
           ;; functions using bound *server*
           #:get-subsystem
           #:switch-interface
           #:remove-clients
           #:get-timestamp-last-change
           #:get-user-list
           #:get-user-info
           #:get-file-changes
           #:shutdown
           #:generate-info-response))
