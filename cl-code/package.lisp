;;;; package.lisp

(defpackage #:lodds.core
  (:use #:cl)
  (:export #:sha-256
           #:copy-stream
           #:get-timestamp
           #:str-case
           #:split-directory))

(defpackage #:lodds.watcher
  (:use #:cl)
  (:export #:watcher
           #:file-table-name
           #:file-table-hash
           #:get-all-tracked-file-infos
           #:stop-watcher))

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
  (:export ;; general functions
           #:get-interfaces
           #:get-interface-info
           #:get-broadcast-address
           #:get-ip-address
           ;; lodds-server reader/accessors
           #:shutdown-condition
           #:subsystem
           #:*subsystems*
           #:get-subsystem
           #:lodds-server
           #:name
           #:broadcast-port
           #:listener
           #:advertiser
           #:handler-port
           #:handler
           #:client-timeout
           #:interface
           #:clients
           #:current-load
           #:watchers
           #:list-of-changes
           #:advertise-timeout
           ;; lodds server methods
           #:switch-interface
           ;; subsystem stuff
           #:subsystem-start
           #:subsystem-stop

           #:remove-clients
           #:get-file-info
           #:get-user-list
           #:get-user-info
           #:get-timestamp-last-change
           #:generate-info-response
           #:get-file-changes
           #:get-shared-folders
           #:share-folder
           #:unshare-folder
           #:shutdown))
