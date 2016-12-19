;;;; package.lisp

(defpackage #:lodds.core
  (:use #:cl)
  (:export #:generate-checksum
           #:copy-stream
           #:get-timestamp
           #:str-case
           #:split-directory
           #:format-size
           #:split-user-identifier))

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
           #:cleanup-fn
           #:init-args
           #:event-queue
           ;; function/methos on subsystem + server
           #:start
           #:stop))

(defpackage #:lodds.task
  (:use #:cl)
  (:export #:tasker
           #:task
           #:name
           #:run-task
           #:submit-task
           ;; task-client and accessors
           #:task-client
           #:client-name
           #:client-ip
           #:client-port
           ;; task-client-info and accessors
           #:task-client-info
           #:client-message-timestamp
           #:client-last-change
           #:client-load
           ;; task-request and accerssors
           #:task-request
           #:request-socket
           ;; task-request-file and accerssors
           #:task-request-file
           #:request-checksum
           #:request-start
           #:request-end
           #:request-written
           #:request-filename
           #:request-file-stream
           ;; task-request-info and accerssors
           #:task-request-info
           #:request-timestamp
           ;; task-request-send-permission and accerssors
           #:task-request-send-permission
           #:request-size
           #:request-timeout
           #:request-filename
           ;; task-get-file and accerssors
           #:get-local-file-path
           #:get-checksum
           #:get-size
           #:get-socket
           #:get-local-file-stream
           #:get-read-bytes
           ;; task-get-file-from-user and accerssors
           #:task-get-file-from-user
           #:get-user
           #:get-ip
           #:get-port
           ;; task-get-file-from-users and accerssors
           #:task-get-file-from-users
           #:get-current-part
           #:get-read-bytes-part
           #:get-part-size))

(defpackage #:lodds.watcher
  (:use #:cl)
  (:export #:watcher
           #:started-tracking
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
           #:cleanup
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
  (:use #:cl)
  (:export ;; global variables and macros
           #:*server*
           #:*event-queue*
           #:with-server
           ;; general functions
           #:get-interfaces
           #:get-interface-info
           #:get-broadcast-address
           #:get-ip-address
           #:update-load
           ;; client-info reader/accessor
           #:client-info
           #:c-name
           #:c-last-message
           #:c-ip
           #:c-port
           #:c-last-change
           #:c-load
           #:c-file-table-name
           #:c-file-table-hash
           #:c-lock
           ;; lodds-server reader/accessors
           #:lodds-server
           #:name
           #:broadcast-port
           #:handler-port
           #:client-timeout
           #:interface
           #:clients
           #:current-load
           #:list-of-changes-lock
           #:list-of-changes
           #:advertise-timeout
           ;; functions using bound *server*
           #:get-subsystem
           #:switch-interface
           #:remove-clients
           #:get-timestamp-last-change
           #:get-user-list
           #:get-user-info
           #:get-file-info
           #:get-file-changes
           #:shutdown
           #:generate-info-response
           #:get-file))
