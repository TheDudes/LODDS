;;;; package.lisp

(defpackage #:lodds.core
  (:use #:cl)
  (:export #:generate-checksum
           #:copy-stream
           #:get-timestamp
           #:str-case
           #:split-directory
           #:format-size
           #:split-user-identifier
           #:split-path))

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
           #:on-finish-hook
           #:run-task
           #:submit-task
           #:task-client
           #:task-client-info
           #:task-request
           #:task-request-file
           #:task-request-info
           #:task-request-send-permission
           #:get-local-file-path
           #:task-get-file-from-user
           #:task-get-file-from-users
           #:task-get-folder
           #:task-send-file))

(defpackage #:lodds.watcher
  (:use #:cl)
  (:export #:watcher
           #:started-tracking
           #:dir-watchers
           #:list-of-changes
           #:last-change
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
  (:export #:run
           #:update-client-list))

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
           #:get-folder-info
           #:get-file-changes
           #:get-checksum-from-path
           #:shutdown
           #:generate-info-response
           #:get-file
           #:get-folder
           #:send-file
           #:send-file-user))