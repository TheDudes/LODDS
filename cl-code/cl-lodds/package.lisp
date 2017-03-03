;;;; package.lisp

(defpackage #:lodds.core
  (:use #:cl)
  (:export #:generate-checksum
           #:input-rdy-p
           #:copy-stream
           #:get-timestamp
           #:str-case
           #:split-directory
           #:format-size
           #:split-user-identifier
           #:split-path
           #:add-missing-slash
           #:remove-newline
           #:get-folder
           #:format-seconds
           #:get-interfaces
           #:get-interface-info
           #:get-broadcast-address
           #:get-ip-address))

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

(defpackage #:lodds.config
  (:use #:cl)
  (:export #:*load-path*
           #:generate-default-config
           #:save-to-file
           #:validate-config
           #:update-entry
           #:load-from-file
           #:load-default-config-files
           #:get-all-keys
           #:get-value
           #:get-description
           #:get-type
           #:get-selection-options
           #:get-integer-max
           #:get-integer-min
           #:get-suggestions))

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
           #:get-load
           #:get-task-progresses
           #:get-task-by-id
           #:cancel-task
           #:run-task
           #:finish-task
           #:submit-task
           #:put-task-on-hold
           #:submit-task-from-hold
           #:remove-task-from-hold
           #:task-user
           #:task-info
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
           #:folder-already-shared-p
           #:folder-shareable-p
           #:share-folder
           #:unshare-folder
           #:folder-busy-p))

(defpackage #:lodds.event
  (:use #:cl)
  (:export #:event-queue
           #:add-callback
           #:remove-callback
           #:push-event
           #:cleanup
           #:run
           #:callback-exists-p))

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
           #:list-of-changes-lock
           #:list-of-changes
           #:advertise-timeout
           ;; functions using bound *server*
           #:get-subsystem
           #:get-load
           #:remove-clients
           #:get-timestamp-last-change
           #:get-user-list
           #:get-user-info
           #:get-user-by-ip
           #:get-file-info
           #:get-folder-info
           #:get-file-changes
           #:get-checksum-from-path
           #:shutdown
           #:generate-info-response
           #:get-file
           #:get-folder
           #:send-file
           #:send-file-user
           #:settings
           #:switch-config))
