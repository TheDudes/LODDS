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
           #:get-size-color
           #:split-user-identifier
           #:split-path
           #:add-missing-slash
           #:remove-newline
           #:format-seconds
           #:get-interfaces
           #:get-interface-info
           #:get-broadcast-address
           #:get-ip-address
           #:escaped-ensure-directories-exist
           #:escaped-get-folder-name
           #:directory-exists
           #:file-exists
           #:escape-wildcards
           #:get-absolute-path
           #:set-socket-timeout))

(defpackage #:lodds.low-level-api
  (:use #:cl
        #:lodds.core)
  (:export ;; broadcast
           #:format-send-advertise
           #:send-advertise
           #:read-advertise
           ;; parses incomming requests
           #:parse-request
           ;; get family
           #:format-get-file
           #:get-file
           #:format-get-info
           #:get-info
           #:format-get-send-permission
           #:get-send-permission
           ;; respond family
           #:format-respond-info
           #:respond-info
           #:format-respond-send-permission
           #:respond-send-permission
           ;; handle family
           #:handle-info
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

;; (defpackage #:lodds.subsystem
;;   (:use #:cl)
;;   (:export #:shutdown-condition
;;            #:subsystem
;;            ;; accessors
;;            #:name
;;            #:thread
;;            #:alive-p
;;            #:init-fn
;;            #:cleanup-fn
;;            #:init-args
;;            #:event-queue
;;            ;; function/methos on subsystem + server
;;            #:start
;;            #:stop))

;; (defpackage #:lodds.task
;;   (:use #:cl)
;;   (:export #:tasker
;;            #:task
;;            #:name
;;            #:on-finish-hook
;;            #:get-load
;;            #:get-task-progresses
;;            #:get-task-count
;;            #:get-task-by-id
;;            #:cancel-task
;;            #:run-task
;;            #:finish-task
;;            #:submit-task
;;            #:put-task-on-hold
;;            #:submit-task-from-hold
;;            #:remove-task-from-hold
;;            #:task-info
;;            #:task-request
;;            #:task-request-file
;;            #:task-request-info
;;            #:task-request-send-permission
;;            #:get-local-file-path
;;            #:task-get-file-from-user
;;            #:task-get-file-from-users
;;            #:task-get-folder
;;            #:task-send-file
;;            #:retry-task
;;            #:skip-task))

(defpackage #:lodds.watcher
  (:use #:cl)
  (:export #:watcher
           #:alive-p
           #:started-tracking
           #:dir-watchers
           #:list-of-changes
           #:last-change
           #:get-all-tracked-file-infos
           #:get-file-info
           #:get-shared-folders
           #:folder-already-shared-p
           #:folder-shareable-p
           #:share-folder
           #:unshare-folder
           #:folder-busy-p
           #:stop))

(defpackage #:lodds.event
  (:use #:cl)
  (:export #:event-queue
           #:add-callback
           #:remove-callback
           #:push-event
           #:cleanup
           #:start
           #:stop
           #:callback-exists-p))

(defpackage #:lodds.listener
  (:use #:cl)
  (:export #:start
           #:stop
           #:listener
           #:update-client-list))

(defpackage #:lodds.advertiser
  (:use #:cl)
  (:export #:send-advertise))

;; (defpackage #:lodds.handler
;;   (:use #:cl)
;;   (:export #:run))

(defpackage #:lodds.event-loop
  (:use #:cl)
  (:export #:event-loop
           #:get-load
           #:get-task-progresses
           #:get-task-count
           #:get-task-by-id
           #:with-event-loop
           #:task-get-file-from-user
           #:task-get-file-from-users
           #:task-get-folder
           #:task-send-file
           #:ev-task-init
           #:ev-task-run
           #:ev-update-user-info
           #:task-load
           #:task-cancel
           #:task-info
           #:task-load
           #:start
           #:stop))

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
           #:start
           #:stop
           #:shutdown
           #:generate-info-response
           #:find-best-user
           #:get-file
           #:get-folder
           #:send-file
           #:send-file-user
           #:settings
           #:switch-config
           #:update-config
           #:get-status
           #:user-is-trusted
           #:user-is-blocked
           #:untrust-user
           #:trust-user
           #:unblock-user
           #:block-user
           #:get-event-loop
           #:get-watcher
           #:get-event-queue
           #:get-listener))
