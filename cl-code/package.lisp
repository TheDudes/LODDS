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

(defpackage #:lodds
  (:use #:cl
        #:lodds.core
        #:lodds.low-level-api)
  (:export
   #:get-interfaces
   #:get-interface-info
   #:get-broadcast-address
   #:get-ip-address
   ;; lodds-server and his methods (TODO: export accessors)
   #:lodds-server
   #:switch-interface
   #:start-listening
   #:stop-listening
   #:remove-clients
   #:start-advertising
   #:stop-advertising
   #:get-user-list
   #:get-user-info
   #:get-timestamp-last-change
   #:get-file-changes
   #:get-shared-folders
   #:share-folder
   #:unshare-folder))
