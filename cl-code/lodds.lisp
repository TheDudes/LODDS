;;;; lodds.lisp

(in-package #:lodds)

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop
     :for interface :in (ip-interfaces:get-ip-interfaces-by-flags
                         '(:iff-up :iff-running))
     :collect (ip-interfaces:ip-interface-name interface)))

(defun get-interface-info (interface)
  "returns the specified interface"
  (find interface (ip-interfaces:get-ip-interfaces-by-flags
                   '(:iff-up :iff-running))
        :key #'ip-interfaces:ip-interface-name
        :test #'string=))

(defun get-broadcast-address (interface)
  "returns the broadcast address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (ip-interfaces:ip-interface-broadcast-address
   (get-interface-info interface)))

(defun get-ip-address (interface)
  "returns the ip address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (ip-interfaces:ip-interface-address
   (get-interface-info interface)))

(stmx:transactional
 (defclass lodds-server ()
   ((name
     :accessor name
     :initarg :name
     :initform nil
     :type string
     :transactional nil
     :documentation "Advertised name. Will be displayed by other
                    Clients as ur name.")
    (listening-ip
     :reader listening-ip
     :initform nil
     :type fixnum
     :documentation "Transactional variable, will be set by
                    SWITCH-INTERFACE inside a stmx:atomic block. Do
                    not set by hand.")
    (listening-port
     :accessor listening-port
     :initarg :listening-port
     :initform 4567
     :transactional nil
     :documentation "Port the LODDS-SERVER listens on. Listening
                    (STOP/START-LISTENING) has to be restarted for
                    changes to take effect")
    (broadcast-ip
     :reader broadcast-ip
     :initform nil
     :documentation "Transactional variable, IP LODDS-SERVER
                    Advertises information, will be set by
                    SWITCH-INTERFACE inside a stmx:atomic block. Do
                    not set by hand.")
    (broadcast-port
     :accessor broadcast-port
     :initarg :broadcast-port
     :initform 9002
     :transactional nil
     :documentation "Port the LODDS-SERVER advertises to. Broadcasting
                    (STOP/START-BROADCASTING) has to be restarted for
                    changes to take effect")
    (client-timeout
     :accessor client-timeout
     :initarg :client-timeout
     :initform 5
     :type integer
     :transactional nil
     :documentation "Timeout till client gets delete from local
                    list. Each advertise from other Clients is saved
                    with a timestamp, if timestamp is older than
                    CLIENT-TIMEOUT, the client will be deleted.")
    (interface
     :reader interface
     :initform nil
     :type string
     :documentation "Transactional (STMX)!
                    Currently selected interface. To get a list of
                    available interface use GET-INTERFACES. Use
                    SWITCH-INTERFACE to change, or set, the
                    interface. SWITCH-INTEFACE will switch the value
                    inside a stmx:atomic.")
    (broadcast-listener
     :accessor broadcast-listener
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "BROADCAST-LISTENER server object. If this member
                    variable is nil the Server is not listening to
                    broadcast messages of other clients. Use
                    START-LISTEINING and STOP-LISTENING and do not set
                    this member by hand, since its spawning a thread
                    and manipulating the CLIENT member.")
    (broadcast-advertiser
     :accessor broadcast-advertiser
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "BROADCAST-ADVERTISER broadcasts information to
                    other clients.")
    (handler-socket
     :reader handler-socket
     :initform nil
     :type usocket:socket
     :transactional nil
     :documentation "Server Socket which listens for incomming connections.")
    (handler-thread
     :reader handler-thread
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "Thread which listens for incomming connections.")
    (clients
     :accessor clients
     :initform (make-hash-table :test #'equalp)
     :type hashtable
     :transactional nil
     :documentation "hashtable containing all clients which their
                    broadcast information. This table is updated by
                    BROADCAST-LISTENER.
                    TODO: implement something to retrieve a copy.")
    (current-load
     :initform 0
     :accessor current-load
     :type integer
     :documentation "Transactional (STMX)!
                    current load, this variable describes the sum of
                    all outstanding bytes which need to be
                    transfered. Do NOT set this variable, retrieving
                    it should be fine. TODO: who sets this?")
    (watchers
     :reader watchers
     :initform nil
     :type list
     :transactional nil
     :documentation "List of Directory watchers")
    (list-of-changes
     :accessor list-of-changes
     :initform '()
     :type list
     :documentation "list of changes. Each member is a list of
                     Timestamp, Type, checksum, size and name in that
                     order.")
    (advertise-timeout
     :accessor advertise-timeout
     :initform 1
     :transactional nil
     :documentation "Specifies timeout between
                    advertisements. Specified in seconds."))))

(defgeneric switch-interface (server interface)
  (:documentation
   "Switch server interface and set addresses (ip and broadcast ip)
    accordingly. Interface is a string, to retrieve a list of
    available interfaces use GET-INTERFACES. Ports wont be
    set. SWITCH-INTEFFACE will check if a server is running and
    restart it. For example, (SWITCH-INTERFACE \"enp0s25\") will stop
    the broadcasting listener, switch the interface, switch broadcast
    and listening ip accordingly and start the listener again. If the
    Listener was not running it will not start it."))

(defgeneric switch-advertise-timeout (server timeout)
  (:documentation
   "switch ADVERTISE-TIMEOUT, TIMEOUT is given in seconds."))

(defgeneric start-listening (server)
  (:documentation
   "Will start listening for broadcast messages of other clients,
   it will spawn a seperate thread which updates a hashtable inside
   server (accessable through :clients)."))

(defgeneric stop-listening (server)
  (:documentation
   "Stops the given server from listening on broadcast address."))

(defgeneric remove-clients (server inactive-time)
  (:documentation
   "removes all clients which a longer inactive then INACTIVE-TIME"))

(defgeneric get-timestamp-last-change (server)
  (:documentation
   "returns the timestamp of the last change, who would have thought?
   :D"))

(defgeneric start-advertising (server)
  (:documentation
   "Start advertising information about server on BROADCAST-IP and
   BROADCAST-PORT. use SWITCH-INTERFACE to set the ip. This function
   will spawn a thread."))

(defgeneric stop-advertising (server)
  (:documentation
   "Stop advertising information about server."))

(defgeneric get-user-list (server)
  (:documentation
   "Returns List of users who advertised themselfs.

    CL-USER> (get-user-list *lodds-server*)
    => (\"peter\" \"steve\" \"josh\")

    Use GET-USER-INFO to get information about a user."))

(defgeneric get-user-info (server user)
  (:documentation
   "Returns List with information about an given user.
    To get a list of available users see get-user-list.

    CL-USER> (get-user-info *lodds-server* \"someUser\")
    => (1475670931 #(172 19 246 14) 4567 0 0 \"someUser\")

    List contains timestamp-received, ip, port, timestamp-client,
    load, name in that order. The first timestamp describes a local
    timestamp when the message was received.

    Returns nil if user is not found."))

(defgeneric get-shared-folders (server)
  (:documentation
   "returns a list of all currently shared folders."))

(defgeneric share-folder (server folder-path)
  (:documentation
   "share a given folder, adds a watcher to handle updates."))

(defgeneric unshare-folder (server &optional folder-path)
  (:documentation
   "unshare the given folder, if folder-path is nil (or not specified)
   all folders will be removed"))

(defgeneric get-file-changes (server current-timestamp &optional timestamp)
  (:documentation
   "returns a list of all changes since the given timestamp. if
   timestamp is nil a full list of all files will be returnd"))

(defgeneric shutdown (server)
  (:documentation
   "shuts down the whole server, removes all handles and joins all
   spawned threads."))

(defmethod switch-interface ((server lodds-server) (interface string))
  (let ((interface-info (get-interface-info interface)))
    (unless (null interface-info)
      (with-accessors ((ip ip-interfaces:ip-interface-address)
                       (bc ip-interfaces:ip-interface-broadcast-address))
          (get-interface-info interface)
        (let ((was-listening nil))
          (when (broadcast-listener server)
            (stop-listening server)
            (setf was-listening t))
          (stmx:atomic
           (setf (slot-value server 'interface) interface
                 (slot-value server 'listening-ip) ip
                 (slot-value server 'broadcast-ip) bc))
          (when was-listening
            (start-listening server))))
      interface)))

(defun broadcast-listener-callback (buffer server)
  "handles broadcast received messages. This function is a callback
  for the BROADCAST-LISTENER. It is set by START-LISTENING"
  (format t "broadcast: ~a ~a~%"
          (get-timestamp)
          (flexi-streams:octets-to-string buffer))
  (multiple-value-bind (error result) (read-advertise buffer)
    (unless (eql error 0)
      (format t "TODO: remove me: ERROR from read-advertise != 0~%")
      (return-from broadcast-listener-callback))
    (let ((current-time (get-timestamp))
          (clients (clients server)))
      ;; remove all clients older then :client-timeout
      (maphash (lambda (key val)
                 (when (> (- current-time (car val))
                          (client-timeout server))
                   (remhash key clients)))
               clients)
      ;; add client
      (setf (gethash (car (last result)) (clients server))
            (cons current-time result)))))

(defmethod start-listening ((server lodds-server))
  ;; TODO: potentional Bug here when START-LISTENING gets called twice,
  ;; the whole if should be atomic, but is it needed? What about blocking this function
  ;; while its beeing processed by another thread? could this even happen?
  (if (broadcast-listener server)
      ;; TODO: error message or something here
      (format t "broadcast listener already running.~%")
      (let* ((b-ip (broadcast-ip server))
             (b-port (broadcast-port server)))
        (if (or (null b-ip)
                (null b-port))
            ;; TODO: implement error handling here.
            (format t "error: broadcast ip or port not set!~%")
            (setf (broadcast-listener server)
                  (usocket:socket-server
                   b-ip
                   b-port
                   (lambda (buffer)
                     (broadcast-listener-callback buffer server)
                     ;; Sooooooooooooo, i found out that everything i
                     ;; this function returns gets sent, so i get a
                     ;; error if its not a buffer of (unsigned-byte 8)
                     ;; :D
                     nil)
                   nil
                   :in-new-thread t
                   :protocol :datagram))))))

(defmethod stop-listening ((server lodds-server))
  ;; TODO: Same bug as START-LISTENING
  (if (null (broadcast-listener server))
      (format t "broadcast listener not running.~%")
      ;; TODO this destroy has to go away
      (progn
        (bt:destroy-thread (broadcast-listener server))
        (setf (broadcast-listener server) nil))))

(defmethod remove-clients ((server lodds-server) (inactive-time fixnum))
  (let ((remove-me (list))
        (current-time (get-timestamp)))
    (maphash (lambda (key value)
               (when (> (- current-time (car value))
                        inactive-time)
                 (push key remove-me)))
             (clients server))
    (mapcar (lambda (key)
              (remhash key (clients server)))
            remove-me)))

(defmethod get-timestamp-last-change ((server lodds-server))
  (car (car (list-of-changes server))))

(defun advertiser (server)
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (send-advertise
   (broadcast-ip server)
   (broadcast-port server)
   (list (listening-ip server)
         (listening-port server)
         (get-timestamp-last-change server)
         (current-load server)
         (name server))))

(defun generate-info-response (server timestamp)
  (let* ((type (if (eql 0 timestamp)
                   :all
                   :upd))
         (current-timestamp (get-timestamp))
         (file-infos (get-file-changes server
                                       current-timestamp
                                       (case type
                                         (:all nil)
                                         (:upd timestamp)))))
    (list type
          current-timestamp
          file-infos)))

(defun get-file-info (server checksum)
  "returns a list with information about the requested file. if file
  with requested checksum is not found nil will be returned"
  (car
   (loop
      :for watcher :in (watchers server)
      :when (gethash checksum
                     (lodds.watcher:file-table-hash watcher))
      :return it)))

(defun handler (stream server)
  "handles incomming connections and starts threads to handle
  requests"
  (declare (type stream stream))
  (handler-case
      (multiple-value-bind (error request)
          (lodds.low-level-api:parse-request stream)
        (if (eql 0 error)
            (case (car request)
              (:file
               (destructuring-bind (checksum start end)
                   (cdr request)
                 (let ((filename (get-file-info server checksum)))
                   (if filename
                       (with-open-file (file-stream filename
                                                    :direction :input)
                         (lodds.low-level-api:respond-file stream
                                                           file-stream
                                                           start end))
                       (format t "TODO: could not find file!!~%")))))
              (:info (apply #'lodds.low-level-api:respond-info
                            stream
                            (generate-info-response server (cadr request))))
              (:send-permission
               (destructuring-bind (size timeout filename)
                   (cdr request)
                 (declare (ignore timeout))
                 ;; TODO: ask user here for file path
                 (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                              :direction :output
                                              :if-exists :supersede)
                   (lodds.low-level-api:respond-send-permission stream
                                                                file-stream
                                                                size)))))
            (error "low level api returned error ~a~%" error)))
    ;; TODO: error handling
    (end-of-file ()
      (format t "TODO: tcp: got end of file~%"))
    (error (e)
      (format t "TODO: tcp: error occured: ~a~%" e))))

(defmethod start-advertising ((server lodds-server))
  ;;TODO: same as START-LISTENING, could be a bug here
  (if (broadcast-advertiser server)
      ;; TODO: some kind of error handling here
      (format t "already advertising!~%")
      (setf (broadcast-advertiser server)
            (bt:make-thread
             (lambda ()
               (loop
                  :while t
                  :with timeout
                  :do (progn
                        ;; repull timeout to get changes
                        (setf timeout (advertise-timeout server))
                        (advertiser server)
                        (sleep timeout)))
               :name "broadcast-advertiser"))))
  (if (handler-thread server)
      (multiple-value-bind (thread socket)
          (usocket:socket-server (listening-ip server)
                                 (listening-port server)
                                 ;; TODO: remove lambda if i dont recompile handler anymore
                                 (lambda (stream server)
                                   (handler stream server))
                                 (list server)
                                 :in-new-thread t
                                 :reuse-address t)
        (setf (slot-value server 'handler-thread) thread
              (slot-value server 'handler-socket) socket))))

(defmethod stop-advertising ((server lodds-server))
  ;; TODO: Same here as START-ADVERTISING, what happens if it gets called twice?
  (if (broadcast-advertiser server)
      ;; TODO: this destroy has to go away
      (bt:destroy-thread (broadcast-advertiser server))
      ;; TODO: some error handling here
      (format t "advertising not running~%"))
  (when (handler-socket server)
    (usocket:socket-close (handler-socket server)))
  (let ((thread (handler-thread server)))
    (when (and thread
               (bt:thread-alive-p thread))
      (bt:join-thread (handler-thread server))))
  (setf (broadcast-advertiser server) nil
        (slot-value server 'handler-thread) nil
        (slot-value server 'handler-socket) nil))

(defmethod get-user-list ((server lodds-server))
  (loop
     :for key :being :the :hash-key :of (clients server)
     :collect key))

(defmethod get-user-info ((server lodds-server) (user string))
  (gethash user (clients server)))

(defmethod get-shared-folders ((server lodds-server))
  (mapcar #'cl-fs-watcher:dir (watchers server)))

(defmethod share-folder ((server lodds-server) (folder-path string))
  ;; check if a folder like the given one exists already
  (let ((absolute-path (format nil "~a" (car (directory folder-path )))))
    (if (eql 0 (length absolute-path))
        (error "TODO: some error on given directory :( (does it exist?)")
        (multiple-value-bind (path name) (lodds.core:split-directory absolute-path)
          (declare (ignore path))
          (loop
             :for shared-folder :in (get-shared-folders server)
             :do (multiple-value-bind (p n) (lodds.core:split-directory shared-folder)
                   (declare (ignore p))
                   (when (string= name n)
                     (error "TODO: the given directory can not be shared since a directory with that name already exists :(")))))))
  (when (find folder-path (get-shared-folders server))
    (error "TODO: the folder you tried to share has the same name"))
  (let ((new-watcher (make-instance 'lodds.watcher:watcher
                                    :change-hook (lambda (change)
                                                   (stmx:atomic
                                                    (push change (list-of-changes server))))
                                    :dir folder-path
                                    :recursive-p t)))
    (stmx:atomic
     (push new-watcher
           (slot-value server 'watchers)))))

(defmethod unshare-folder ((server lodds-server) &optional (folder-path nil))
  (if folder-path
      (let ((watcher (find (format nil "~a" (car (directory folder-path)))
                           (watchers server)
                           :key #'cl-fs-watcher:dir
                           :test #'string=)))
        (if watcher
            (progn
              (lodds.watcher:stop-watcher watcher)
              (stmx:atomic
               (setf (slot-value server 'watchers)
                     (remove watcher (watchers server)))))
            (error "TODO: could not find watcher to unshare with given folder-path")))
      (progn
        (mapcar
         (lambda (watcher)
           (lodds.watcher:stop-watcher watcher nil))
         (watchers server))
        (setf (slot-value server 'watchers) nil
              (list-of-changes server) nil))))

(defmethod get-file-changes ((server lodds-server) current-timestamp &optional (timestamp nil))
  (if timestamp
      (reverse
       (loop
          :for (ts . change) :in (list-of-changes server)
          ;; only collect infos with timestamps not equal to
          ;; CURRENT-TIMESTAMP and which are not older than TIMESTAMP
          :when (and (>= ts timestamp)
                     (not (eql ts current-timestamp)))
          :collect change :into result
          :else
          ;; stop and return only if this :else was triggerd by TS
          ;; beeing older then TIMESTAMP, and not if TS was eql to
          ;; CURRENT-TIMESTAMP
          :do (when (<= ts timestamp)
                (return result))
          :finally (return result)))
      (apply #'append
             (mapcar
              (lambda (watcher)
                (loop
                   :for info :in (lodds.watcher:get-all-tracked-file-infos watcher)
                   :collect (cons :add
                                  info)))
              (watchers server)))))

(defmethod shutdown ((server lodds-server))
  (stop-advertising server)
  (stop-listening server)
  (unshare-folder server))
