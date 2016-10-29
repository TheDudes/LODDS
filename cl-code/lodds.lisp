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
     :initform nil
     :initarg :name
     :accessor :name
     :type string
     :transactional nil
     :documentation "Advertised name. Will be displayed by other
                    Clients as ur name.")
    (listening-ip
     :initform nil
     :reader :listening-ip
     :type fixnum
     :documentation "Transactional variable, will be set by
                    SWITCH-INTERFACE inside a stmx:atomic block. Do
                    not set by hand.")
    (listening-port
     :initform 4567
     :initarg :listening-port
     :accessor :listening-port
     :transactional nil
     :documentation "Port the LODDS-SERVER listens on. Listening
                    (STOP/START-LISTENING) has to be restarted for
                    changes to take effect")
    (broadcast-ip
     :initform nil
     :reader :broadcast-ip
     :documentation "Transactional variable, IP LODDS-SERVER
                    Advertises information, will be set by
                    SWITCH-INTERFACE inside a stmx:atomic block. Do
                    not set by hand.")
    (broadcast-port
     :initform 9002
     :initarg :broadcast-port
     :accessor :broadcast-port
     :transactional nil
     :documentation "Port the LODDS-SERVER advertises to. Broadcasting
                    (STOP/START-BROADCASTING) has to be restarted for
                    changes to take effect")
    (client-timeout
     :initform 5
     :initarg :client-timeout
     :accessor :client-timeout
     :type integer
     :transactional nil
     :documentation "Timeout till client gets delete from local
                    list. Each advertise from other Clients is saved
                    with a timestamp, if timestamp is older than
                    CLIENT-TIMEOUT, the client will be deleted.")
    (interface
     :initform nil
     :reader :interface
     :type string
     :documentation "Transactional (STMX)!
                    Currently selected interface. To get a list of
                    available interface use GET-INTERFACES. Use
                    SWITCH-INTERFACE to change, or set, the
                    interface. SWITCH-INTEFACE will switch the value
                    inside a stmx:atomic.")
    (broadcast-listener
     :initform nil
     :accessor :broadcast-listener
     :transactional nil
     :documentation "BROADCAST-LISTENER server object. If this member
                    variable is nil the Server is not listening to
                    broadcast messages of other clients. Use
                    START-LISTEINING and STOP-LISTENING and do not set
                    this member by hand, since its spawning a thread
                    and manipulating the CLIENT member.")
    (broadcast-advertiser
     :initform nil
     :accessor :broadcast-advertiser
     :transactional nil
     :documentation "BROADCAST-ADVERTISER broadcasts information to
                    other clients. TODO implement")
    (clients
     :initform (make-hash-table :test #'equalp)
     :accessor :clients
     :transactional nil
     :type hashtable
     :documentation "hashtable containing all clients which their
                    broadcast information. This table is updated by
                    BROADCAST-LISTENER.
                    TODO: implement something to retrieve a copy.")
    (load
     :initform 0
     :accessor :load
     :type integer
     :documentation "Transactional (STMX)!
                    current load, this variable describes the sum of
                    all outstanding bytes which need to be
                    transfered. Do NOT set this variable, retrieving
                    it should be fine. TODO: who sets this?")
    (watchers
     :reader :watchers
     :initform nil
     :type list
     :transactional nil
     :documentation "List of Directory watchers")
    (list-of-changes
     :type list
     :initform '()
     :accessor :list-of-changes
     :documentation "list of changes. Each member is a list of
                     Timestamp, Type, checksum, size and name in that
                     order.")
    (advertise-timeout
     :initform 1
     :accessor :advertise-timeout
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


(defgeneric get-file-changes (server &optional timestamp)
  (:documentation
   "returns a list of all changes since the given timestamp. if
   timestamp is nil a full list of all files will be returnd"))
(defmethod switch-interface ((server lodds-server) (interface string))
  (let ((interface-info (get-interface-info interface)))
    (unless (null interface-info)
      (with-accessors ((ip ip-interfaces:ip-interface-address)
                       (bc ip-interfaces:ip-interface-broadcast-address))
          (get-interface-info interface)
        (let ((was-listening nil))
          (when (:broadcast-listener server)
            (stop-listening server)
            (setf was-listening t))
          (stmx:atomic
           (setf (slot-value server 'interface) interface
                 (slot-value server 'listening-ip) ip
                 (slot-value server 'broadcast-ip) bc))
          (when was-listening
            (start-listening server))))
      interface)))

(defun broadcast-listener (buffer server)
  "handles broadcast received messages. This function is a callback
  for the BROADCAST-LISTENER. It is set by START-LISTENING"
  (format t "broadcast: ~a ~a~%"
          (get-timestamp)
          (flexi-streams:octets-to-string buffer))
  (multiple-value-bind (error result) (read-advertise buffer)
    (unless (eql error 0)
      (format t "TODO: remove me: ERROR from read-advertise != 0~%"))
    (let ((current-time (get-timestamp))
          (clients (:clients server)))
      ;; remove all clients older then :client-timeout
      (maphash (lambda (key val)
                 (when (> (- current-time (car val))
                          (:client-timeout server))
                   (remhash key clients)))
               clients)
      ;; add client
      (setf (gethash (car (last result)) (:clients server))
            (cons current-time result)))))

(defmethod start-listening ((server lodds-server))
  ;; TODO: potentional Bug here when START-LISTENING gets called twice,
  ;; the whole if should be atomic, but is it needed? What about blocking this function
  ;; while its beeing processed by another thread? could this even happen?
  (if (:broadcast-listener server)
      ;; TODO: error message or something here
      (format t "broadcast listener already running.~%")
      (let* ((b-ip (:broadcast-ip server))
             (b-port (:broadcast-port server)))
        (if (or (null b-ip)
                (null b-port))
            ;; TODO: implement error handling here.
            (format t "error: broadcast ip or port not set!~%")
            (setf (:broadcast-listener server)
                  (usocket:socket-server
                   b-ip
                   b-port
                   (lambda (buffer)
                     (broadcast-listener buffer server)
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
  (if (null (:broadcast-listener server))
      (format t "broadcast listener not running.~%")
      ;; TODO this destroy has to go away
      (progn
        (bt:destroy-thread (:broadcast-listener server))
        (setf (:broadcast-listener server) nil))))

(defmethod remove-clients ((server lodds-server) (inactive-time fixnum))
  (let ((remove-me (list))
        (current-time (get-timestamp)))
    (maphash (lambda (key value)
               (when (> (- current-time (car value))
                        inactive-time)
                 (push key remove-me)))
             (:clients server))
    (mapcar (lambda (key)
              (remhash key (:clients server)))
            remove-me)))

(defmethod get-timestamp-last-change ((server lodds-server))
  (car (car (:list-of-changes server))))

(defun advertiser (server)
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (send-advertise
   (:broadcast-ip server)
   (:broadcast-port server)
   (list (:listening-ip server)
         (:listening-port server)
         (get-timestamp-last-change server)
         (:load server)
         (:name server))))

(defmethod start-advertising ((server lodds-server))
  ;;TODO: same as START-LISTENING, could be a bug here
  (if (:broadcast-advertiser server)
      ;; TODO: some kind of error handling here
      (format t "already advertising!~%")
      (setf (:broadcast-advertiser server)
            (bt:make-thread
             (lambda ()
               (loop
                  :while t
                  :with timeout
                  :do (progn
                        ;; repull timeout to get changes
                        (setf timeout (:advertise-timeout server))
                        (advertiser server)
                        (sleep timeout)))
               :name "broadcast-advertiser")))))

(defmethod stop-advertising ((server lodds-server))
  ;; TODO: Same here as START-ADVERTISING, what happens if it gets called twice?
  (if (:broadcast-advertiser server)
      ;; TODO: this destroy has to go away
      (bt:destroy-thread (:broadcast-advertiser server))
      ;; TODO: some error handling here
      (format t "advertising not running~%"))
  (setf (:broadcast-advertiser server) nil))

(defmethod get-user-list ((server lodds-server))
  (loop
     :for key :being :the :hash-key :of (:clients server)
     :collect key))

(defmethod get-user-info ((server lodds-server) (user string))
  (gethash user (:clients server)))

(defmethod get-file-changes ((server lodds-server) &optional (timestamp nil))
  (if timestamp
      (reverse
       (loop
          :for (ts . change) :in (:list-of-changes server)
          :when (>= ts timestamp)
          :collect change :into result
          :else
          :do (return result)
          :finally (return result)))
      (apply #'append
             (mapcar
              (lambda (watcher)
                (loop
                   :for info :in (lodds.watcher:get-all-tracked-file-infos watcher)
                   :collect (cons :add
                                  info)))
              (:watchers server)))))
