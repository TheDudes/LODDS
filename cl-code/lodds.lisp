;;;; lodds.lisp

(in-package #:lodds)

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop :for interface :in (ip-interfaces:get-ip-interfaces-by-flags
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

(defclass lodds-server ()
  ((config
    :initform (let ((ht (make-hash-table)))
                (setf (gethash :listening-ip ht) nil
                      (gethash :listening-port ht) nil
                      (gethash :broadcast-ip ht) nil
                      (gethash :broadcast-port ht) 9002)
                ht)
    :accessor :config
    :documentation "lodds server configuration. do not edit these by
                   hand. these will be updated by methods. See
                   SWITCH-INTERFACE, SWITCH-LISTENING-PORT and
                   SWITCH-BROADCAST-PORT")
   (lock
    :initform (bt:make-recursive-lock "lodds-server-lock")
    :accessor :lock
    :documentation "lock to access the member variables. this lock has
                   to be set if a instance of this class is accessed.")
   (interface
    :initform nil
    :accessor :interface
    :documentation "currently selected interface. To get a list of
                   available interface use GET-INTERFACES. Use
                   SWITCH-INTERFACE to change, or set, the
                   interface.")
   (broadcast-listener
    :initform nil
    :accessor :broadcast-listener
    :documentation "BROADCAST-LISTENER server object. If this member
                    variable is nil the Server is not listening to
                    broadcast messages of other clients. Use
                    START-LISTEINING and STOP-LISTENING and do not set
                    this member by hand, since its spawning a thread
                    and manipulating the CLIENT member.")
   (broadcast-advertiser
    :initform nil
    :accessor :broadcast-advertiser
    :documentation "BROADCAST-ADVERTISER broadcasts information to
                    other clients. TODO implement")
   (clients
    :initform nil
    :accessor :clients
    :documentation "hashtable containing all clients which their
                   broadcast information. This table is updated by
                   BROADCAST-LISTENER.
                   TODO: implement something to retrieve a copy.")))

(defgeneric switch-interface (server interface)
  (:documentation
   "Switch server interface and set addresses (ip and broadcast ip)
    accordingly. Interface is a string, to retrieve a list of
    available interfaces use GET-INTERFACES. Ports wont be set, use
    SWITCH-BROADCAST-PORT to set the broadcast port, and
    SWITCH-LISTENING-PORT to switch the listening port (direct
    communication). All SWITCH- Methods will check if a server is
    running and restart them. For example, (SWITCH-INTERFACE
    \"enp0s25\") will stop the broadcasting listener, switch the
    interface, and start the listener again. if the Listener was not
    running it will not start it."))

(defgeneric switch-broadcast-port (server port)
  (:documentation
   "Switches the Port where the Server listens for broadcast
   messages. To Change the Broadcast ip see SWITCH-INTERFACE. Will
   restart broadcast-listener if running."))

(defgeneric switch-listening-port (server port)
  (:documentation
   "Switches the Port where the Server listens for direct
   connections. This Port is Advertised. To Change the ip see
   SWITCH-INTERFACE. Will restart broadcast-listener if running."))

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

(defmethod switch-interface ((server lodds-server) (interface string))
  (let ((interface-info (get-interface-info interface)))
    (unless (null interface-info)
      (with-accessors ((ip ip-interfaces:ip-interface-address)
                       (bc ip-interfaces:ip-interface-broadcast-address))
          (get-interface-info interface)
        (bt:with-recursive-lock-held ((:lock server))
          (let ((was-listening nil))
            (when (:broadcast-listener server)
              (stop-listening server)
              (setf was-listening t))
            (setf (:interface server) interface
                  (gethash :listening-ip (:config server)) ip
                  (gethash :broadcast-ip (:config server)) bc)
            (when was-listening
              (start-listening server)))))
      interface)))

(defmethod switch-broadcast-port ((server lodds-server) (port fixnum))
  (bt:with-recursive-lock-held ((:lock server))
    (let ((was-listening nil))
      (when (:broadcast-listener server)
        (stop-listening server)
        (setf was-listening t))
      (setf (gethash :broadcast-port (:config server)) port)
      (when was-listening
        (start-listening server)))))

(defmethod switch-listening-port ((server lodds-server) (port fixnum))
  (bt:with-recursive-lock-held ((:lock server))
    (setf (gethash :listening-port (:config server)) port)))

(defun broadcast-listener (buffer server)
  "handles broadcast received messages. This function is a callback
  for the BROADCAST-LISTENER. It is set by START-LISTENING"
  (format t "broadcast: ~a ~a~%"
          (get-timestamp)
          (flexi-streams:octets-to-string buffer))
  (multiple-value-bind (error result) (read-advertise buffer)
    (unless (eql error 0)
      (format t "TODO: remove me: ERROR from read-advertise != 0~%"))
    (bt:with-recursive-lock-held ((:lock server))
      (setf (gethash (car (last result)) (:clients server))
            (cons (get-timestamp) result))
      ;; TODO: removing this nil is causing a error, i have no idea why :(
      nil)))

(defmethod start-listening ((server lodds-server))
  (bt:with-recursive-lock-held ((:lock server))
    (if (not (null (:broadcast-listener server)))
        ;; TODO: error message or something here
        (format t "broadcast listener already running.~%")
        (setf (:clients server) (make-hash-table :test #'equalp)
              (:broadcast-listener server)
              (usocket:socket-server
               (gethash :broadcast-address (:config server))
               (gethash :broadcast-port (:config server))
               (lambda (buffer)
                 (broadcast-listener buffer server))
               nil
               :in-new-thread t
               :protocol :datagram)))))

(defmethod stop-listening ((server lodds-server))
  (bt:with-recursive-lock-held ((:lock server))
    (if (null (:broadcast-listener server))
        (format t "broadcast listener not running.~%")
        (progn
          (bt:destroy-thread (:broadcast-listener server))
          (setf (:broadcast-listener server) nil)))))

(defmethod remove-clients ((server lodds-server) (inactive-time fixnum))
  (bt:with-recursive-lock-held ((:lock server))
    (let ((remove-me (list))
          (current-time (get-timestamp)))
      (maphash (lambda (key value)
                 (when (> (- current-time (car value))
                          inactive-time)
                   (push key remove-me)))
               (:clients server))
      (mapcar (lambda (key)
                (remhash key (:clients server)))
              remove-me))))
