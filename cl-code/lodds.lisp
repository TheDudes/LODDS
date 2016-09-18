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
    :documentation "configuration and values")
   (lock
    :initform (bt:make-recursive-lock "lodds-server-lock")
    :accessor :lock
    :documentation "lock to access the member variables")
   (interface
    :initform nil
    :accessor :interface
    :documentation "currently selected interface, get a list with
                    'get-interfaces'. switch interface with 'switch-interface'")
   (broadcast-listener
    :initform nil
    :accessor :broadcast-listener
    :documentation "broadcast-listener server object which handles
                    advertisements of other clients.")
   (broadcast-advertiser
    :initform nil
    :accessor :broadcast-advertiser
    :documentation "broadcast-advertiser broadcasts information to
                    other clients")
   (clients
    :initform nil
    :accessor :clients
    :documentation "hashtable containing all clients which broadcasted themselves")))

(defgeneric switch-interface (server interface)
  (:documentation "Switch interface and set addresses accordingly"))

(defgeneric start-listening (server)
  (:documentation
   "will start listening for broadcast messages of other clients,
   it will spawn a seperate thread which updates a hashtable inside
   server (accessable through :clients)."))

(defgeneric stop-listening (server)
  (:documentation
   "stops the given server from listening on broadcast address."))

(defgeneric remove-clients (server inactive-time)
  (:documentation
   "removes all clients which are longer inactive then 'inactive-time'"))

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

(defun broadcast-listener (buffer server)
  "handles broadcast received messages"
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
