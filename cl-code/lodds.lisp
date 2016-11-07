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
  "returns the specified interface, nil if interface was not found"
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

(defmethod initialize-instance ((server lodds-server) &rest initargs)
  (declare (ignorable initargs))
  (call-next-method)
  (setf (subsystems server)
        ;; EVENT-QUEUE subsystem, this one is special to the others,
        ;; since its used by all the other subsystems. it contains a
        ;; STMX.util:TFIFO where messages can be added
        ;; (PUSH-EVENT). its used to transfer messages between
        ;; subsystems and the server object (or anything that has
        ;; attached a callback to the queue (ADD-CALLBACK). see
        ;; event.lisp for more info
        (let ((event-queue (make-instance 'lodds.event:event-queue
                                          :name :event-queue
                                          :init-fn #'lodds.event:run)))
          (lodds.subsystem:start event-queue)
          (list event-queue
                ;; LISTENER subsystem listens on broadcast address of
                ;; the set INTERFACE and BROADCAST-PORT member of
                ;; server for advertisements from other clients
                (make-instance 'lodds.subsystem:subsystem
                               :name :listener
                               :init-fn #'lodds.listener:run
                               :init-args (list server)
                               :event-queue event-queue)
                ;; ADVERTISER subystem, broadcasts information to
                ;; other clients on broadcast address of INTERFACE and
                ;; BROADCAST-PORT.
                (make-instance 'lodds.subsystem:subsystem
                               :name :advertiser
                               :init-fn #'lodds.advertiser:run
                               :init-args (list server)
                               :event-queue event-queue)
                ;; HANDLER subsystem, listens for incomming
                ;; connections and handles those (starts threads etc).
                (make-instance 'lodds.subsystem:subsystem
                               :name :handler
                               :init-fn #'lodds.handler:run
                               :init-args (list server)
                               :event-queue event-queue)
                (make-instance 'lodds.watcher:watcher
                               :name :watcher
                               :init-fn nil
                               :event-queue event-queue)))))
(defgeneric switch-interface (server interface)
  (:documentation
   "Switch interface the server acts on.  Interface is a string, to
    retrieve a list of available interfaces use GET-INTERFACES. Ports
    wont be set. SWITCH-INTEFFACE will check if a subsystem is running
    and restart it. For example, (SWITCH-INTERFACE \"enp0s25\") will
    stop the advertiser (if running), switch the interface, and start
    the advertiser again. Wont start any subsystems"))

(defgeneric remove-clients (server inactive-time)
  (:documentation
   "removes all clients which a longer inactive then INACTIVE-TIME"))

(defgeneric get-timestamp-last-change (server)
  (:documentation
   "returns the timestamp of the last change, who would have thought?
   :D"))

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

(defgeneric get-file-changes (server current-timestamp &optional timestamp)
  (:documentation
   "returns a list of all changes since the given timestamp. if
   timestamp is nil a full list of all files will be returnd"))

(defgeneric shutdown (server)
  (:documentation
   "shuts down the whole server, removes all handles and joins all
   spawned threads."))

(defun get-subsystem (server name)
  (find name (subsystems server) :key #'lodds.subsystem:name))

(defmethod switch-interface ((server lodds-server) (interface string))
  (if (null (get-interface-info interface))
      (format t "TODO: given interface could not be found. Available interfaces: ~a"
              (get-interfaces))

      ;; collect all running subsystems
      (let ((was-running
             (loop
                :for key :in (list :handler
                                   :advertiser
                                   :listener)
                :for subsystem = (get-subsystem server key)
                :when (lodds.subsystem:alive-p subsystem )
                :collect subsystem)))

        ;; stop all subsystem which are running atm
        ;; TODO: fix waiting until all subsystems are closed
        (loop
           :for subsystem :in was-running
           :do (lodds.subsystem:stop subsystem))

        (stmx:atomic
         (setf (stmx:$ (interface server)) interface))

        ;; start all subsystem which where running before
        (loop
           :for subsystem :in was-running
           :do (lodds.subsystem:start subsystem))

        interface)))

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
  (first
   (stmx.util:tfirst
    (lodds.watcher:list-of-changes
     (get-subsystem server :watcher)))))

(defmethod get-user-list ((server lodds-server))
  (loop
     :for key :being :the :hash-key :of (clients server)
     :collect key))

(defmethod get-user-info ((server lodds-server) (user string))
  (gethash user (clients server)))

(defmethod get-file-changes ((server lodds-server)
                             current-timestamp
                             &optional (timestamp nil))
  (if timestamp
      (reverse
       (loop
          :for val = (lodds.watcher:list-of-changes (get-subsystem server :watcher))
          :then (stmx.util:trest val)
          :for first = (stmx.util:tfirst val)
          :until (not first)
          :for ts = (car first)
          :for change = (cdr first)
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
              (lodds.watcher:dir-watchers (get-subsystem server :watcher))))))

(defmethod shutdown ((server lodds-server))
  (loop :for subsystem :in (subsystems server)
     :do (lodds.subsystem:stop subsystem)))

(defun generate-info-response (server timestamp)
  (let* ((type (if (eql 0 timestamp)
                   :all
                   :upd))
         (current-timestamp (lodds.core:get-timestamp))
         (file-infos (lodds:get-file-changes server
                                             current-timestamp
                                             (case type
                                               (:all nil)
                                               (:upd timestamp)))))
    (list type
          current-timestamp
          file-infos)))
