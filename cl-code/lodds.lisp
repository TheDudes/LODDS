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
    (broadcast-port
     :accessor broadcast-port
     :initarg :broadcast-port
     :initform 9002
     :transactional nil
     :documentation "Port the LODDS-SERVER advertises to. Broadcasting
                    (subsystem :advertiser) has to be restarted for
                    changes to take effect")
    (listener
     :accessor listener
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "LISTENER subsystem thread. If this member
                    variable is nil the Server is not listening to
                    broadcast messages of other clients. Use (start
                    ur-server-obj :listening) to start listening to
                    broadcast messages. Do not set this member by
                    hand, since its spawning a thread
                    and manipulating the CLIENT member.")
    (advertiser
     :accessor advertiser
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "ADVERTISER broadcasts information to other
                    clients. use (start ur-server-obj :advertiser) to
                    start advertising information.")
    (handler-port
     :accessor handler-port
     :initarg :handler-port
     :initform 4567
     :transactional nil
     :documentation "Port the LODDS-SERVER listens on. The Handler
                    subsystem has to be restarted for changes to take
                    effect.")
    (handler
     :accessor handler
     :initform nil
     :type bt:thread
     :transactional nil
     :documentation "HANDLER subsystem thread, listens for incomming
                    connections and handles thems (starts threads
                    etc).")
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
     :accessor interface
     :initform nil
     :type string
     :documentation "Transactional (STMX)!
                    Currently selected interface. To get a list of
                    available interface use GET-INTERFACES. Use
                    SWITCH-INTERFACE to change, or set, the
                    interface. SWITCH-INTERFACE will switch the value
                    inside a stmx:atomic.")
    (clients
     :accessor clients
     :initform (make-hash-table :test #'equalp)
     :type hashtable
     :transactional nil
     :documentation "hashtable containing all clients which their
                    broadcast information. This table is updated by
                    LISTENER.
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
     :accessor watchers
     :initform nil
     :type list
     :transactional nil
     :documentation "List of Directory watchers.")
    (list-of-changes
     :accessor list-of-changes
     :initform '()
     :type list
     :documentation "List of changes. Each member is a list of
                     Timestamp, Type, checksum, size and name in that
                     order.")
    (advertise-timeout
     :accessor advertise-timeout
     :initform 1
     :transactional nil
     :documentation "Timeout between advertisements. Specified in
                    seconds. Restarting the ADVERTISE is not
                    necessary."))))

(define-condition shutdown-condition (error)
  nil)

(defvar *subsystems*
  ;;          identifier  slot        package
  (list (list :advertiser 'advertiser 'lodds.advertiser)
        (list :listener   'listener   'lodds.listener)
        (list :handler    'handler    'lodds.handler)))

(defgeneric switch-interface (server interface)
  (:documentation
   "Switch interface the server acts on.  Interface is a string, to
    retrieve a list of available interfaces use GET-INTERFACES. Ports
    wont be set. SWITCH-INTEFFACE will check if a subsystem is running
    and restart it. For example, (SWITCH-INTERFACE \"enp0s25\") will
    stop the advertiser (if running), switch the interface, and start
    the advertiser again. Wont start any subsystems"))

(defgeneric switch-advertise-timeout (server timeout)
  (:documentation
   "switch ADVERTISE-TIMEOUT, TIMEOUT is given in seconds."))

(defgeneric start (server &optional subsystem)
  (:documentation
   "Starts the given subsystem. subsystem can be one of:
   :listener
   :advertiser
   :handler
   not specifing a subsystem will start all (or those not started yet)."))

(defgeneric stop (server &optional subsystem)
  (:documentation
   "Stops the given subsystem. subsystem can be one of:
   :listener
   :advertiser
   :handler
   if left out, stops all subsystems"))

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
  (if (null (get-interface-info interface))
      (format t "TODO: given interface could not be found. Available interfaces: ~a"
              (get-interfaces))

      ;; collect all running subsystems
      (let ((was-running
             (loop
                :for (subsystem slot package) :in *subsystems*
                :when (slot-value server slot)
                :collect subsystem)))

        ;; stop all subsystem which are running atm
        (loop
           :for subsystem :in was-running
           :do (stop server subsystem))

        (stmx:atomic
         (setf (interface server) interface))

        ;; start all subsystem which where running before
        (loop
           :for subsystem :in was-running
           :do (start server subsystem))

        interface)))

(defmethod start ((server lodds-server) &optional (subsystem nil))
  (labels ((run-subsystem (subsys slot package)
             (let ((handle (slot-value server slot)))
               (if handle
                   (format t "~a is already Running.~%" subsys)
                   (bt:make-thread
                    (lambda ()
                      (funcall (symbol-function (find-symbol "RUN" package))
                               server))
                    :name (format nil "~a" subsys))))))
    (if subsystem
        (destructuring-bind (subsystem slot package)
            (assoc subsystem *subsystems*)
          (setf (slot-value server slot)
                (run-subsystem subsystem slot package)))
        (loop
           :for (subsystem slot package) :in *subsystems*
           :do (setf (slot-value server slot)
                     (run-subsystem subsystem slot package))))))

(defmethod stop ((server lodds-server) &optional (subsystem nil))
  (labels ((stop-subsystem (subsys slot)
             (let ((handle (slot-value server slot)))
               (if handle
                   (bt:interrupt-thread
                    handle
                    (lambda ()
                      (signal (make-condition 'shutdown-condition))))
                   (format t "~a not running.~%" subsys)))))
    (if subsystem
        (destructuring-bind (subsystem slot package)
            (assoc subsystem *subsystems*)
          (declare (ignore package))
          (stop-subsystem subsystem slot))
        (loop
           :for (subsystem slot package) :in *subsystems*
           :do (stop-subsystem subsystem slot)))))

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
           (watchers server)))))

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
               (setf (watchers server)
                     (remove watcher (watchers server)))))
            (error "TODO: could not find watcher to unshare with given folder-path")))
      (progn
        (mapcar
         (lambda (watcher)
           (lodds.watcher:stop-watcher watcher nil))
         (watchers server))
        (setf (watchers server) nil
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
  (stop server)
  (unshare-folder server))
