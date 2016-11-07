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

(defclass subsystem ()
  ((name :accessor name
         :initarg :name
         :initform (error "please specify a subsystem name (keyword like :my-awesome-subsystem)")
         :type keyword
         :documentation "Name to identify a subsystem.")
   (thread :accessor thread
           :initform nil
           :type bt:thread
           :documentation "The subsystem Thread. Once a subsystem is
           initialized it calls INIT-FN in a new thread.")
   (alive-p :accessor alive-p
            :initform nil
            :type boolean
            :documentation "Flag to check if subsystem is
            alive/running, do not set this by hand, this is just a
            indicator!. To start/stop a subsystem use SUBSYSTEM-START
            and SUBSYSTEM-STOP.")
   (init-fn :accessor init-fn
            :initarg :init-fn
            :initform (error "please specivy a init function which will be run by the subsystem")
            :type function
            :documentation "The 'main' function which will be called
            by a extra thread.")
   (init-args :accessor init-args
              :initarg :init-args
              :initform nil
              :type list
              :documentation "List of args which will be passed to
              init-fn")
   (event-queue :accessor event-queue
                :initarg :event-queue
                :initform nil
                :type lodds.event:event-queue
                :documentation "The event-queue where new events are
                pushed to.")))

(defmethod print-object ((object subsystem) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name thread alive-p) object
      (format stream "~a :alive-p ~a :thread ~a"
              name alive-p thread))))

(defclass lodds-server ()
  ((name :accessor name
         :initarg :name
         :initform nil
         :type string
         :documentation "Advertised name. Will be displayed by other
         Clients as client name.")
   (broadcast-port :accessor broadcast-port
                   :initarg :broadcast-port
                   :initform 9002
                   :documentation "Port the LODDS-SERVER advertises
                   to. Broadcasting (subsystem :advertiser) has to be
                   restarted for changes to take effect.")
   (subsystems :accessor subsystems
               :initform nil
               :type list
               :documentation "list of subsystems known to lodds, will
               be set after init, see INITIALIZE-INSTANCE for more
               details.")
   (handler-port :accessor handler-port
                 :initarg :handler-port
                 :initform 4567
                 :documentation "Port the LODDS-SERVER listens on. The
                 :handler subsystem has to be restarted for changes to
                 take effect.")
   (client-timeout :accessor client-timeout
                   :initarg :client-timeout
                   :initform 5
                   :type integer
                   :documentation "Timeout till client gets deleted
                   from local list. Each advertise from other Clients
                   is saved with a timestamp, if timestamp is older
                   than CLIENT-TIMEOUT, the client will be deleted.")
   (interface :accessor interface
              :initform (stmx:tvar nil)
              :type stmx:tvar
              :documentation "STMX:TVAR Currently selected
              interface. To get a list of available interface use
              GET-INTERFACES. Use SWITCH-INTERFACE to change, or set,
              the interface. SWITCH-INTERFACE will switch the value
              inside a STMX:ATOMIC.")
   (clients :accessor clients
            :initform (make-hash-table :test #'equalp)
            :type hashtable
            :documentation "Hashtable containing all clients which
            their broadcast information. This table is updated by
            LISTENER. TODO: implement something to retrieve a copy.")
   (current-load :accessor current-load
                 :initform (stmx:tvar 0)
                 :type stmx:tvar
                 :documentation "STMX:TVAR, describes the sum of all
                 outstanding bytes which need to be transfered. Do NOT
                 set this variable, retrieving it should be
                 fine tho. TODO: who sets this?")
   (watchers :accessor watchers
             :initform nil
             :type list
             :documentation "List of Directory watchers.")
   (list-of-changes :accessor list-of-changes
                    :initform (stmx.util:tlist nil)
                    :type stmx.util:tlist
                    :documentation "STMX.UTIL:TLIST, List of
                    changes. Each member is a list of Timestamp, Type,
                    checksum, size and name in that order.")
   (advertise-timeout :accessor advertise-timeout
                      :initform 1
                      :documentation "Timeout between
                      advertisements. Specified in seconds. Restarting
                      the :advertiser subsystem is not necessary,
                      since it rereads the value.")))

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
          (subsystem-start server event-queue)
          (list event-queue
                ;; LISTENER subsystem listens on broadcast address of
                ;; the set INTERFACE and BROADCAST-PORT member of
                ;; server for advertisements from other clients
                (make-instance 'subsystem
                               :name :listener
                               :init-fn #'lodds.listener:run
                               :init-args (list server)
                               :event-queue event-queue)
                ;; ADVERTISER subystem, broadcasts information to
                ;; other clients on broadcast address of INTERFACE and
                ;; BROADCAST-PORT.
                (make-instance 'subsystem
                               :name :advertiser
                               :init-fn #'lodds.advertiser:run
                               :init-args (list server)
                               :event-queue event-queue)
                ;; HANDLER subsystem, listens for incomming
                ;; connections and handles those (starts threads etc).
                (make-instance 'subsystem
                               :name :handler
                               :init-fn #'lodds.handler:run
                               :init-args (list server)
                               :event-queue event-queue)))))

(define-condition shutdown-condition (error)
  nil)

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

(defgeneric subsystem-start (server subsystem)
  (:documentation
   "Starts the given subsystem."))

(defgeneric subsystem-stop (server subsystem)
  (:documentation
   "Stops the given subsystem."))

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
                :for key :in (list :handler
                                   :advertiser
                                   :listener)
                :for subsystem = (get-subsystem server key)
                :when (alive-p subsystem )
                :collect subsystem )))

        ;; stop all subsystem which are running atm
        ;; TODO: fix waiting until all subsystems are closed
        (loop
           :for subsystem :in was-running
           :do (subsystem-stop server subsystem))

        (stmx:atomic
         (setf (stmx:$ (interface server)) interface))

        ;; start all subsystem which where running before
        (loop
           :for subsystem :in was-running
           :do (subsystem-start server subsystem))

        interface)))

(defun save-subsystem-start (subsystem)
  (labels ((save-init-fn ()
               (unwind-protect
                    (progn
                      (setf (alive-p subsystem) t)
                      (loop
                         :while (alive-p subsystem)
                         :do (handler-case
                                 (apply (init-fn subsystem) subsystem (init-args subsystem))
                               (lodds:shutdown-condition ()
                                 (setf (alive-p subsystem) nil))
                               (error (e)
                                 (format t "got uncaught error from subsystem ~a: ~a~%"
                                         (name subsystem)
                                         e)
                                 (setf (alive-p subsystem) nil)))))
                 (format t "~a stopped!~%" (name subsystem))
                 (setf (alive-p subsystem) nil))))
    (if (alive-p subsystem)
        (format t "Subsystem ~a is already Running!~%" (name subsystem))
        (setf
         (thread subsystem)
         (bt:make-thread #'save-init-fn
                         :name (format nil "LODDS-~a" (name subsystem)))))))

(defun get-subsystem (server name)
  (find name (subsystems server) :key #'name))

(defmethod subsystem-start ((server lodds-server) subsystem)
  (when (eql 'keyword (type-of subsystem))
    (setf subsystem (get-subsystem server subsystem)))
  (when subsystem
    (save-subsystem-start subsystem)))

(defmethod subsystem-stop ((server lodds-server) subsystem)
  (when (eql 'keyword (type-of subsystem))
    (setf subsystem (get-subsystem server subsystem)))
  (when (and subsystem
             (alive-p subsystem))
    (bt:interrupt-thread
     (thread subsystem)
     (lambda ()
       (signal (make-condition 'shutdown-condition))))))

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
  (car (stmx.util:tfirst (list-of-changes server))))

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
                                                    (stmx.util:tpush change (list-of-changes server))))
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
              (list-of-changes server) (stmx.util:tlist nil)))))

(defmethod get-file-changes ((server lodds-server) current-timestamp &optional (timestamp nil))
  (if timestamp
      (reverse
       (loop
          :for val = (list-of-changes server) :then (stmx.util:trest val)
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
              (watchers server)))))

(defmethod shutdown ((server lodds-server))
  (loop :for subsystem :in (subsystems server)
     :do (subsystem-stop server subsystem))
  (unshare-folder server))
