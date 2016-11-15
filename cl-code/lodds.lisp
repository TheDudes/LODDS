;;;; lodds.lisp

(in-package #:lodds)

(defparameter *server* nil)

(defmacro with-server (server &body body)
  `(let* ((*server* ,server)
          (bt:*default-special-bindings*
            (append (list (cons '*server* ,server))
                    bt:*default-special-bindings*)))
     ,@body))

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop :for interface :in (ip-interfaces:get-ip-interfaces-by-flags
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

(defun event-callback (event)
  (format t "log: ~a~%" event))

(defmethod initialize-instance ((server lodds-server) &rest initargs)
  (declare (ignorable initargs))
  (call-next-method)
  (with-server server ;; bind *server* for every subsystem of *server*
    (setf (subsystems server)
          (list
           ;; EVENT-QUEUE subsystem, this one is special to the
           ;; others, since its used by all the other subsystems. it
           ;; contains a lparallel.queue where messages can be added
           ;; (PUSH-EVENT). its used to transfer messages between
           ;; subsystems and the server object (or anything that has
           ;; attached a callback to the queue (ADD-CALLBACK). see
           ;; event.lisp for more info
           (make-instance 'lodds.event:event-queue
                          :name :event-queue
                          :callbacks (list (cons :lodds (lambda (event)
                                                          (event-callback event))))
                          :cleanup-fn #'lodds.event:cleanup
                          :init-fn #'lodds.event:run)
           ;; LISTENER subsystem listens on broadcast address of
           ;; the set INTERFACE and BROADCAST-PORT member of
           ;; server for advertisements from other clients
           (make-instance 'lodds.subsystem:subsystem
                          :name :listener
                          :init-fn #'lodds.listener:run)
           ;; ADVERTISER subystem, broadcasts information to
           ;; other clients on broadcast address of INTERFACE and
           ;; BROADCAST-PORT.
           (make-instance 'lodds.subsystem:subsystem
                          :name :advertiser
                          :init-fn #'lodds.advertiser:run)
           ;; HANDLER subsystem, listens for incomming
           ;; connections and handles those (starts threads etc).
           (make-instance 'lodds.subsystem:subsystem
                          :name :handler
                          :init-fn #'lodds.handler:run)
           ;; WATCHER subsystem, handles filesystem changes,
           ;; updates/handles local list of shared files
           (make-instance 'lodds.watcher:watcher
                          :name :watcher
                          :init-fn nil)
           ;; TASKER subsystem, handles and schedules several
           ;; tasks on a lparrallel:kernel. Waits on
           ;; event-queue for :task events and then executes
           ;; those.
           (make-instance 'lodds.task:tasker
                          :name :tasker
                          :init-fn nil)))))

(defun get-subsystem (name)
  "returns the requested subsystem, if not found nil will returned"
  (find name (subsystems *server*) :key #'lodds.subsystem:name))

(defun switch-interface (interface)
  "Switch interface the server acts on.  Interface is a string, to
  retrieve a list of available interfaces use GET-INTERFACES. Ports
  wont be set. SWITCH-INTEFFACE will check if a subsystem is running
  and restart it. For example, (SWITCH-INTERFACE \"enp0s25\") will
  stop the advertiser (if running), switch the interface, and start
  the advertiser again. Wont start any subsystems"
  (if (null (get-interface-info interface))
      ;; TODO: interface selection?
      (error "Given interface could not be found. Available interfaces: ~a"
             (get-interfaces))
      ;; collect all running subsystems
      (let ((was-running
              (loop :for key :in (list :handler
                                       :advertiser
                                       :listener)
                    :for subsystem = (get-subsystem key)
                    :when (lodds.subsystem:alive-p subsystem)
                    :collect subsystem)))

        ;; stop all subsystem which are running atm
        ;; TODO: fix waiting until all subsystems are closed
        (loop :for subsystem :in was-running
              :do (lodds.subsystem:stop subsystem))

        (setf (interface *server*) interface)

        ;; start all subsystem which where running before
        (loop :for subsystem :in was-running
              :do (lodds.subsystem:start subsystem))

        interface)))

(defun remove-clients (inactive-time)
  "removes all clients which a longer inactive then INACTIVE-TIME"
  (let ((remove-me (list))
        (current-time (get-timestamp)))
    (maphash (lambda (key value)
               (when (> (- current-time (car value))
                        inactive-time)
                 (push key remove-me)))
             (clients *server*))
    (mapcar (lambda (key)
              (remhash key (clients *server*)))
            remove-me)))

(defun get-timestamp-last-change ()
  "returns the timestamp of the last change, who would have thought?
  :D"
  (caar
   (lodds.watcher:list-of-changes
    (get-subsystem :watcher))))

(defun get-user-list ()
  "Returns List of users who advertised themselfs.

  CL-USER> (get-user-list *lodds-server*)
  => (\"peter\" \"steve\" \"josh\")

  Use GET-USER-INFO to get information about a user."
  (loop :for key :being :the :hash-key :of (clients *server*)
        :collect key))

(defun get-user-info (user)
  "Returns List with information about an given user.
  To get a list of available users see get-user-list.

  CL-USER> (get-user-info *lodds-server* \"someUser\")
  => (1475670931 #(172 19 246 14) 4567 0 0 \"someUser\")

  List contains timestamp-received, ip, port, timestamp-client,
  load, name in that order. The first timestamp describes a local
  timestamp when the message was received.

  Returns nil if user is not found."
  (gethash user (clients *server*)))

(defun get-file-changes (current-timestamp &optional (timestamp nil))
  "returns a list of all changes since the given timestamp. if
  timestamp is nil a full list of all files will be returnd"
  (if timestamp
      (reverse
       (loop :for (ts . change) :in (lodds.watcher:list-of-changes (get-subsystem :watcher))
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
                (loop :for info :in (lodds.watcher:get-all-tracked-file-infos watcher)
                      :collect (cons :add
                                     info)))
              (lodds.watcher:dir-watchers (get-subsystem :watcher))))))

(defun shutdown ()
  "shuts down the whole server, removes all handles and joins all
  spawned threads."
  (let ((event-queue nil))
    (loop :for subsystem :in (subsystems *server*)
          :if (eql (lodds.subsystem:name subsystem)
                   :event-queue)
            :do (setf event-queue subsystem)
          :else
            :do (lodds.subsystem:stop subsystem))
    ;; stop event-queue after the others, so i can see stopped messages
    (when event-queue
      (lodds.subsystem:stop event-queue))))

(defun generate-info-response (timestamp)
  (let* ((type (if (eql 0 timestamp)
                   :all
                   :upd))
         (current-timestamp (lodds.core:get-timestamp))
         (file-infos (get-file-changes current-timestamp
                                       (case type
                                         (:all nil)
                                         (:upd timestamp)))))
    (list type
          current-timestamp
          file-infos)))
