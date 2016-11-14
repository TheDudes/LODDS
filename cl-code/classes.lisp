;; lodds.subsystem classes

(in-package #:lodds.subsystem)

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
   (cleanup-fn :accessor cleanup-fn
               :initarg :cleanup-fn
               :initform nil
               :type function
               :documentation "Cleanup function, gets called when subsytem is shut down.")
   (event-queue :accessor event-queue
                :initarg :event-queue
                :initform nil
                :type lodds.event:event-queue
                :documentation "The event-queue where new events are
                pushed to.")))

;; lodds.task classes

(in-package #:lodds.task)

(defclass tasker (lodds.subsystem:subsystem)
  ((kernel :accessor kernel
           :initform nil
           :type lparallel:kernel
           :documentation "Tasker lparallel:kernel")
   (channel :accessor channel
            :initform nil
            :type lparallel:channel
            :documentation "tasker lparallel:channel")))

(defclass task ()
  ((name :accessor name
         :initarg :name
         :initform (error "please specify a task name!")
         :type string
         :documentation "Task Name.")))

(defclass task-client (task)
    ((client-name :reader client-name
                  :initarg :client-name
                  :initform (error "Specify client-name")
                  :type string
                  :documentation "name of client, for example: d4yus@192.168.2.1:1234")
     (client-ip :reader client-ip
                :initarg :client-ip
                :initform (error "Specify client-ip")
                :type vector
                :documentation "Ip of client, for example: #(192 168 2 1)")
     (client-port :reader client-port
                  :initarg :client-port
                  :initform (error "Specify client-port")
                  :type integer
                  :documentation "Port of client, for exampe: 1234")))

(defclass task-client-info (task-client)
  ((client-message-timestamp :reader client-message-timestamp
                             :initform nil
                             :initarg :client-message-timestamp
                             :type integer
                             :documentation "Timestamp the Broadcast message
                             was received")
   (client-last-change :reader client-last-change
                       :initarg :client-last-change
                       :initform (error "Specify client-last-change")
                       :type integer
                       :documentation "Timestamp of last change,
                       received vom client")
   (client-load :reader client-load
                :initarg :client-load
                :initform (error "Specify client load")
                :type integer
                :documentation "Advertised load of given Client")))

;; lodds.event classes

(in-package #:lodds.event)

(defclass event-queue (lodds.subsystem:subsystem)
  ((queue :accessor queue
          :initform (lparallel.queue:make-queue)
          :type lparallel.queue:queue
          :documentation "the actual queue containing events")
   (callbacks :accessor callbacks
              :initform nil
              :type list
              :documentation "callback functions which will be called
              if a event occures. Functions inside CALLBACKS will
              always be called, no matter what type of event occured.")
   (typed-callbacks :accessor typed-callbacks
                    :initform (make-hash-table)
                    :type hash-table
                    :documentation "hash-table containing callbacks
                    accessable by their EVENT-TYPE. Callbacks saved
                    under a specific EVENT-TYPE will only be called if
                    a event occures with the given EVENT-TYPE.")))

;; lodds.watcher classes

(in-package #:lodds.watcher)

(defclass dir-watcher (cl-fs-watcher:watcher)
  ((root-dir-name :type string
                  :reader root-dir-name
                  :documentation "contains the root directory name of
                  the watched dir (DIR), without the path and starting
                  with a slash. set after
                  initialization. ROOT-DIR-PATH concatenated with
                  ROOT-DIR-NAME gives DIR. This variable is used to
                  calculate the path for LIST-OF-CHANGE.")
   (root-dir-path :type string
                  :reader root-dir-path
                  :documentation "contains the directory name where
                  the root-directory is located. set after
                  initialization. ROOT-DIR-PATH concatenated with
                  ROOT-DIR-NAME gives DIR. This variable is used to
                  calculate the path for LIST-OF-CHANGE.")
   (file-table-name :type hashtable
                    :initform (make-hash-table :test 'equal)
                    :reader file-table-name
                    :documentation "hashtable of tracked files, with
                    their path as key.  Value is a list of file
                    checksum and its size.")
   (file-table-hash :type hashtable
                    :initform (make-hash-table :test 'equal)
                    :reader file-table-hash
                    :documentation "hashmap of tracked files, with
                    their checksum as key.  Value is a list of files
                    with the given checksum.")
   (change-hook :type function
                :initform (error "Specify a :change-hook!")
                :initarg :change-hook
                :reader change-hook
                :documentation "this hook gets called when a change
                occured and a new change entry was generated, its used
                to put changes from multiple watchers into one list of
                changes. Its called with one argument, a list of
                Timestamp, Type, checksum, size and name in that
                order.")))

(defclass watcher (lodds.subsystem:subsystem)
  ((dir-watchers :accessor dir-watchers
                 :initform nil
                 :type list
                 :documentation "List of Directory watchers.")
   (list-of-changes-lock :accessor list-of-changes-lock
                         :initform (bt:make-lock "list-of-changes-lock")
                         :documentation "Lock to access list-of-changes")
   (list-of-changes :accessor list-of-changes
                    :initform '()
                    :type list
                    :documentation "List of changes. Each member is a
                    list of Timestamp, Type, checksum, size and name
                    in that order. Lock with LIST-OF-CHANGES-LOCK
                    before modifying the list.")))

;; lodds classes

(in-package #:lodds)

(defclass client-info ()
  ((c-name :accessor c-name
           :initarg :c-name
           :initform (error "specify client name")
           :type string
           :documentation "client name, like d4ryus@192.168.2.102")
   (c-last-message :accessor c-last-message
                   :initarg :c-last-message
                   :initform (error "specivy clients last-message timestamp")
                   :type integer
                   :documentation "clients last message timestamp")
   (c-ip :accessor c-ip
         :initarg :c-ip
         :initform (error "specify client ip")
         :type vector
         :documentation "client ip")
   (c-port :accessor c-port
           :initarg :c-port
           :initform (error "specify client port")
           :type integer
           :documentation "client port")
   (c-last-change :accessor c-last-change
                  :initarg :c-last-change
                  :initform (error "specify client last-change")
                  :type integer
                  :documentation "clients last change timestamp")
   (c-load :accessor c-load
           :initarg :c-load
           :initform (error "specify clients load")
           :type integer
           :documentation "clients load")
   (c-file-table-name :accessor c-file-table-name
                      :type hashtable
                      :initform (make-hash-table :test 'equal)
                      :documentation "hashtable of clients shared
                                    files, with their path as key.
                                    Value is a list of file checksum
                                    and its size.")
   (c-file-table-hash :accessor c-file-table-hash
                      :initform (make-hash-table :test 'equal)
                      :type hashtable
                      :documentation "hashmap of clients shared files, with their checksum as key.
                                    Value is a list of files with the given checksum.")
   (c-lock :accessor c-lock
           :initform (bt:make-lock "c-lock")
           :documentation "Look to access member variables.")))

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
              :initform nil
              :type string
              :documentation "Currently selected interface. To get a
              list of available interfaces use GET-INTERFACES. Use
              SWITCH-INTERFACE to change, or set, the interface.")
   (clients :accessor clients
            :initform (make-hash-table :test #'equalp)
            :type hashtable
            :documentation "Hashtable containing all clients which
            their broadcast information. This table is updated by
            LISTENER. TODO: implement something to retrieve a copy.")
   (current-load-lock :accessor current-load-lock
                      :initform (bt:make-lock "current-load-lock")
                      :documentation "Lock to modify CURRENT-LOAD")
   (current-load :accessor current-load
                 :initform 0
                 :type rational
                 :documentation "STMX:TVAR, describes the sum of all
                 outstanding bytes which need to be transfered. Do NOT
                 set this variable, retrieving it should be
                 fine tho. TODO: who sets this?")
   (advertise-timeout :accessor advertise-timeout
                      :initform 1
                      :documentation "Timeout between
                      advertisements. Specified in seconds. Restarting
                      the :advertiser subsystem is not necessary,
                      since it rereads the value.")))
