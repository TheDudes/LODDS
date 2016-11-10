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
   (event-queue :accessor event-queue
                :initarg :event-queue
                :initform nil
                :type lodds.event:event-queue
                :documentation "The event-queue where new events are
                pushed to.")))

;; lodds.event classes

(in-package #:lodds.event)

(defclass event-queue (lodds.subsystem:subsystem)
  ((queue :accessor queue
          :initform (make-instance 'stmx.util:tfifo )
          :type stmx.util:tfifo
          :documentation "the actual queue containing events")
   (callbacks :accessor callbacks
              :initform (make-hash-table :test 'equalp)
              :type hash-table
              :documentation "hash-table containing callbacks which
              will be called by the event-queue if a event occurs")))

;; lodds.watcher classes

(in-package #:lodds.watcher)

(stmx:transactional
 (defclass dir-watcher (cl-fs-watcher:watcher)
   ((root-dir-name :type string
                   :reader root-dir-name
                   :transactional nil
                   :documentation "contains the root directory name of
                                  the watched dir (DIR), without the
                                  path and starting with a slash. set
                                  after initialization. ROOT-DIR-PATH
                                  concatenated with ROOT-DIR-NAME
                                  gives DIR. This variable is used to
                                  calculate the path for
                                  LIST-OF-CHANGE.")
    (root-dir-path :type string
                   :reader root-dir-path
                   :transactional nil
                   :documentation "contains the directory name where
                                  the root-directory is located. set
                                  after initialization. ROOT-DIR-PATH
                                  concatenated with ROOT-DIR-NAME
                                  gives DIR. This variable is used to
                                  calculate the path for
                                  LIST-OF-CHANGE.")
    (file-table-name :type hashtable
                     :initform (make-hash-table :test 'equal)
                     :reader file-table-name
                     :documentation "hashtable of tracked files, with their path as key.
                                    Value is a list of file checksum and its size.
                                    Note: this member is STMX:TRANSACTIONAL")
    (file-table-hash :type hashtable
                     :initform (make-hash-table :test 'equal)
                     :reader file-table-hash
                     :documentation "hashmap of tracked files, with their checksum as key.
                                    Value is a list of files with the given checksum.
                                    Note: this member is STMX:TRANSACTIONAL")
    (change-hook :type function
                 :initform (error "Specify a :change-hook!")
                 :initarg :change-hook
                 :reader change-hook
                 :transactional nil
                 :documentation "this hook gets called when a change
                                 occured and a new change entry was
                                 generated, its used to put changes
                                 from multiple watchers into one list
                                 of changes. Its called with one
                                 argument, a list of Timestamp, Type,
                                 checksum, size and name in that
                                 order."))))

(defclass watcher (lodds.subsystem:subsystem)
  ((dir-watchers :accessor dir-watchers
                 :initform nil
                 :type list
                 :documentation "List of Directory watchers.")
   (list-of-changes :accessor list-of-changes
                    :initform (stmx.util:tlist nil)
                    :type stmx.util:tlist
                    :documentation "STMX.UTIL:TLIST, List of
                    changes. Each member is a list of Timestamp, Type,
                    checksum, size and name in that order.")))

;; lodds classes

(in-package #:lodds)

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
   (advertise-timeout :accessor advertise-timeout
                      :initform 1
                      :documentation "Timeout between
                      advertisements. Specified in seconds. Restarting
                      the :advertiser subsystem is not necessary,
                      since it rereads the value.")))
