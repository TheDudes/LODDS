;; lodds.subsystem classes

(in-package #:lodds.subsystem)

(defclass subsystem ()
  ((name :accessor name
         :initarg :name
         :initform (error "please specify a subsystem name ~
                          (keyword like :my-awesome-subsystem)")
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
            :initform (error "please specify a init function which ~
                             will be run by the subsystem")
            :type function
            :documentation "The 'main' function which will be called
            by a extra thread.")))

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
            :documentation "tasker lparallel:channel")
   (lock :initform (bt:make-recursive-lock "tasker lock")
         :documentation "look to access tasks and tasks-on-hold")
   (tasks :initform (make-hash-table :test #'equalp)
          :type hash-table
          :documentation "Hashtable containing all existing tasks.")
   (tasks-on-hold :initform (make-hash-table :test #'equalp)
                  :type hash-table
                  :documentation "Hashtable containing Tasks which are
                  on hold. See PUT-TASK-ON-HOLD, SUBMIT-TASK-FROM-HOLD
                  and REMOVE-TASK-FROM-HOLD.")))

(defclass task ()
  ((id :initform nil
       :type string)
   (type :initform :task)
   (info :initform "task"
         :type string
         :documentation "information about the task can be placed in
         this slot")
   (load :initarg :load
         :initform 0
         :documentation "The load a task produces, will be decremented
         while the task is running")
   (max-load :initarg :max-load
             :initform 0
             :documentation "The maximum load a task will produce, set
             in initialization. This can be used in conjunction with
             load the determine the progress of the task")
   (name :initarg :name
         :initform (error "please specify a task name!")
         :type string
         :documentation "Task Name.")
   (resubmit-p :initform nil
               :documentation "Flag which is t if the task should get
               resubmittet")
   (canceled-p :initform nil
               :documentation "Flag which is t if task was canceled")
   (finished-p :initform nil
               :documentation "Flag which is t if task has finished")
   (aktive-p :initform nil
             :type boolean
             :documentation "Flag which is t if task is currently
             aktive")
   (on-finish-hook :initarg :on-finish-hook
                   :initform nil
                   :type function
                   :documentation "Function which gets called when the
                   task finishes. Will be called before finish-task
                   method gets called")
   (on-error-hook :initarg :on-error-hook
                  :initform nil
                  :type function
                  :documentation "Function which gets called when the
                  task errors.")
   (on-cancel-hook :initarg :on-cancel-hook
                   :initform nil
                   :type function
                   :documentation "Function which gets called when the
                   task was canceled.")))

(defclass task-user (task)
    ((user :initarg :user
           :initform (error "Specify user")
           :documentation "name of user, for example:
           d4yus@192.168.2.1:1234")
     (ip :initarg :ip
         :initform (error "Specify ip")
         :type vector
         :documentation "Ip of client, for example: #(192 168 2 1)")
     (port :initarg :port
           :initform (error "Specify port")
           :type integer
           :documentation "Port of client, for exampe: 1234")))

(defclass task-info (task-user)
  ((timestamp :initarg :timestamp
              :initform nil
              :type integer
              :documentation "Timestamp the Broadcast message was
              received")
   (last-change :initarg :last-change
                :initform (error "Specify client-last-change")
                :type integer
                :documentation "Timestamp of last change,received vom
                client")
   (user-load :initarg :user-load
              :initform (error "Specify client load")
              :type integer
              :documentation "Advertised load of given Client")))

(defclass task-request (task)
  ((socket :initform (error "Specify a socket pls.")
           :initarg :socket
           :type usocket:socket
           :documentation "Socket with the requesting client on the
           other end :D")))

(defclass task-request-file (task-request)
  ((type :initform :request-file)
   (checksum :initarg :checksum
             :initform nil
             :type string
             :documentation "Requested File checksum.")
   (start :initarg :start
          :initform nil
          :type rational
          :documentation "Requested File start position.")
   (end :initarg :end
        :initform nil
        :type rational
        :documentation "Requested File end position.")
   (filename :type string
             :initform nil
             :documentation "Local Filename of Requested File")
   (file-stream :type stream
                :initform nil
                :documentation "Stream pointing to local file
                request-filename.")
   (written :initform 0
            :type rational
            :documentation "Bytes Written onto socket")))

(defclass task-request-info (task-request)
  ((timestamp :initform 0
              :initarg :timestamp
              :type rational
              :documentation "Requested info timestamp.")))

(defclass task-request-send-permission (task-request)
  ((type :initform :request-send-permisison)
   (size :initarg :size
         :initform nil
         :type rational
         :documentation "Size of the File requested to send.")
   (timeout :initarg :timeout
            :initform nil
            :type rational
            :documentation "Time the Requesting Client will wait for a
            answer.")
   (filename :initarg :filename
             :initform nil
             :type string
             :documentation "Name of the File requested to send")
   (file-stream :type file-stream
                :initform nil
                :documentation "File stream pointing to filename")
   (read-bytes :type bignum
               :initform 0
               :documentation "Amount of bytes already read from the
               socket-stream")))

(defclass task-get-file (task)
  ((type :initform :get-file)
   (local-file-path :initform (error "Specify a local-file-path pls.")
                    :initarg :local-file-path
                    :type string
                    :documentation "String describing the local file
                    path. The Path describes where the file, which is
                    getting downloaded from another client, is getting
                    saved on the local filesystem.")
   (checksum :initform (error "Specify a checksum pls.")
             :initarg :checksum
             :type string
             :documentation "Checksum to identify the File. Used to
             request the File from the user and find all users who
             hold the file.")
   (size :initform 0
         :type bignum
         :documentation "Size of specified File, will be set on
         initialize instance")
   (socket :initform nil
           :type usocket:socket
           :documentation "If connection is established socket is
           set. It will then be read for information.")
   (local-file-stream :type file-stream
                      :initform nil
                      :documentation "If connection is established and
                      transfer has started file-stream will point to
                      local-file-path. Everything read from socket
                      will be saved to file-stream.")
   (read-bytes :initform 0
               :type bignum
               :documentation "Bignum describing how many bytes have
               been read from the socket and saved to the file.")))

(defclass task-get-file-from-user (task-get-file)
  ((type :initform :get-file-from-user)
   (user :initform (error "Specify a user pls.")
         :initarg :user
         :type string
         :documentation "The User where the file is getting downloaded
         from. User has to be specified with his ip and port in the
         following format: username@ip:port, for example:
         d4ryus@192.168.2.101:1234")
   (ip :initform nil
       :type string
       :documentation "Ip of User, will be set on initialize
       instance. Can be parsed from get-user.")
   (port :initform nil
         :type fixnum
         :documentation "Port of User, will be set on initialize
         instance. Can be parsed from get-user.")))

(defclass task-get-file-from-users (task-get-file)
  ((type :initform :get-file-from-users)
   (current-part :initform 0
                 :type bignum
                 :documentation "The current part which is
                 downloaded.")
   (read-bytes-part :initform 0
                    :type bignum
                    :documentation "Bytes read of current part.")
   (part-size :initform 0
              :type bignum
              :documentation "Size limit after which lodds checks
              again for a the user with the lowest load.")))

(defclass task-get-folder (task)
  ((type :initform :get-folder)
   (user :initarg :user
         :initform (error "please specify the User who contains ~ the
         wanted Folder")
         :type string
         :documentation "User who got the wanted Folder")
   (local-path :initarg :local-path
               :initform (error "please specify a local folder")
               :type string
               :documentation "Local Folder where Files of Remote
               Folder will be downloaded too")
   (remote-root :initarg :remote-root
                :initform (error "please specify the remote folder
                root path")
                :type string
                :documentation "Remote Folder Root Path which should
                be downloaded")
   (remote-path :initarg :remote-path
                :initform (error "please specify the remote folder
                path")
                :type string
                :documentation "Remote Folder which should be
                downloaded")
   (items :initform nil
          :type list
          :documentation "List of files and (path checksum size) the
          Remote Folder contains. Will be filled by GET-FOLDER-INFO
          when initialized")
   (items-done :initform nil
               :type list
               :documentation "List of files which are already
               downloaded")))

(defclass task-send-file (task-user)
  ((type :initform :send-file)
   (filepath :initarg :filepath
             :initform (error "Specify file")
             :documentation "Full path to the local file which will be
             sent")
   (timeout :initarg :timeout
            :initform (error "Specify timeout")
            :type integer
            :documentation "Timeout in seconds how long we wait for a
            responsse from the receiving client. If there is no
            positive Response within the given timeout, the send-file
            task will be abortet")
   (socket :initform nil
           :documentation "Socket of the recieving Client (Socket of
           given ip and port), will be opened on first call to
           run-task")
   (file-stream :initform nil
                :type file-stream
                :documentation "File stream pointing to filepath, will
                be set on first call to run-task")
   (size :initform 0
         :type bignum
         :documentation "Size of file described by file-stream and
         filepath, will be set on first call to run-task")
   (written :initform 0
            :type bignum
            :documentation "Amount of bytes already written to the
            Socket")))

;; lodds.event classes

(in-package #:lodds.event)

(defclass event-queue (lodds.subsystem:subsystem)
  ((queue :accessor queue
          :initform (lparallel.queue:make-queue)
          :type lparallel.queue:queue
          :documentation "the actual queue containing events")
   (callbacks :accessor callbacks
              :initform nil
              :initarg :callbacks
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
  ((started-tracking :accessor started-tracking
                     :initform 0
                     :type integer
                     :documentation "When the first Directory is
                     shared start-tracking will be set to the current
                     timestamp. This is used to check if a timestamp
                     describes a time before the watcher tracked
                     files. Will be set to 0 if watcher is stopped.")
   (dir-watchers :accessor dir-watchers
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
                    before modifying the list.")
   (last-change :accessor last-change
                :initform 0
                :type rational
                :documentation "Timestamp of file change on watched
                files.")))

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
         :initform (machine-instance)
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
   (advertise-timeout :accessor advertise-timeout
                      :initform 1
                      :documentation "Timeout between
                      advertisements. Specified in seconds. Restarting
                      the :advertiser subsystem is not necessary,
                      since it rereads the value.")))
