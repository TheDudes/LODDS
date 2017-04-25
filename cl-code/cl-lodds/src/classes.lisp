#|

This file contains all classes (iam not sure if it contains all, but at
least 95% of them :D). This way classes are 'forward declared' by
loading the classes file first.

|#

(in-package #:lodds.task)

(defclass tasks ()
  ((id-counter :initform 0)
   (lock :initform (bt:make-recursive-lock "tasker lock")
         :documentation "look to access tasks")
   (tasks :initform (make-hash-table :test #'equal)
          :type hash-table
          :documentation "hashtable containing all existing tasks.")))

(defclass task ()
  ((tasks :initform (error "Specify tasks")
          :initarg :tasks)
   (id :reader task-id)
   (thread :initform nil)
   (in-thread :initform t :initarg :in-thread)
   (initialized-p :initform nil)
   (canceled :initform nil)
   (bytes-transfered :initform 0 :type fixnum)
   (total-load :initform 0 :initarg :total-load :type fixnum)
   (socket :initform nil :initarg :socket)
   (file-stream :initform nil)
   (file-pathname :initarg :file-pathname)
   (on-finish :initform nil :initarg :on-finish)
   (on-error :initform nil :initarg :on-error)
   (on-cancel :initform nil :initarg :on-cancel)))

(defclass task-request (task)
  ())

(defclass task-request-info (task-request)
  ((timestamp :initarg :timestamp)))

(defclass task-request-send-permission (task-request)
  ((users :initarg :users)
   (timeout :initarg :timeout)))

(defclass task-request-file (task-request)
  ((checksum :initarg :checksum)
   (start :initarg :start :initform 0)
   (end :initarg :end :initform 0)
   (user :initarg :user)))

(defclass task-get-info (task)
  ((user :initarg :user)))

(defclass task-send-file (task)
  ((time-waited :initform 0)
   (user :initarg :user)
   (timeout :initarg :timeout)))

(defclass task-get-file (task)
  ((checksum :initarg :checksum)))

(defclass task-get-file-from-user (task-get-file)
  ((user :initarg :user)))

(defclass task-get-file-from-users (task-get-file)
  ((current-user :initform nil)
   (current-part :initform 0 :type fixnum)
   (part-size :initform 0 :type fixnum)
   (digester :initform (if (lodds.config:get-value :validate-checksum)
                           (ironclad:make-digest :sha1)
                           nil))))

(defclass task-get-folder (task)
  ((user :initarg :user)
   (local-path :initarg :local-path)
   (remote-path :initarg :remote-path)
   (items :initform nil)
   (items-done :initform nil)
   (current-task :initform nil)))


(in-package #:lodds.watcher)

(defclass dir-watcher (cl-fs-watcher:watcher)
  ((root-dir-name :type pathname
                  :reader root-dir-name
                  :documentation "contains the root directory name of
                  the watched dir (DIR), relative to
                  root-dir-path. set after
                  initialization. ROOT-DIR-PATH merged with
                  ROOT-DIR-NAME gives DIR. This variable is used to
                  calculate the path for LIST-OF-CHANGE.")
   (root-dir-path :type pathname
                  :reader root-dir-path
                  :documentation "contains the directory name where
                  the root-directory is located. Set after
                  initialization. ROOT-DIR-PATH merged with
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

(defclass watcher ()
  ((alive-p :reader alive-p
            :initform nil
            :type boolean
            :documentation "Flag to check if watcher is
            alive/running, do not set this by hand, this is just a
            indicator! Use lodds.watcher:stop to stop the watcher")
   (started-tracking :accessor started-tracking
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


(in-package #:lodds.event)

(defclass event-queue ()
  ((thread :accessor thread
           :initform nil
           :type bt:thread
           :documentation "The subsystem Thread. Once a subsystem is
           initialized it calls INIT-FN in a new thread.")
   (alive-p :accessor alive-p
            :initform nil
            :type boolean
            :documentation "Flag to check if event-queue is
            alive/running, do not set this by hand, this is just a
            indicator!. To start/stop a the event-queue use start/stop
            from the event package.")
   (queue :accessor queue
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


(in-package #:lodds.event-loop)

(defclass event-loop ()
  ((hook-notifier :initform nil)
   (stop-notifier :initform nil)
   (alive-p :initform nil)
   (thread :initform nil)
   (queue :initform (lparallel.queue:make-queue))))


(in-package #:lodds.listener)

(defclass listener ()
  ((alive-p :initform nil)
   (thread :initform nil
           :type bt:thread)))


(in-package #:lodds.handler)

(defclass handler ()
  ((alive-p :initform nil)
   (thread :initform nil
           :type bt:thread)
   #+os-windows
   (socket :initform nil)))

(in-package #:lodds)

(defclass user-info ()
  ((name :accessor user-name
         :initarg :name
         :initform (error "specify user name")
         :type string
         :documentation "user name, like d4ryus@192.168.2.102")
   (last-message :accessor user-last-message
                 :initarg :last-message
                 :initform (error "specify users last-message timestamp")
                 :type integer
                 :documentation "local timestamp which was created
                 when the last message was received.")
   (ip :accessor user-ip
       :initarg :ip
       :initform (error "specify user ip")
       :type string)
   (port :accessor user-port
         :initarg :port
         :initform (error "specify user port")
         :type integer)
   (last-change :accessor user-last-change
                :initarg :last-change
                :initform (error "specify user last-change")
                :type integer
                :documentation "Remote timestamp which was received on
                last info update.")
   (load :accessor user-load
         :initarg :load
         :initform (error "specify users load")
         :type integer)
   (file-table-name :accessor user-file-table-name
                    :type hashtable
                    :initform (make-hash-table :test 'equal)
                    :documentation "hashtable of users shared files,
                    with their unix namestring as key. Value is a list
                    of file checksum and its size.")
   (file-table-hash :accessor user-file-table-hash
                    :initform (make-hash-table :test 'equal)
                    :type hashtable
                    :documentation "hashmap of users shared files,
                    with their checksum as key. Value is a list of
                    files with share the same checksum.")
   (lock :accessor user-lock
         :initform (bt:make-lock "user-lock")
         :documentation "Look to access member variables.")))

(defclass lodds-server ()
  ((event-loop :initform nil
               :documentation "Event loop which does all input/output
               on sockets and filestreams. See event-loop.lisp")
   (tasks :initform nil
          :documentation "Class which wraps around a hashtable
          containing all running tasks. Can be used to query
          information about load etc. see functions starting with
          'tasks-' inside lodds.task package.")
   (handler :initform nil
            :documentation "Handles incomming connections and creates
            task-request objects.")
   (watcher :initform nil
            :documentation "Watcher which handles changes in shared
            directories.")
   (event-queue :initform nil
                :documentation "Used to transfer events over multiple
                threads")
   (listener :initform nil
             :documentation "Listener class which handles broadcast
             messages")
   (users :accessor users
          :initform (make-hash-table :test #'equal)
          :type hashtable
          :documentation "Hashtable containing all users which
          their broadcast information. This table is updated by
          LISTENER which initiates a task-get-info.")
   (settings :initform nil
             :initarg :settings
             :type hashtable
             :documentation "Settings hashtable which contains all
             server settings. See config.lisp for more info.")))
