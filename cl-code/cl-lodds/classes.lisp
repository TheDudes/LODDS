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
   (on-finish :initform nil :initarg :on-finish)
   (on-error :initform nil :initarg :on-error)
   (on-cancel :initform nil :initarg :on-cancel)))

(defclass task-request (task)
  ())

(defclass task-request-info (task-request)
  ((timestamp :initarg :timestamp)))

(defclass task-request-send-permission (task-request)
  ((users :initarg :users)
   (timeout :initarg :timeout)
   (filename :initarg :filename)))

(defclass task-request-file (task-request)
  ((filename :initform nil)
   (checksum :initarg :checksum)
   (start :initarg :start :initform 0)
   (end :initarg :end :initform 0)
   (user :initarg :user)))

(defclass task-get-info (task)
  ((user :initarg :user)))

(defclass task-send-file (task)
  ((filename :initarg :filename)
   (time-waited :initform 0)
   (user :initarg :user)
   (timeout :initarg :timeout)))

(defclass task-get-file (task)
  ((local-file-path :initarg :local-file-path)
   (checksum :initarg :checksum)))

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
   (remote-root :initarg :remote-root)
   (remote-path :initarg :remote-path)
   (items :initform nil)
   (items-done :initform nil)
   (current-task :initform nil)))


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
           :type bt:thread)))


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
         :type string
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
  ((event-loop :initform (make-instance 'lodds.event-loop:event-loop)
               :documentation "Event loop which does all input/output
               on sockets and filestreams. See event-loop.lisp")
   (tasks :initform (make-instance 'lodds.task:tasks)
          :documentation "Class which wraps around a hashtable
          containing all running tasks. Can be used to query
          information about load etc. see functions starting with
          'tasks-' inside lodds.task package.")
   (handler :initform (make-instance 'lodds.handler:handler)
            :documentation "Handles incomming connections and creates
            task-request objects.")
   (watcher :initform (make-instance 'lodds.watcher:watcher)
            :documentation "Watcher which handles changes in shared
            directories.")
   (event-queue :initform (make-instance 'lodds.event:event-queue)
                :documentation "Used to transfer events over multiple
                threads")
   (listener :initform (make-instance 'lodds.listener:listener)
             :documentation "Listener class which handles broadcast
             messages")
   (clients :accessor clients
            :initform (make-hash-table :test #'equal)
            :type hashtable
            :documentation "Hashtable containing all clients which
            their broadcast information. This table is updated by
            LISTENER. TODO: implement something to retrieve a copy.")
   (settings :initform (multiple-value-bind (config error)
                           (lodds.config:load-default-config-files)
                         (or config
                             (progn
                               (format t "Configuration Error: ~a" error)
                               (lodds.config:generate-default-config))))
             :initarg :settings
             :type hashtable
             :documentation "Settings hashtable which contains all
             server settings. See config.lisp for more info.")))
