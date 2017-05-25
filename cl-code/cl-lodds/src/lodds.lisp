#|

This file should become a interface to the outer world. It contains
all kinds of handy functions to modify the lodds-server or get
information. This file should (once finished) behave like a interface
with all kinds of handy functions a user of the library needs. Atm
there is still much stuff missing which is not wrapped yet, but some
importand functions are there already (for example:
start,stop,shutdown, ...).

|#

(in-package #:lodds)

(defparameter *server* nil)

(defmethod print-object ((object user-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name
                 last-message
                 load
                 file-table-name) object
      (format stream "~a last-message: ~a load: ~a shares: ~a"
              name
              last-message
              load
              (hash-table-count file-table-name)))))

(defmethod initialize-instance :after ((server lodds-server) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (event-loop settings tasks handler watcher event-queue
               listener)
      server
    (lodds.core:with-server server
      (setf event-loop
            (make-instance 'lodds.event-loop:event-loop))
      (setf settings
            (multiple-value-bind (config error)
                (lodds.config:load-default-config-files)
              (or config
                  (progn
                    (format t "Configuration Error: ~a~%" error)
                    (lodds.config:generate-default-config)))))
      (setf tasks (make-instance 'lodds.task:tasks)
            handler (make-instance 'lodds.handler:handler)
            watcher (make-instance 'lodds.watcher:watcher)
            event-queue (make-instance 'lodds.event:event-queue)
            listener (make-instance 'lodds.listener:listener)))))

(defun event-callback (event)
  (format t "log: ~a~%" event))

(defun switch-config (new-config)
  ;; on fail of validate-config just return the error string
  (or (lodds.config:validate-config new-config)
      (progn
        (setf (slot-value *server* 'settings) new-config)
        (lodds.event:push-event :config-changed))))

(defun update-config (new-config)
  "updates the current config with the new-config. Goes over every key
  on new-config and updates the current one if found and different"
  (maphash (lambda (key value)
             (lodds.config:update-entry key
                                        (car value)))
           new-config))

(defun get-load ()
  (lodds.task:tasks-get-load (get-tasks)))

(defun get-timestamp-last-change ()
  "returns the timestamp of the last change, who would have thought?
  :D"
  (lodds.watcher:last-change
   (get-watcher)))

(defun get-user-list ()
  "Returns List of users who advertised themselfs.
  lodds:*server* needs to be bound.

  CL-USER> (get-user-list)
  => (\"peter@123.321.2.1:1234\"
      \"steve@123.321.2.2:1234\"
      \"josh@123.321.2.3:1234\")

  Use GET-USER-INFO to get information about a user."
  (loop :for key :being :the :hash-key :of (users *server*)
        :collect key))

(defun get-user-info (user)
  "Returns List with information about an given user.
  To get a list of available users see get-user-list.
  lodds:*server* needs to be bound.

  CL-USER> (get-user-info \"someUser@123.123.123.1:1234\")
  => #<LODDS:USER-INFO>

  Returns nil if user is not found."
  (gethash user (users *server*)))

(defun get-user-by-ip (ip)
  "Returns a list with user names (in name@ip:port form) which match
  the given ip. Ip can either be a string (\"192.168.2.1\") or a
  vector (#(192 168 2 1)). The returned usernames can be used by
  GET-USER-INFO to retrieve further info about a user"
  (let ((ip (if (stringp ip)
                (usocket:dotted-quad-to-vector-quad ip)
                ip)))
    (loop :for user-info :being :the :hash-value :of (users lodds:*server*)
          :if (equalp (user-ip user-info) ip)
          :collect (user-name user-info))))

(defun get-file-info (checksum &optional user)
  "Returns information about the given checksum.
  If user is left out it will return a list of user who own the given
  file. The Load, Size and a list of Filenames matching the checksum
  is also returned.

  CL-USER> (get-file-info \"03x87fjlwd...\")
  => ((\"d4ryus@192.168.2.2:4567\" 32 1234 (\"/shares/a.txt\"
                                            \"/shares/subfolder/x.txt\"))
      (\"pete@192.168.2.4:1234\" 1320 1234 (\"/public/z.txt\")))
  which states that d4ryus@... and pete@... share the given file,
  d4ryus@...  has a load of 32 bytes and the file is located at
  \"shares/a.txt\" and \"shares/subfolder/x.txt\" on his
  system. pete@... has a load of 1320 bytes and the file is located at
  \"/public/z.txt\". The third element in the list describes the
  filesize, which is 1234 on both.

  CL-USER> (get-file-info \"03x87fjlwd...\" \"d4ryus@192.168.2.2:4567\")
  => (1234 (\"/shares/a.txt\" \"/shares/subfolder/x.txt\"))"
  (if user
      (let ((user-info (get-user-info user)))
        (unless user-info
          (return-from get-file-info nil))
        (let ((locations (gethash checksum (user-file-table-hash user-info))))
          (unless locations
            (return-from get-file-info nil))
          (destructuring-bind (checksum size) (gethash (car locations) (user-file-table-name user-info))
            (declare (ignore checksum))
            (list size
                  locations))))
      (remove-if
       #'null
       (loop :for user :in (get-user-list)
             :collect (let ((info (get-file-info checksum user)))
                        (when info
                          (apply #'list user (user-load (get-user-info user)) info)))))))

(defun get-checksum-from-path (file-path user)
  "Gets the checksum of the given FILE-PATH from the given USER,
  returns nil if FILE-PATH or USER does not exist.
  lodds:*server* needs to be bound

  CL-USER> (get-checksum-from-path \"/shared/\" \"d4ryus@192.168.321.123\")
  => \"10fha02fh2093f...\")"
  (let ((user-info (get-user-info user)))
    (when user-info
      (car (gethash file-path (user-file-table-name user-info))))))

(defun get-folder-info (folder user)
  "Returns a list of Files with their Checksums and size which the
  given FOLDER of given USER contains. If the User ist not found or
  the User does not share FOLDER, nil is returned.
  lodds:*server* needs to be bound

  CL-USER> (get-folder-info \"d4ryus@192.168.2.123\" \"/shared/\")
  => ((\"this.txt\" \"10hf198f...\" 10)
      (\"that.txt\" \"fj90823f...\" 3213)
      (\"another-folder/a.txt\" \"20h2ny9f...\" 9513)
      (\"another-folder/b.txt\" \"gh0q09mb...\") 3183)"
  (let ((user-info (get-user-info user)))
    (when user-info
      (bt:with-lock-held ((user-lock user-info))
        (loop :for path :being :the :hash-key :of (user-file-table-name user-info)
              :using (hash-value checksum+size)
              :if (cl-strings:starts-with path folder)
              :collect (cons path checksum+size))))))

(defun get-file-changes (current-timestamp &optional (timestamp nil))
  "returns a list of all changes since the given timestamp. if
  timestamp is nil a full list of all files will be returnd"
  (let* ((watcher (get-watcher))
         (lock (lodds.watcher::list-of-changes-lock watcher)))
    (bt:with-lock-held (lock)
      (if timestamp
          (reverse
           (loop :for (ts . change) :in (lodds.watcher:list-of-changes watcher)
                 ;; only collect infos with timestamps not equal to
                 ;; CURRENT-TIMESTAMP and which are not older than TIMESTAMP
                 :if (and (>= ts timestamp)
                          (not (eql ts current-timestamp)))
                 :collect change :into result
                 :else
                 ;; stop and return only if this :else was triggerd by TS
                 ;; beeing older then TIMESTAMP, and not if TS was eql to
                 ;; CURRENT-TIMESTAMP
                 :do (when (< ts timestamp)
                       (return result))
                 :finally (return result)))
          (apply #'append
                 (mapcar
                  (lambda (watcher)
                    (let ((files (lodds.watcher:get-all-tracked-file-infos watcher)))
                      (loop :for info :in files
                            :collect (cons :add info))))
                  (lodds.watcher:dir-watchers (get-watcher))))))))

(defun update-user (user update)
  (let ((user-info (lodds:get-user-info user)))
    (when user-info
      (with-slots (name last-change file-table-hash file-table-name)
          user-info
        (destructuring-bind (type timestamp changes) update
          (when (eql type :all)
            (setf file-table-hash (make-hash-table :test 'equal)
                  file-table-name (make-hash-table :test 'equal)))
          (loop :for (typ cs size filename) :in changes
                :do (ecase typ
                      (:add (let ((val (gethash cs file-table-hash)))
                              (unless (find filename val :test #'string=)
                                (setf (gethash cs file-table-hash)
                                      (cons filename val)))
                              (setf (gethash filename file-table-name)
                                    (list cs size))))
                      (:del (let ((new-val (remove filename (gethash cs file-table-hash)
                                                   :test #'string=)))
                              (if new-val
                                  (setf (gethash cs file-table-hash)
                                        new-val)
                                  (remhash cs file-table-hash))
                              (remhash filename file-table-name)))))
          (setf last-change timestamp)
          (lodds.event:push-event :list-update
                                  name
                                  type
                                  timestamp
                                  changes))))))

(defun start ()
  (lodds.event:start)
  (lodds.event:push-event :info "starting up lodds...")
  (lodds.listener:start)
  (lodds.handler:start)
  (lodds.event-loop:start)
  (lodds.event:push-event :info "lodds started."))

(defun stop ()
  (lodds.event:push-event :info "stopping lodds...")
  (lodds.handler:stop)
  (lodds.listener:stop)
  (lodds.event-loop:stop)
  (lodds.event:push-event :info "lodds stopped."))

(defun shutdown ()
  "shuts down the whole server, removes all handles and joins all
  spawned threads."
  (lodds.event:push-event :shutdown)
  (stop)
  (lodds.watcher:stop)
  (lodds.event:stop))

(defun generate-info-response (timestamp)
  (let* ((started-tracking (lodds.watcher:started-tracking (get-watcher)))
         (type (if (or (eql 0 timestamp)
                       (<= timestamp started-tracking)
                       (not (lodds.watcher:alive-p (get-watcher))))
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

(defun find-best-user (checksum)
  "Returns the best user (in terms of load) who shares the given file,
nil if no user is found"
  (let ((best-user nil)
        (best-load nil))
    (loop :for (user load . rest) :in (lodds:get-file-info checksum)
          :do (when (or (not best-user)
                        (> best-load load))
                (setf best-user user
                      best-load load)))
    (when best-user
      (lodds.event:push-event :debug
                              (format nil
                                      "best user: ~a"
                                      best-user))
      best-user)))

(defun get-file (file-pathname checksum &optional user)
  (lodds.task:task-run
   (if user
       (make-instance 'lodds.task:task-get-file-from-user
                      :tasks (get-tasks)
                      :checksum checksum
                      :user user
                      :file-pathname file-pathname)
       (make-instance 'lodds.task:task-get-file-from-users
                      :tasks (get-tasks)
                      :checksum checksum
                      :file-pathname file-pathname))))

(defun get-folder (full-folder-path local-pathname user)
  "gets given folder and saves it to local-path, full-folder-path
describes the full unix folder path of the wanted folder."
  (lodds.task:task-run
   (make-instance 'lodds.task:task-get-folder
                  :tasks (get-tasks)
                  :user user
                  :remote-path (lodds.core:ensure-trailing-slash full-folder-path)
                  :local-path local-pathname)))

(defun send-file (file-pathname user timeout)
  (lodds.task:task-run
   (make-instance 'lodds.task:task-send-file
                  :tasks (get-tasks)
                  :timeout timeout
                  :user user
                  :file-pathname file-pathname)))

(defun remove-old-users (&optional (current-time (lodds.core:get-timestamp)))
  "removes all users older than user-timeout"
  (let ((users (lodds:users lodds:*server*))
        (timeout (lodds.config:get-value :user-timeout)))
    (maphash (lambda (key user)
               (when (> (- current-time (lodds:user-last-message user))
                        timeout)
                 (remhash key users)
                 (lodds.event:push-event :user-removed
                                         key)))
             users)))

(defun get-status (&optional (format nil))
  "Describes Lodds current status.
Total Load: Sum of all Loads accross the Network
Load: How much load the client currently has
Tasks: How many tasks are currently running
Network Files: Amount of Files in the network (non Unique)
Shared Folders: Amount of currently shared folders
Users: Amount of User on the Network"
  (let ((tasks (get-tasks))
        (total-load 0)
        (total-shared 0))
    (dolist (user (get-user-list))
      (let ((info (get-user-info user)))
        (incf total-load (user-load info))
        (incf total-shared (hash-table-count
                            (user-file-table-name info)))))
    (let ((tasks (lodds.task:tasks-get-task-count tasks))
          (load (lodds.task:tasks-get-load tasks))
          (users (length (lodds:get-user-list)))
          (shared-folders (length (lodds.watcher:get-shared-folders))))
      (if format
          (list (cons "Total Load" (lodds.core:format-size total-load))
                (cons "Load" (lodds.core:format-size load))
                (cons "Tasks" (format nil "~:d" tasks))
                (cons "Shared Folders" (format nil "~:d" shared-folders))
                (cons "Users" (format nil "~:d" users))
                (cons "Network Files" (format nil "~:d" total-shared)))
          (list (cons "Total Load" total-load)
                (cons "Load" load)
                (cons "Tasks" tasks)
                (cons "Shared Folders" shared-folders)
                (cons "Users" users)
                (cons "Network Files" total-shared))))))

(defun user-is-trusted (user)
  (find user (lodds.config:get-value :trusted-users)
        :test #'equal))

(defun user-is-blocked (user)
  (find user (lodds.config:get-value :blocked-users)
        :test #'equal))

(defun untrust-user (user)
  (when (user-is-trusted user)
    (lodds.config:update-entry
     :trusted-users
     (remove user
             (lodds.config:get-value :trusted-users)
             :test #'equal))))

(defun trust-user (user)
  (when (user-is-blocked user)
    (unblock-user user))
  (unless (user-is-trusted user)
    (lodds.config:update-entry
     :trusted-users
     (append (list user)
             (lodds.config:get-value :trusted-users)))))

(defun unblock-user (user)
  (when (user-is-blocked user)
    (lodds.config:update-entry
     :blocked-users
     (remove user
             (lodds.config:get-value :blocked-users)
             :test #'equal))))

(defun block-user (user)
  (when (user-is-trusted user)
    (untrust-user user))
  (unless (user-is-blocked user)
    (lodds.config:update-entry
     :blocked-users
     (append (list user)
             (lodds.config:get-value :blocked-users)))))

(defun get-event-loop ()
  (slot-value lodds:*server* 'event-loop))

(defun get-tasks ()
  (slot-value lodds:*server* 'tasks))

(defun get-watcher ()
  (slot-value lodds:*server* 'watcher))

(defun get-event-queue ()
  (slot-value lodds:*server* 'event-queue))

(defun get-listener ()
  (slot-value lodds:*server* 'listener))

(defun get-handler ()
  (slot-value lodds:*server* 'handler))
