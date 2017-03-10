;;;; lodds.lisp

(in-package #:lodds)

(defparameter *server* nil)

(defmacro with-server (server &body body)
  `(let* ((*server* ,server)
          (bt:*default-special-bindings*
            (append (list (cons '*server* ,server))
                    bt:*default-special-bindings*)))
     ,@body))

(defmethod print-object ((object client-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (c-name
                 c-last-message
                 c-load
                 c-file-table-name) object
      (format stream "~a last-message: ~a load: ~a shares: ~a"
              c-name
              c-last-message
              c-load
              (hash-table-count c-file-table-name)))))

(defun event-callback (event)
  (format t "log: ~a~%" event))

(defun on-config-change (server  &rest event-args)
  (declare (ignore event-args))
  (with-server server
    (let ((advertiser (lodds:get-subsystem :advertiser))
          (incognito (lodds.config:get-value :incognito-mode)))
      (cond
        ((and incognito
              (lodds.subsystem:alive-p advertiser))
         (lodds.subsystem:stop advertiser))
        ((and (not incognito)
              (not (lodds.subsystem:alive-p advertiser)))
         (lodds.subsystem:start advertiser))))))

(defmethod initialize-instance :after ((server lodds-server) &rest initargs)
  (declare (ignorable initargs))
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
                          :init-fn #'lodds.event:run)
           ;; TASKER subsystem, handles and schedules several
           ;; tasks on a lparrallel:kernel. Waits on
           ;; event-queue for :task events and then executes
           ;; those.
           (make-instance 'lodds.task:tasker
                          :name :tasker
                          :init-fn nil)
           ;; LISTENER subsystem listens on broadcast address of
           ;; the set INTERFACE and BROADCAST-PORT member of
           ;; server for advertisements from other clients
           (make-instance 'lodds.subsystem:subsystem
                          :name :listener
                          :init-fn #'lodds.listener:run)
           ;; HANDLER subsystem, listens for incomming
           ;; connections and handles those (starts threads etc).
           (make-instance 'lodds.subsystem:subsystem
                          :name :handler
                          :init-fn #'lodds.handler:run)
           ;; ADVERTISER subystem, broadcasts information to
           ;; other clients on broadcast address of INTERFACE and
           ;; BROADCAST-PORT.
           (make-instance 'lodds.subsystem:subsystem
                          :name :advertiser
                          :init-fn #'lodds.advertiser:run)
           ;; WATCHER subsystem, handles filesystem changes,
           ;; updates/handles local list of shared files
           (make-instance 'lodds.watcher:watcher
                          :name :watcher
                          :init-fn nil)))
    (lodds.event:add-callback :lodds-config-change
                              (lambda (&rest args)
                                (on-config-change server args))
                              :config-changed)))

(defun switch-config (new-config)
  ;; on fail of validate-config just return the error string
  (or (lodds.config:validate-config new-config)
      (progn
        (setf (slot-value *server* 'settings) new-config)
        (lodds.event:push-event :config-changed nil))))

(defun update-config (new-config)
  "updates the current config with the new-config. Goes over every key
  on new-config and updates the current one if found and different"
  (maphash (lambda (key value)
             (lodds.config:update-entry key
                                        (car value)))
           new-config))

(defun get-subsystem (name)
  "returns the requested subsystem, if not found nil will returned"
  (find name (subsystems *server*) :key #'lodds.subsystem:name))

(defun get-load ()
  (lodds.task:get-load (lodds:get-subsystem :tasker)))

(defun get-timestamp-last-change ()
  "returns the timestamp of the last change, who would have thought?
  :D"
  (lodds.watcher:last-change
   (get-subsystem :watcher)))

(defun get-user-list ()
  "Returns List of users who advertised themselfs.
  lodds:*server* needs to be bound.

  CL-USER> (get-user-list)
  => (\"peter@123.321.2.1:1234\"
      \"steve@123.321.2.2:1234\"
      \"josh@123.321.2.3:1234\")

  Use GET-USER-INFO to get information about a user."
  (loop :for key :being :the :hash-key :of (clients *server*)
        :collect key))

(defun get-user-info (user)
  "Returns List with information about an given user.
  To get a list of available users see get-user-list.
  lodds:*server* needs to be bound.

  CL-USER> (get-user-info \"someUser@123.123.123.1:1234\")
  => #<LODDS:CLIENT-INFO>

  Returns nil if user is not found."
  (gethash user (clients *server*)))

(defun get-user-by-ip (ip)
  "Returns a list with user names (in name@ip:port form) which match
  the given ip. Ip can either be a string (\"192.168.2.1\") or a
  vector (#(192 168 2 1)). The returned usernames can be used by
  GET-USER-INFO to retrieve further info about a user"
  (let ((ip (if (stringp ip)
                (usocket:dotted-quad-to-vector-quad ip)
                ip)))
    (loop :for user-info :being :the :hash-value :of (clients lodds:*server*)
          :if (equalp (c-ip user-info) ip)
          :collect (c-name user-info))))

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
        (let ((locations (gethash checksum (c-file-table-hash user-info))))
          (unless locations
            (return-from get-file-info nil))
          (destructuring-bind (checksum size) (gethash (car locations) (c-file-table-name user-info))
            (declare (ignore checksum))
            (list size
                  locations))))
      (remove-if
       #'null
       (loop :for user :in (get-user-list)
             :collect (let ((info (get-file-info checksum user)))
                        (when info
                          (apply #'list user (c-load (get-user-info user)) info)))))))

(defun get-checksum-from-path (file-path user)
  "Gets the checksum of the given FILE-PATH from the given USER,
  returns nil if FILE-PATH or USER does not exist.
  lodds:*server* needs to be bound

  CL-USER> (get-checksum-from-path \"/shared/\" \"d4ryus@192.168.321.123\")
  => \"10fha02fh2093f...\")"
  (let ((user-info (get-user-info user)))
    (when user-info
      (car (gethash file-path (c-file-table-name user-info))))))

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
      (bt:with-lock-held ((c-lock user-info))
        (loop :for path :being :the :hash-key :of (c-file-table-name user-info)
              :using (hash-value checksum+size)
              :if (cl-strings:starts-with path folder)
              :collect (cons path checksum+size))))))

(defun get-file-changes (current-timestamp &optional (timestamp nil))
  "returns a list of all changes since the given timestamp. if
  timestamp is nil a full list of all files will be returnd"
  (let* ((watcher (get-subsystem :watcher))
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
                  (lodds.watcher:dir-watchers (get-subsystem :watcher))))))))

(defun start ()
  "Starts up all subsystem of currently bound *server*"
  (lodds.subsystem:start (lodds:get-subsystem :event-queue))
  (lodds.subsystem:start (lodds:get-subsystem :tasker))
  (lodds.subsystem:start (lodds:get-subsystem :listener))
  (lodds.subsystem:start (lodds:get-subsystem :handler))
  (unless (lodds.config:get-value :incognito-mode)
    (lodds.subsystem:start (lodds:get-subsystem :advertiser))))

(defun stop ()
  "Stops all subsystems besides event-queue and watcher of currently
  bound *server*. Calling stop will stop the tasker, advertiser,
  handler and listener. This can be used by a gui to \"stop\" all
  connections. By not shutting down the Event-loop stuff like changing
  the settings will still work. To cleanup and end the lodds-server
  call shutdown"
  (lodds.subsystem:stop (lodds:get-subsystem :tasker))
  (lodds.subsystem:stop (lodds:get-subsystem :listener))
  (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
  (lodds.subsystem:stop (lodds:get-subsystem :handler)))

(defun shutdown ()
  "shuts down the whole server, removes all handles and joins all
  spawned threads."
  (lodds.event:push-event :shutdown nil)
  (let ((event-queue nil))
    (loop :for subsystem :in (subsystems *server*)
          :if (eql (lodds.subsystem:name subsystem)
                   :event-queue)
            :do (setf event-queue subsystem)
          :else
            :do (lodds.subsystem:stop subsystem))
    ;; stop event-queue after the others, so i can see stopped messages
    (when event-queue
      (lodds.subsystem:stop event-queue)
      (lodds.event:push-event :done nil))))

(defun generate-info-response (timestamp)
  (let* ((started-tracking (lodds.watcher:started-tracking (get-subsystem :watcher)))
         (type (if (or (eql 0 timestamp)
                       (<= timestamp started-tracking)
                       (not (lodds.subsystem:alive-p (get-subsystem :watcher))))
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

(defun get-file (local-file-path checksum &optional user)
  (lodds.task:submit-task
   (if user
       (make-instance 'lodds.task:task-get-file-from-user
                      :name "get-file-from-user"
                      :checksum checksum
                      :user user
                      :local-file-path local-file-path)
       (make-instance 'lodds.task:task-get-file-from-users
                      :name "get-file"
                      :checksum checksum
                      :local-file-path local-file-path))))

(defun get-folder (full-folder-path local-path user)
  "gets given folder and saves it to local-path"
  (setf full-folder-path
        (lodds.core:add-missing-slash full-folder-path))
  (let ((folder (lodds.core:escaped-get-folder-name full-folder-path)))
    (lodds.task:submit-task
     (make-instance 'lodds.task:task-get-folder
                    :name "get-folder"
                    :user user
                    :remote-root (subseq full-folder-path
                                         0
                                         (- (length full-folder-path)
                                            (length folder)))
                    :remote-path full-folder-path
                    :local-path local-path))))

(defun send-file (file user timeout)
  (let ((task (make-instance 'lodds.task:task-send-file
                              :name "send-file"
                              :user user
                              :filepath file
                              :timeout timeout)))
    (lodds.task:submit-task task)
    (slot-value task 'lodds.task::id)))


(defun get-status (&optional (format nil))
  "Describtes Lodds current status.
Tasks: How many tasks are currently running
Load: How much load the client currently has
Users: Amount of User on the Network
Shared Folders: Amount of currently shared folders
Network Files: Amount of Files in the network (non Unique)
Total Load: Sum of all Loads accross the Network"
  (let ((tasker (get-subsystem :tasker))
        (total-load 0)
        (total-shared 0))
    (dolist (user (get-user-list))
      (let ((info (get-user-info user)))
        (incf total-load (lodds:c-load info))
        (incf total-shared (hash-table-count
                            (c-file-table-name info)))))
    (let ((tasks (lodds.task:get-task-count tasker))
          (load (lodds.task:get-load tasker))
          (users (length (lodds:get-user-list)))
          (shared-folders (length (lodds.watcher:get-shared-folders))))
      (if format
          (list (cons "Tasks" (format nil "~:d" tasks))
                (cons "Load" (lodds.core:format-size load))
                (cons "Users" (format nil "~:d" users))
                (cons "Shared Folders" (format nil "~:d" shared-folders))
                (cons "Network Files" (format nil "~:d" total-shared))
                (cons "Total Load" (lodds.core:format-size total-load)))
          (list (cons "Tasks" tasks)
                (cons "Load" load)
                (cons "Users" users)
                (cons "Shared Folders" shared-folders)
                (cons "Network Files" total-shared)
                (cons "Total Load" total-load))))))

(defun get-status-doc ()
  "Calling (documentation 'lodds:get-status 'function) returns nil,
dunno why. But thats why this wrapper exists."
  (documentation 'get-status 'function))
