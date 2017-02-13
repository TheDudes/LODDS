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
  (let ((info (get-interface-info interface)))
    (when info
      (ip-interfaces:ip-interface-broadcast-address info))))

(defun get-ip-address (interface)
  "returns the ip address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (let ((info (get-interface-info interface)))
    (when info
      (ip-interfaces:ip-interface-address info))))

(defun event-callback (event)
  (format t "log: ~a~%" event))

(defun update-load (load-diff)
  (bt:with-lock-held ((current-load-lock *server*))
    (incf (current-load *server*) load-diff)))

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

(defun switch-name (new-name)
  "Switches servername which is advertised by the server. Also emits a
  :name-changed event with the new name"
  ;;TODO: check if name is accepted
  (let ((old-name (name *server*)))
    (setf (name *server*) new-name)
    (lodds.event:push-event :name-changed
                            (list "Name changed from" old-name
                                  "to" new-name))))

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
  (let* ((started-tracking (lodds.watcher:started-tracking (get-subsystem :watcher)))
         (type (if (or (eql 0 timestamp)
                       (< timestamp started-tracking)
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

(defun get-folder (remote-path remote-root local-path user)
  (lodds.task:submit-task
   (make-instance 'lodds.task:task-get-folder
                  :name "get-folder"
                  :user user
                  :remote-root remote-root
                  :remote-path remote-path
                  :local-path local-path)))

(defmethod initialize-instance :after ((task lodds.task:task-get-file-from-user) &rest initargs)
  (with-accessors ((t-ip lodds.task:get-ip)
                   (t-port lodds.task:get-port)
                   (t-size lodds.task:get-size)) task
    (let ((user (getf initargs :user))
          (checksum (getf initargs :checksum)))
      (lodds.core:split-user-identifier (name ip port) user
        (setf t-ip ip
              t-port (parse-integer port)))
      ;; if size was not given, just download the whole file
      (update-load
       (setf t-size
             (car (get-file-info checksum user)))))))

(defmethod initialize-instance :after ((task lodds.task:task-get-file-from-users) &rest initargs)
  (let ((size (third (first (get-file-info (getf initargs :checksum))))))
    (update-load size)
    (setf (lodds.task:get-size task) size)
    (setf (lodds.task:get-part-size task)
          (let ((64mb (ash 1 26))
                (32mb (ash 1 25)))
            (if (> size 64mb)
                (ash size -4) ;; / 16 (split up into 16 parts
                (if (> size 32mb)
                    (ash size -3) ;; / 8 (split up into 8 parts
                    ;; if file is smaller than 32 mb just
                    ;; download it in one go
                    size))))))

(defmethod initialize-instance :after ((task lodds.task:task-get-folder) &rest initargs)
  (let* ((user (getf initargs :user))
         (remote-path (getf initargs :remote-path))
         (items (get-folder-info remote-path user)))
    (update-load (reduce #'+ items :key #'third))
    (setf (lodds.task:folder-items task)
          items)))

(defun load-chunk (stream-from stream-to size &optional (chunk-size (ash 1 21)))
  (let ((transfered (if (> size chunk-size)
                        chunk-size
                        size)))
    (lodds.core:copy-stream stream-from
                            stream-to
                            transfered)
    (update-load (- transfered))
    transfered))

(defun open-file (file-path)
  (open file-path :direction :output
                  :if-does-not-exist :create
                  :element-type '(unsigned-byte 8)
                  ;; here we could ask the user, overwrite?
                  :if-exists :supersede))

(defun request-file (ip port checksum start end)
  (let ((socket (usocket:socket-connect ip port
                                        :element-type '(unsigned-byte 8))))
    (lodds.low-level-api:get-file (usocket:socket-stream socket) checksum start end)
    socket))

(defmethod lodds.task:run-task ((task lodds.task:task-get-file-from-user))
  (with-accessors ((local-file-path lodds.task:get-local-file-path)
                   (user lodds.task:get-user)
                   (ip lodds.task:get-ip)
                   (port lodds.task:get-port)
                   (checksum lodds.task:get-checksum)
                   (size lodds.task:get-size)
                   (read-bytes lodds.task:get-read-bytes)
                   (socket lodds.task:get-socket)
                   (local-file-stream lodds.task:get-local-file-stream)
                   (on-finish-hook lodds.task:on-finish-hook)) task
    (labels ((cleanup (&optional error-occured error)
               (if error-occured
                   (progn
                     (lodds.event:push-event :error
                                             (list "on get file from user"
                                                   local-file-path ":" error))
                     (update-load (- read-bytes size)))
                   (progn
                     (when on-finish-hook
                       (funcall on-finish-hook))
                     (lodds.event:push-event :info (list "downloaded file"
                                                         local-file-path))))
               (when socket
                 (usocket:socket-close socket))
               (when local-file-stream
                 (close local-file-stream :abort error-occured))))
      (handler-case
          (progn
            (unless socket
              (setf socket (request-file ip port checksum 0 size)))
            (unless local-file-stream
              (setf local-file-stream (open-file local-file-path)))
            (incf read-bytes
                  (load-chunk (usocket:socket-stream socket)
                              local-file-stream
                              (- size read-bytes)))
            (finish-output local-file-stream)
            (if (eql size read-bytes)
                (cleanup)
                (lodds.task:submit-task task)))
        (error (e)
          (cleanup t e))))))

(defun find-best-user (checksum)
  (let ((best-user nil)
        (best-load nil))
    (loop :for (user load . rest) :in (get-file-info checksum)
          :do (when (or (not best-user)
                        (> best-load load))
                (setf best-user user
                      best-load load)))
    (if best-user
      (lodds.core:split-user-identifier (name ip port) best-user
        (lodds.event:push-event :debug (list "best user:" best-user
                                             ", ip:" ip
                                             ", port:" port
                                             ", load:" (lodds.core:format-size best-load)))
        (values ip port))
      (error "Could not find a user who shares ~a~%" checksum))))

(defmethod lodds.task:run-task ((task lodds.task:task-get-file-from-users))
  (with-accessors ((local-file-path lodds.task:get-local-file-path)
                   (checksum lodds.task:get-checksum)
                   (size lodds.task:get-size)
                   (read-bytes lodds.task:get-read-bytes)
                   (socket lodds.task:get-socket)
                   (local-file-stream lodds.task:get-local-file-stream)
                   (current-part lodds.task:get-current-part)
                   (read-bytes-part lodds.task:get-read-bytes-part)
                   (part-size lodds.task:get-part-size)
                   (on-finish-hook lodds.task:on-finish-hook)) task
    (labels ((cleanup (error-occured close-file-p)
               (when socket
                 (usocket:socket-close socket)
                 (setf socket nil))
               (when close-file-p
                 (when local-file-stream
                   (close local-file-stream :abort error-occured)))))
      (handler-case
          (progn
            (unless local-file-stream
              (setf local-file-stream (open-file local-file-path)))
            (unless socket
              (multiple-value-bind (ip port) (find-best-user checksum)
                (setf socket
                      (request-file
                       ip
                       (parse-integer port)
                       checksum
                       (* current-part part-size)
                       (let ((end-next-part (* (+ 1 current-part)
                                               part-size)))
                         (if (> end-next-part
                                size)
                             (progn
                               (setf part-size (- size
                                                  (* current-part part-size)))
                               size)
                             end-next-part))))))
            (let ((written (load-chunk (usocket:socket-stream socket)
                                       local-file-stream
                                       (- part-size read-bytes-part))))
              (incf read-bytes written)
              (incf read-bytes-part written))
            (finish-output local-file-stream)
            (if (eql size read-bytes)
                (progn
                  (when on-finish-hook
                    (funcall on-finish-hook))
                  (lodds.event:push-event :info (list "downloaded file"
                                                      local-file-path))
                  (cleanup nil t))
                (progn
                  (when (eql read-bytes-part part-size)
                    (incf current-part)
                    (setf read-bytes-part 0)
                    (cleanup nil nil))
                  (lodds.task:submit-task task))))
        (error (e)
          (cleanup t t)
          (update-load (- read-bytes size))
          (lodds.event:push-event :error
                                  (list "on get file from users" local-file-path ":" e)))))))

(defmethod lodds.task:run-task ((task lodds.task:task-get-folder))
  (with-accessors ((local-path lodds.task:folder-local-path)
                   (remote-path lodds.task:folder-remote-path)
                   (remote-root lodds.task:folder-remote-root)
                   (items lodds.task:folder-items)
                   (user lodds.task:folder-user)
                   (on-finish-hook lodds.task:on-finish-hook)) task
    (if items
        (destructuring-bind (file checksum size) (pop items)
          (unless checksum
            ;; the file might have changed, if so we should be able to
            ;; get the checksum from remote-path again, if this also
            ;; fails checksum will just be nil, so the following if
            ;; will catch that.
            (setf checksum (get-checksum-from-path remote-path user)))
          ;; remove size from load since the new task will add it again
          (update-load (- size))
          (if checksum
              (lodds.task:submit-task
               (make-instance 'lodds.task:task-get-file-from-users
                              :name "get-file"
                              :checksum checksum
                              :local-file-path (ensure-directories-exist
                                                (concatenate 'string
                                                             local-path
                                                             (subseq file (length remote-root))))
                              ;; resubmit current task-get-folder when file
                              ;; download is complete
                              :on-finish-hook (lambda ()
                                                (lodds.task:submit-task task))))
              (error "The file ~a from user ~a with checksum ~a does not exist anymore."
                     file user checksum)))
        (progn
          (when on-finish-hook
            (funcall on-finish-hook))
          (lodds.event:push-event :info (list "folder"
                                              remote-path
                                              "sucessfully downloaded to"
                                        local-path))))))
