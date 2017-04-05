#|

Tasks are wrapper around network transfers. For example if a user
requests info or a file, a task gets created (task-request-info,
task-request-file). Tasks get cleaned up by the task-cleanup
method. The task-run method does the actual work it is wrapped in a
:around method to ensure the task gets cleaned up in case of an
error. Other methods are task-info to retrieve a string with
information about a task and task-cancel to cancel a running
task. Each task-run is run in a new thread, if not stated otherwise
(:in-thread nil on task creation). All task methods are prefixed with
task-, all tasks functions with tasks-. The tasks- functions can be
used to retrieve info about all tasks, for example tasks-get-load can
be used to retrieve the load all currently running tasks produce.

|#

(in-package #:lodds.task)

(defun tasks-get-load (&optional (tasks (lodds:get-tasks)))
  (with-slots (tasks lock) tasks
    (let ((loads nil))
      (bt:with-recursive-lock-held (lock)
        (setf loads
              (loop :for task :being :the :hash-value :of tasks
                    :collect (task-load task))))
      (if loads
          (reduce #'+ loads)
          0))))

(defun tasks-get-task-progresses (&optional (tasks (lodds:get-tasks)))
  "returns a list containing each tasks id, total-load, progress, type
  and info.
  CL-USER> (get-task-progresses tasker)
  => ((\"id1234\" 5903 1324 :send-file \"blub.txt\")
      (\"id3142\" 141204 4214 :get-file \"somename@123.123.1.2:8282\"))
  Each of the inner lists describe a task, with its id, its maximum
  load, already processed load, type and info."
  (with-slots (tasks lock) tasks
    (bt:with-recursive-lock-held (lock)
      (loop :for task :being :the :hash-value :of tasks
            :collect (with-slots (id total-load bytes-transfered) task
                       (list id
                             total-load
                             bytes-transfered
                             (type-of task)
                             (task-info task)))))))

(defun tasks-get-task-count (&optional (tasks (lodds:get-tasks)))
  (hash-table-count (slot-value tasks 'tasks)))

(defun tasks-get-task-by-id (task-id &optional (tasks (lodds:get-tasks)))
  "Returns task with given id, nil if task is not found"
  (gethash task-id (slot-value tasks 'tasks)))

(let ((to-be-killed nil)
      (to-be-interrupted nil))
  (defun tasks-cleanup (&optional (tasks (lodds:get-tasks)))
    (dolist (task to-be-killed)
      (handler-case
          (with-slots (thread id) task
            (when (tasks-get-task-by-id id)
              (task-cleanup task "Killed")
              (when (and thread
                         (bt:thread-alive-p thread))
                (bt:destroy-thread thread))))
        (error (e)
          (lodds.event:push-event :error
                                  "Error Killing task" task e))))
    (setf to-be-killed nil)
    (dolist (task to-be-interrupted)
      (handler-case
          (with-slots (thread id) task
            (when (tasks-get-task-by-id id)
              (when (and thread
                         (bt:thread-alive-p thread))
                (bt:interrupt-thread thread
                                     (lambda ()
                                       (error "-- Die --"))))
              (push task to-be-killed)))
        (error (e)
          (lodds.event:push-event :error
                                  "Error Interrupting task" task e))))
    (bt:with-recursive-lock-held ((slot-value tasks 'lock))
      (setf to-be-interrupted
            (loop :for task :being :the :hash-value :of (slot-value tasks 'tasks)
                  :when (and (slot-value task 'canceled)
                             (not (find (task-id task)
                                        to-be-killed
                                        :key #'task-id
                                        :test #'equal)))
                  :collect task)))))

;; initialize-instance for each task to set the id on init

(defmethod initialize-instance :after ((task task) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (tasks id) task
    (with-slots (lock tasks id-counter) tasks
      (bt:with-lock-held (lock)
        (setf id (format nil "id-~a" (incf id-counter)))
        (setf (gethash id tasks) task)))))

;; print-object functions for all tasks

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~a"
            (task-id task))))

(defmethod print-object ((task task-request-info) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (timestamp) task
      (format stream "timestamp ~a"
              timestamp))))

(defmethod print-object ((task task-request-send-permission) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id total-load timeout file-pathname) task
      (format stream "~a :size ~a :timeout ~a :file-pathname ~a"
              id
              (lodds.core:format-size total-load)
              timeout
              (lodds.core:format-pathname file-pathname)))))

(defmethod print-object ((task task-request-file) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id checksum start end file-pathname) task
      (format stream "~a :checksum ~a :file-pathname ~a :start ~:d :end ~:d"
              id
              (lodds.core:format-checksum checksum)
              (lodds.core:format-pathname file-pathname)
              start
              end))))

(defmethod print-object ((task task-get-info) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id user) task
      (format stream "~a :user ~a"
              id
              user))))

(defmethod print-object ((task task-send-file) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id ip file-pathname timeout total-load) task
      (format stream "~a file-pathname: ~a timeout: ~a size: ~a"
              id
              (lodds.core:format-pathname file-pathname)
              timeout
              (lodds.core:format-size total-load)))))

(defmethod print-object ((task task-get-file-from-user) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id file-pathname total-load user checksum) task
      (format stream "~a :file-pathname ~a size: ~a :user ~a :checksum ~a"
              id
              (lodds.core:format-pathname file-pathname)
              (lodds.core:format-size total-load)
              user
              (lodds.core:format-checksum checksum)))))

(defmethod print-object ((task task-get-file-from-users) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id file-pathname part-size total-load checksum) task
      (format stream "~a :file-pathname ~a size: ~a :part-size ~a :checksum ~a"
              id
              (lodds.core:format-pathname file-pathname)
              (lodds.core:format-size total-load)
              (lodds.core:format-size part-size)
              (lodds.core:format-checksum checksum)))))

(defmethod print-object ((task task-get-folder) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id items items-done remote-path) task
      (format stream "~a items: ~a items-done: ~a folder: ~a"
              id
              (length items)
              (length items-done)
              remote-path))))

;; task-cancel methods

(defgeneric task-cancel (task)
  (:documentation "can be used to cancel a running task"))

(defmethod task-cancel ((task task))
  (with-slots (canceled tasks) task
    (setf canceled t)))

(defmethod task-cancel ((id string))
  (let ((task (tasks-get-task-by-id id)))
    (when task
      (task-cancel task))))

(defmethod task-cancel ((task task-get-folder))
  (with-slots (current-task) task
    (when current-task
      (task-cancel current-task)))
  (call-next-method))

;; task-info methods to retrieve a information string from a given task

(defgeneric task-info (task)
  (:documentation "Returns a information string about a given task"))

(defmethod task-info :around ((task task))
  (with-slots (canceled id) task
    (format nil "~a~a"
            (if canceled "- Canceled - " "")
            (handler-case (call-next-method)
              (error (e)
                (format nil "ERROR: ~a" e))))))

(defmethod task-info ((task task))
  (format nil "~a" task))

(defmethod task-info ((id string))
  (let ((task (tasks-get-task-by-id id)))
    (when task
      (task-info task))))

(defmethod task-info ((task task-request-send-permission))
  (with-slots (file-pathname users) task
    (format nil "[Request] ~a: ~a" users file-pathname)))

(defmethod task-info ((task task-request-file))
  (with-slots (user total-load file-pathname) task
    (format nil "[Upload] (~a - ~a): ~a"
            user
            (lodds.core:format-size total-load)
            file-pathname)))

(defmethod task-info ((task task-send-file))
  (with-slots (file-pathname user time-waited timeout) task
    (format nil "[Send File] (~a~a): ~a "
            user
            (if time-waited
                (format nil " (~a/~a)"
                        (lodds.core:format-seconds time-waited)
                        (lodds.core:format-seconds timeout))
                "")
            file-pathname)))

(defmethod task-info ((task task-get-file-from-user))
  (with-slots (file-pathname user) task
    (format nil "[Download] (~a): ~a "
            user
            file-pathname)))

(defmethod task-info ((task task-get-file-from-users))
  (with-slots (current-user file-pathname) task
    (format nil "[Download] (~a): ~a"
            (or current-user "none")
            file-pathname)))

(defmethod task-info ((task task-get-folder))
  (with-slots (items items-done) task
    (let* ((left (length items))
           (done (length items-done))
           (total (+ left done)))
      (format nil "[Folder Download] (File ~a/~a): ~a"
              done
              total
              (or (car (car items-done)) "none")))))

;; helper functions

(defun secure-close (socket)
  (when socket
    (handler-case
        (usocket:socket-close socket)
      (error (e)
        (declare (ignore e))))))

(defun connect (ip port)
  (let ((socket (usocket:socket-connect ip port
                                        :element-type '(unsigned-byte 8)
                                        :timeout 1)))
    (lodds.core:set-socket-timeout socket
                                   (lodds.config:get-value :socket-timeout))
    socket))

(defun socket-info (socket)
  (when socket
    (values (usocket:get-peer-address socket)
            (usocket:get-peer-port socket))))

(defun open-file (file-pathname direction)
  (ecase direction
    (:input
     (open file-pathname
           :direction :input
           :element-type '(unsigned-byte 8)))
    (:output
     (open file-pathname
           :direction :output
           :if-does-not-exist :create
           :if-exists :supersede
           :element-type '(unsigned-byte 8)))))

(defun transfer-chunk (stream-from stream-to max &optional digester (chunk-size (ash 1 18)))
  "Transfers bytes from STREAM-FROM to STREAM-TO. It will never
  transfer more then (max size chunk-size) bytes. Returns the Amount
  of transfered Bytes. Updates digester if given"
  (lodds.core:copy-stream stream-from
                          stream-to
                          (if (> max chunk-size)
                              chunk-size
                              max)
                          digester))

(defun task-transfer (task direction &optional max digester)
  (with-slots (socket file-stream
               bytes-transfered total-load
               canceled)
      task
    (let* ((max (or max total-load))
           (stream-from nil)
           (stream-to nil))
      (ecase direction
        (:socket-to-file
         (setf stream-from (usocket:socket-stream socket))
         (setf stream-to file-stream))
        (:file-to-socket
         (setf stream-from file-stream)
         (setf stream-to (usocket:socket-stream socket))))
      (loop
        :do (incf bytes-transfered
                  (transfer-chunk stream-from
                                  stream-to
                                  (- max bytes-transfered)
                                  digester))
        :if (> bytes-transfered max)
        :do (error "Too much transfered")
        :while (and (not (eql bytes-transfered max))
                    (not canceled))))))

(defun handle-send-permission-request (tasks socket size timeout file-pathname)
  (let* ((users (lodds:get-user-by-ip (socket-info socket)))
         (action
           ;; get type of action
           (cond
             ((lodds.config:get-value :deny-requests)
              :deny)
             ((null users)
              (if (lodds.config:get-value :allow-unkown-user-send)
                  :ask
                  :unknown))
             ((intersection users (lodds.config:get-value :blocked-users)
                            :test #'equal)
              :blocked)
             ((subsetp users (lodds.config:get-value :trusted-users)
                       :test #'equal)
              :accept)
             (t :ask))))
    (when (and (eql action :ask)
               (not (lodds.event:callback-exists-p :send-permission)))
      (setf action :no-callback))
    (ecase action
      (:deny (lodds.event:push-event
              :send-permission
              "received and denied (deny requests true)"))
      (:blocked (lodds.event:push-event
                 :send-permission
                 (format nil "received and denied (user ~{~a~^and~} blocked)"
                         users)))
      (:no-callback (lodds.event:push-event
                     :send-permission
                     "received and denied (blocking send-permissions)"))
      (:unknown (lodds.event:push-event
                 :send-permission
                 "received and denied (blocking receiving files from unkown users)"))
      ((:accept :ask)
       (let ((task (make-instance 'task-request-send-permission
                                  :tasks tasks
                                  :users (or users (list "unknown"))
                                  :socket socket
                                  :total-load size
                                  :timeout timeout
                                  :file-pathname (merge-pathnames
                                                  file-pathname
                                                  (lodds.config:get-value :upload-folder)))))
         (case action
           (:accept (progn
                      (task-run task)
                      (lodds.event:push-event
                       :send-permission
                       (format nil "accepted (user ~{~a~^and~} trusted)"
                               users))))
           (:ask (lodds.event:push-event :send-permission
                                         (task-id task)
                                         file-pathname
                                         timeout
                                         users
                                         size
                                         ;; deny
                                         (lambda ()
                                           (task-cleanup task "Denied"))
                                         ;; accept
                                         (lambda (&optional new-file-pathname)
                                           (when new-file-pathname
                                             (setf (slot-value task 'file-pathname)
                                                   new-file-pathname))
                                           (task-run task))))))))))

;; task-load methods

(defgeneric task-load (task)
  (:documentation "Method returning the load the given task
  produces"))

(defmethod task-load ((task task))
  (with-slots (total-load bytes-transfered) task
    (- total-load bytes-transfered)))

;; task-cleanup methods

(defgeneric task-cleanup (task err)
  (:documentation "Closes open socket and file streams. Will be called
  once a task finishes."))

(defmethod task-cleanup ((task task) err)
  (with-slots (socket file-stream
               id tasks canceled
               on-cancel on-finish on-error)
      task
    (remhash id (slot-value tasks 'tasks))
    (secure-close socket)
    (setf socket nil)
    (when file-stream
      (close file-stream)
      (setf file-stream nil))

    ;; check if the error was raised due to a task-cancel
    ;; (canceled would be set to t then)
    (cond
      (err
       (when on-error
         (funcall on-error))
       (lodds.event:push-event :task-failed
                               task err))
      (canceled
       (when on-cancel
         (funcall on-cancel))
       (lodds.event:push-event :task-canceled
                               task))
      (t
       (when on-finish
         (funcall on-finish))
       (lodds.event:push-event :task-finished
                               task)))))

(defmethod task-cleanup ((task task-get-file) err)
  (with-slots (file-pathname canceled) task
    (when (or canceled err)
      (uiop:delete-file-if-exists file-pathname))
    (call-next-method)))

;; task-run methods to run a task

(defgeneric task-run (task)
  (:documentation "Will run the task until it finishes, fails or gets
  canceled. When socket was initialized with in-thread t (default),
  then task-run will open a new thread and return, if not, it will
  block until the task has finished, was canceled or failed"))

(defmethod task-run :around ((task task))
  ;; run the task when state is normal
  (with-slots (in-thread thread) task
    (lodds.core:with-server lodds:*server*
      (labels ((run ()
                 (handler-case
                     (progn
                       (call-next-method)
                       (task-cleanup task nil))
                   (error (err)
                     (task-cleanup task err)))))
        (if in-thread
            (setf thread
                  (bt:make-thread #'run
                                  :name (format nil "~a" task)))
            (run))))))

(defmethod task-run ((task task-request))
  (with-slots (socket tasks on-finish) task
    (multiple-value-bind (err request)
        (lodds.low-level-api:parse-request
         socket)
      (when (> err 0)
        (error (format nil "low level api returned ~a on parse-request"
                       err)))
      (ecase (car request)
        ((:file :info)
         (let ((sock socket)) ;; capture socket
           (setf on-finish
                 (lambda ()
                   (task-run
                    (ecase (car request)
                      (:file
                       (destructuring-bind (checksum start end) (cdr request)
                         (make-instance 'task-request-file
                                        :user (or (lodds:get-user-by-ip (socket-info sock)) "unknown@0.0.0.0:1234")
                                        :in-thread nil
                                        :tasks tasks
                                        :socket sock
                                        :checksum checksum
                                        :start start
                                        :end end)))
                      (:info
                       (make-instance 'task-request-info
                                      :in-thread nil
                                      :tasks tasks
                                      :socket sock
                                      :timestamp (second request)))))))))
        (:send-permission (apply 'handle-send-permission-request
                                 tasks
                                 socket
                                 (cdr request))))
      ;; make sure socket does not get cleared up
      (setf socket nil))))

(defmethod task-run ((task task-request-info))
  (with-slots (socket timestamp) task
    (apply #'lodds.low-level-api:respond-info
           (usocket:socket-stream socket)
           (lodds:generate-info-response timestamp))))

(defmethod task-run ((task task-request-send-permission))
  (with-slots (file-stream socket file-pathname) task
    (setf file-stream (open-file file-pathname :output))
    (lodds.low-level-api:respond-send-permission
     (usocket:socket-stream socket))
    (task-transfer task :socket-to-file)))

(defmethod task-run ((task task-request-file))
  (with-slots (checksum start end file-pathname file-stream total-load
               bytes-transfered)
      task
    ;; set file-pathname
    (let ((name (lodds.watcher:get-file-info checksum)))
      (unless name
        (error "Requested file not found"))
      (setf file-pathname name))
    ;; open local file stream
    (setf file-stream (open-file file-pathname :input))
    (let ((size (- end start))
          (file-size (file-length file-stream)))
      ;; do some checks if request would error
      (cond
        ((< size 0) (error "Requested size < 0"))
        ((> size file-size) (error "Requested size > file-length"))
        ((> end file-size) (error "Requested end > file-length")))
      ;; setup load and max-load
      (setf total-load size)
      ;; move to right file position
      (unless (eql start 0)
        (file-position file-stream start)))
    (task-transfer task :file-to-socket)))

(defmethod task-run ((task task-get-info))
  (with-slots (socket user)
      task
    (lodds.core:split-user-identifier (name ip port t) user
      (setf socket (connect ip port)))
    (let* ((user-info (lodds:get-user-info user))
           (lock (lodds:c-lock user-info)))
      (if (bt:acquire-lock lock nil)
          ;; only go on if we locked, if not, just drop the update, we
          ;; will update on the next advertise. unwind-protect to be sure
          ;; we unlock that lock.
          (unwind-protect
               (progn
                 (lodds.low-level-api:get-info (usocket:socket-stream socket)
                                               (lodds:c-last-change user-info))
                 (multiple-value-bind (err update)
                     (lodds.low-level-api:handle-info socket)
                   (if (eql 0 err)
                       (lodds:update-user user
                                          update)
                       (error "low level api returned: ~a" err))))
            (bt:release-lock lock))
          (lodds.event:push-event :info :dropped task)))))

(defmethod task-run ((task task-send-file))
  (with-slots (socket file-stream
               user timeout file-pathname bytes-transfered
               total-load canceled time-waited)
      task
    (setf file-stream (open-file file-pathname :input))
    (setf total-load
          (file-length file-stream))
    (lodds.core:split-user-identifier (name ip port t) user
      (setf socket (connect ip port)))
    (lodds.low-level-api:get-send-permission
     (usocket:socket-stream socket)
     total-load
     timeout
     (file-namestring file-pathname))
    (let ((done nil))
      (loop :while (and (not done)
                        (not canceled))
            :if (>= time-waited timeout)
            :do (error "Timeout. No Response from user, aborting Send File.")
            :else
            :do
            (case (lodds.low-level-api:handle-send-permission socket 1)
              (0 (progn
                   (setf time-waited nil)
                   (task-transfer task :file-to-socket)
                   (setf done t)))
              (3 (incf time-waited))
              (t (error "handle-send-permission returned error")))))))

(defmethod task-run ((task task-get-file-from-user))
  (with-slots (socket file-stream
               file-pathname bytes-transfered total-load
               checksum user)
      task
    (setf total-load (or (car (lodds:get-file-info checksum user))
                         (error "File with given checksum not found")))
    (setf file-stream (open-file file-pathname :output))
    (lodds.core:split-user-identifier (name ip port t) user
      (setf socket (connect ip port)))
    (lodds.low-level-api:get-file (usocket:socket-stream socket)
                                  checksum
                                  0
                                  total-load)
    (task-transfer task :socket-to-file)))

(defmethod task-run ((task task-get-file-from-users))
  (with-slots (socket file-stream
               bytes-transfered total-load
               checksum digester current-part part-size
               file-pathname current-user)
      task
    (let ((size (third (first (lodds:get-file-info checksum)))))
      (unless size
        (error "File with given checksum not found"))
      (setf file-stream (open-file file-pathname :output))
      (setf total-load size)
      (setf part-size
            (let ((64MiB (ash 1 26))
                  (32MiB (ash 1 25)))
              (if (> size 64MiB)
                  (ash size -4) ;; divide by 16 (split up into 16 parts)
                  (if (> size 32MiB)
                      (ash size -3) ;; divide by 8 (split up into 8 parts)
                      ;; if file is smaller than 32 mb just
                      ;; download it in one go
                      size)))))
    (loop
      :do (let ((end-next-part (* (+ 1 current-part)
                                  part-size)))
            (unless (> total-load end-next-part)
              (setf part-size
                    (- total-load bytes-transfered))
              (setf end-next-part total-load))
            (if (not (setf current-user (lodds:find-best-user checksum)))
                (error "Could not find user who shares the given file")
                (lodds.core:split-user-identifier (name ip port t) current-user
                  (setf socket (connect ip port))))
            (lodds.low-level-api:get-file (usocket:socket-stream socket)
                                          checksum
                                          bytes-transfered
                                          end-next-part)
            (task-transfer task :socket-to-file end-next-part digester)
            (secure-close socket)
            (incf current-part)
            (unless (eql bytes-transfered end-next-part)
              (error "task-transfer did not transfer all bytes"))
            (when (> bytes-transfered total-load)
              (error "Read to much")))
      :while (not (eql total-load bytes-transfered))
      :finally (when (and digester
                          (not (string= checksum
                                        (ironclad:byte-array-to-hex-string
                                         (ironclad:produce-digest digester)))))
                 (delete-file (lodds.core:escape-wildcards file-pathname))
                 (error  "Checksum validation Failed")))))

(defmethod task-run ((task task-get-folder))
  (with-slots (local-path remote-path
               items items-done user state info id
               bytes-transfered tasks canceled
               current-task total-load)
      task
    (setf items (lodds:get-folder-info remote-path user))
    (setf total-load (reduce #'+ items :key #'third))
    (let* ((remote-pathname (uiop:parse-unix-namestring remote-path))
           (remote-directory-pathname
             (make-pathname :defaults remote-pathname
                            :directory (butlast (pathname-directory remote-pathname)))))
      (loop :while (and items (not canceled))
            :for (file checksum size) = (pop items)
            :do
            (progn
              (setf items-done
                    (append (list (list file checksum size))
                            items-done))
              ;; the file might have changed, if so we should be able to
              ;; get the checksum from remote-path again, if this also
              ;; fails checksum will just be nil, then the
              ;; task-get-file-from-users will fail and push a
              ;; folder-download-error, exactly what we want
              (unless checksum
                (setf checksum (lodds:get-checksum-from-path file user)))
              ;; remove size from load since the new task will add it again
              (incf bytes-transfered size)
              (let* ((file-pathname (enough-namestring
                                     (uiop:parse-unix-namestring
                                      (cl-fs-watcher:escape-wildcards file))
                                     remote-directory-pathname))
                     (local-pathname (merge-pathnames file-pathname
                                                      local-path)))
                (ensure-directories-exist local-pathname)
                (task-run
                 (setf current-task
                       (make-instance
                        'task-get-file-from-users
                        :tasks tasks
                        :checksum checksum
                        :file-pathname local-pathname
                        :in-thread nil
                        :on-error
                        (lambda ()
                          ;; if there is no callback, just 'go on' skip
                          ;; the error, finish the
                          ;; task-get-file-from-users task and return to
                          ;; task-get-folders run method. But if there is
                          ;; a callback, set up a condition, push a event
                          ;; and wait on the condition
                          (when (lodds.event:callback-exists-p :folder-download-error)
                            (let ((condition (bt:make-condition-variable :name "Folder error condition"))
                                  (lock (bt:make-lock "Folder error condition lock")))
                              (lodds.event:push-event
                               :folder-download-error
                               (task-id task)
                               remote-path
                               file
                               ;; retry
                               (lambda ()
                                 (destructuring-bind (file checksum size)
                                     (pop items-done)
                                   ;; add item back onto list
                                   (setf items
                                         (append (list (list file checksum size))
                                                 items))
                                   (decf bytes-transfered size)
                                   (bt:with-lock-held (lock)
                                     (bt:condition-notify condition))))
                               ;; skip
                               (lambda ()
                                 (bt:with-lock-held (lock)
                                   (bt:condition-notify condition)))
                               ;; cancel
                               (lambda ()
                                 (setf canceled t)
                                 (bt:with-lock-held (lock)
                                   (bt:condition-notify condition))))
                              (bt:with-lock-held (lock)
                                (bt:condition-wait condition lock))))))))
                (setf current-task nil)))
            :finally
            (unless canceled
              (lodds.event:push-event :info
                                      (format nil "folder ~a sucessfully downloaded to ~a"
                                              remote-path
                                              local-path)))))))
