(in-package #:lodds.task)

(defmethod get-load ((tasker tasker))
  (with-slots (tasks tasks-on-hold lock) tasker
    (let ((loads nil))
      (bt:with-recursive-lock-held (lock)
        (setf loads
              (loop :for task :being :the :hash-value :of tasks
                    :unless (gethash (slot-value task 'id) tasks-on-hold)
                    :collect (slot-value task 'load))))
      (if loads
          (reduce #'+ loads)
          0))))

(defmethod get-task-progresses ((tasker tasker))
  "returns a list containing each tasks id, max-load, progress, type
  and info.
  CL-USER> (get-task-progresses tasker)
  => ((\"id1234\" 5903 1324 :send-file \"blub.txt\")
      (\"id3142\" 141204 4214 :get-file \"somename@123.123.1.2:8282\"))
  Each of the inner lists describe a task, with its id, its maximum
  load, already processed load, type and info."
  (with-slots (tasks tasks-on-hold lock) tasker
    (bt:with-recursive-lock-held (lock)
      (loop :for task :being :the :hash-value :of tasks
            :unless (gethash (slot-value task 'id) tasks-on-hold)
            :collect (with-slots (id max-load load info) task
                       (list id max-load (- max-load load) (type-of task) info))))))

(defmethod get-task-by-id (task-id)
  "Returns task with given id, nil if task is not found"
  (with-slots (tasks lock) (lodds:get-subsystem :tasker)
    (let ((result nil))
      (bt:with-recursive-lock-held (lock)
        (setf result
              (gethash task-id tasks)))
      result)))

(defgeneric cancel-task (task)
  (:method ((task task))
    (setf (slot-value task 'canceled-p) t))
  (:method ((task-id string))
    (let ((task (get-task-by-id task-id)))
      (when task
        (cancel-task task)))))

(defparameter *id-counter* 0)
(defmethod initialize-instance :after ((task task) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (tasks lock (alive-p lodds.subsystem:alive-p))
      (lodds:get-subsystem :tasker)
    (when alive-p
      (with-slots (id) task
        (bt:with-lock-held (lock)
          (setf id (format nil "id-~a" (incf *id-counter*)))
          (setf (gethash id tasks) task)
          id)))))

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a"
            (slot-value object 'name))))

(defmethod print-object ((object task-user) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 user) object
      (format stream "~a :user ~a"
              name
              user))))

(defmethod print-object ((object task-info) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 user) object
      (format stream "~a :user ~a"
              name
              user))))

(defmethod print-object ((object task-request-file) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 checksum
                 start
                 end) object
      (format stream "~a :checksum ~a :start ~:d :end ~:d"
              name
              (concatenate 'string (subseq checksum 0 7) "...")
              start
              end))))

(defmethod print-object ((object task-request-info) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 timestamp) object
      (format stream "~a :timestamp ~a"
              name
              timestamp))))

(defmethod print-object ((object task-request-send-permission) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 size
                 timeout
                 filename) object
      (format stream "~a :size ~a :timeout ~a :filename ~a"
              name
              (lodds.core:format-size size)
              timeout
              filename))))

(defmethod print-object ((object task-get-file-from-user) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 local-file-path
                 size
                 user
                 checksum) object
      (format stream "~a :local-file-path ~a size: ~a :user ~a :checksum ~a"
              name
              local-file-path
              (lodds.core:format-size size)
              user
              checksum))))

(defmethod print-object ((object task-get-file-from-users) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 local-file-path
                 part-size
                 size
                 checksum) object
      (format stream "~a :local-file-path ~a size: ~a :part-size ~a :checksum ~a"
              name
              local-file-path
              (lodds.core:format-size size)
              (lodds.core:format-size part-size)
              checksum))))

(defmethod print-object ((object task-send-file) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 ip
                 port
                 filepath
                 timeout
                 size
                 written) object
      (format stream "~a filepath: ~a address: ~a:~a timeout: ~a size: ~a written: ~a"
              name
              filepath
              ip
              port
              timeout
              (lodds.core:format-size size)
              (lodds.core:format-size written)))))

(defmethod print-object ((object task-get-folder) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name
                 items
                 items-done
                 remote-path) object
      (format stream "~a items: ~a items-done: ~a folder: ~a"
              name
              (length items)
              (length items-done)
              remote-path))))

(defmethod decf-load ((task task) amount)
  (decf (slot-value task 'load) amount))

(defgeneric run-task (task)
  (:documentation "Generic Function which will be called if task was
  added to thread-pool")

  (:method :around ((task task))
    (with-slots (aktive-p
                 finished-p
                 resubmit-p
                 canceled-p
                 id
                 on-finish-hook
                 on-cancel-hook
                 on-error-hook) task
      (when canceled-p
        (finish-task task)
        (when on-cancel-hook
          (funcall on-cancel-hook task))
        (lodds.event:push-event :task-canceled
                                (list id task))
        (return-from run-task))
      (setf aktive-p t)
      (handler-case
          (call-next-method)
        (error (err)
          (setf aktive-p nil
                finished-p t)
          (finish-task task)
          (when on-error-hook
            (funcall on-error-hook task))
          (lodds.event:push-event :task-failed
                                  (list id task err))
          (return-from run-task)))
      (setf aktive-p nil)
      (if finished-p
          (progn
            (finish-task task)
            (when on-finish-hook
              (funcall on-finish-hook task))
            (lodds.event:push-event :task-finished
                                    (list id task)))
          (when resubmit-p
            (submit-task task)))))

  (:method ((task task))
    (declare (ignorable task))
    (error "overwrite run-task with ur task!")))

(defun secure-close (socket)
  (when socket
    (handler-case
        (usocket:socket-close socket)
      (error (e)
        (declare (ignore e))))))

(defgeneric finish-task (task)
  (:documentation "Generic Function which will be called if task has
  finished and will be deleted")

  (:method ((task task))
    nil)

  (:method :before ((task task))
    (with-slots (lock tasks) (lodds:get-subsystem :tasker)
      (bt:with-recursive-lock-held (lock)
        (remhash (slot-value task 'id) tasks))))

  (:method ((task task-get-file-from-user))
    (with-slots (socket local-file-stream) task
      (secure-close socket)
      (when local-file-stream
        (close local-file-stream))))

  (:method ((task task-get-file-from-users))
    (with-slots (socket local-file-stream) task
      (secure-close socket)
      (when local-file-stream
        (close local-file-stream))))

  (:method ((task task-request))
    (with-slots (socket) task
      (secure-close socket)))

  (:method ((task task-request-file))
    (with-slots (socket file-stream) task
      (secure-close socket)
      (when file-stream
        (close file-stream))))

  (:method ((task task-request-info))
    (secure-close (slot-value task 'socket)))

  (:method ((task task-request-send-permission))
    (with-slots (socket file-stream) task
      (secure-close socket)
      (when file-stream
        (close file-stream))))

  (:method ((task task-send-file))
    (with-slots (socket file-stream) task
      (secure-close socket)
      (when file-stream
        (close file-stream)))))

(defun submit-task (task)
  "Adds a new task which will be added to the lparallel:channel."
  (let* ((tasker (lodds:get-subsystem :tasker))
         (lparallel:*kernel* (kernel tasker)))
    (when (lodds.subsystem:alive-p tasker)
      (lparallel:submit-task (channel tasker)
                             #'run-task
                             task))))

(defun put-task-on-hold (task)
  "puts the given task /on-hold/, which just saves it inside a
  hashtable and returns a /task-id/ (string) which can be used to
  submit the task with SUBMIT-TASK-FROM-HOLD or remove the task from
  the table with REMOVE-TASK-FROM-HOLD"
  (with-slots (tasks-on-hold) (lodds:get-subsystem :tasker)
    (with-slots (id) task
      (setf (gethash id tasks-on-hold) task)
      id)))

(defun submit-task-from-hold (task-id)
  "Removes task specified by the given task-id from hold and submits
  it to the tasker. If the given task was found and submittet t is
  returned, otherwise nil"
  (with-slots (tasks-on-hold lock) (lodds:get-subsystem :tasker)
    (bt:with-recursive-lock-held (lock)
      (let ((task (gethash task-id tasks-on-hold)))
        (when task
          (remhash task-id tasks-on-hold)
          (submit-task task)
          t)))))

(defun remove-task-from-hold (task-id)
  "Removes task specified by the given task-id from hold and returns
  it, if the task is not found nil is returned"
  (with-slots (tasks-on-hold lock) (lodds:get-subsystem :tasker)
    (bt:with-recursive-lock-held (lock)
      (let ((task (gethash task-id tasks-on-hold)))
        (when task
          (remhash task-id tasks-on-hold)
          task)))))

(defmethod lodds.subsystem:start ((subsys tasker))
  (with-slots (kernel
               channel
               (alive-p lodds.subsystem:alive-p)) subsys
    (if alive-p
        (lodds.event:push-event :tasker (list "already running!"))
        (let ((lparallel:*kernel* (lparallel:make-kernel 10
                                                         :bindings `((lodds:*server* . ,lodds:*server*)))))
          (setf kernel  lparallel:*kernel*
                channel (lparallel:make-channel)
                alive-p t)
          (lodds.event:push-event :tasker
                                  (list "started!"))))))

(defmethod lodds.subsystem:stop ((subsys tasker))
  (with-slots (kernel
               tasks
               tasks-on-hold
               (alive-p lodds.subsystem:alive-p)) subsys
    (when alive-p
      (bt:make-thread
       (lambda ()
         (let ((tasks-running t)
               (tries 0)
               (tries-max 8))
           (loop :while tasks-running
                 :do (let ((msg (format nil "shutting down remaining tasks... (try ~a/~a)"
                                        tries tries-max)))
                       (when (eql tries tries-max)
                         (format t "forcing tasker shutdown~%")
                         (lodds.event:push-event (lodds.subsystem:name subsys)
                                                 (list "forcing tasker shutdown"))
                         (return))
                       (format t "~a~%" msg)
                       (lodds.event:push-event (lodds.subsystem:name subsys)
                                               (list msg))
                       (setf tasks-running nil)
                       (maphash (lambda (task-id task)
                                  (unless (gethash task-id tasks-on-hold)
                                    (cancel-task task)
                                    (setf tasks-running t)))
                                tasks)
                       (incf tries)
                       (sleep 0.5))))
         (let ((lparallel:*kernel* kernel))
           (lparallel:end-kernel :wait t)
           (setf tasks (make-hash-table :test #'equal)
                 tasks-on-hold (make-hash-table :test #'equal)
                 alive-p nil)
           (lodds.event:push-event (lodds.subsystem:name subsys)
                                   (list "stopped!"))))
       :name "Tasker Shutdown"))))

(defmethod initialize-instance :after ((task task-get-file-from-user) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (ip port size load user checksum max-load) task
    (lodds.core:split-user-identifier (u-name u-ip u-port) user
      (setf ip u-ip
            port (parse-integer u-port)))
    ;; if size was not given, just download the whole file
    (let ((new-size (car (lodds:get-file-info checksum user))))
      (setf load new-size
            max-load new-size
            size new-size))))

(defmethod initialize-instance :after ((task task-get-file-from-users) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (size part-size load checksum max-load) task
    (let ((n-size (third (first (lodds:get-file-info checksum)))))
      (setf size n-size
            load n-size
            max-load n-size
            part-size
            (let ((64mb (ash 1 26))
                  (32mb (ash 1 25)))
              (if (> n-size 64mb)
                  (ash n-size -4) ;; / 16 (split up into 16 parts
                  (if (> n-size 32mb)
                      (ash n-size -3) ;; / 8 (split up into 8 parts
                      ;; if file is smaller than 32 mb just
                      ;; download it in one go
                      n-size)))))))

(defmethod initialize-instance :after ((task task-get-folder) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (user remote-path items load max-load) task
    (setf items (lodds:get-folder-info remote-path user))
    (let ((size (reduce #'+ items :key #'third)))
      (setf load size
            max-load size))))

(defmethod initialize-instance :after ((task task-request-send-permission) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (info filename id) task
    (setf info (format nil "[Request] (Incomming Request): ~a" filename))))

(defun load-chunk (stream-from stream-to size &optional check-input-fn (chunk-size (ash 1 21)))
  "Transfers bytes from STREAM-FROM to STREAM-TO. It will never
  transfer more then (max size chunk-size) bytes. Returns the Amount
  of transfered Bytes."
  (lodds.core:copy-stream stream-from
                          stream-to
                          (if (> size chunk-size)
                              chunk-size
                              size)
                          check-input-fn))

(defun open-file (file-path)
  (open file-path :direction :output
                  :if-does-not-exist :create
                  :element-type '(unsigned-byte 8)
                  ;; TODO: here we could ask the user, overwrite?
                  :if-exists :supersede))

(defun request-file (ip port checksum start end)
  (let ((socket (usocket:socket-connect ip port
                                        :element-type '(unsigned-byte 8)
                                        :timeout 1)))
    (lodds.low-level-api:get-file (usocket:socket-stream socket) checksum start end)
    socket))

(defmethod run-task ((task task-get-file-from-user))
  (with-slots (local-file-path
               user
               ip
               port
               checksum
               size
               read-bytes
               socket
               local-file-stream
               resubmit-p
               finished-p
               info) task
    (unless socket
      (setf socket (request-file ip port checksum 0 size)))
    (unless local-file-stream
      (setf local-file-stream (open-file local-file-path))
      (setf info (format nil "[Download] (~a):~a "
                         user
                         local-file-path)))
    (let ((transfered (load-chunk (usocket:socket-stream socket)
                                  local-file-stream
                                  (- size read-bytes))))
      (decf-load task transfered)
      (incf read-bytes transfered))
    (finish-output local-file-stream)
    (if (eql size read-bytes)
        (progn
          (setf finished-p t)
          (lodds.event:push-event :info (list "downloaded file"
                                              local-file-path)))
        (setf resubmit-p t))))

(defun find-best-user (checksum)
  (let ((best-user nil)
        (best-load nil))
    (loop :for (user load . rest) :in (lodds:get-file-info checksum)
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
        (values name ip port))
      (error "Could not find a user who shares ~a~%" checksum))))

(defmethod run-task ((task task-get-file-from-users))
  (with-slots (local-file-path
               checksum
               size
               read-bytes
               socket
               local-file-stream
               current-part
               read-bytes-part
               part-size
               resubmit-p
               finished-p
               info) task
    (unless local-file-stream
      (setf local-file-stream (open-file local-file-path)))
    (unless socket
      (multiple-value-bind (user ip port) (find-best-user checksum)
        (setf socket
              (request-file
               ip
               (parse-integer port)
               checksum
               (* current-part part-size)
               (let ((end-next-part (* (+ 1 current-part)
                                       part-size)))
                 (if (> size end-next-part)
                     end-next-part
                     (progn
                       (setf part-size (- size
                                          (* current-part part-size)))
                       size)))))
        (setf info (format nil "[Download] (~a): ~a" user local-file-path))))
    (let ((written (load-chunk (usocket:socket-stream socket)
                               local-file-stream
                               (- part-size read-bytes-part))))
      (decf-load task written)
      (incf read-bytes written)
      (incf read-bytes-part written))
    (finish-output local-file-stream)
    (if (eql size read-bytes)
        (setf finished-p t)
        (progn
          (when (eql read-bytes-part part-size)
            (incf current-part)
            (setf read-bytes-part 0)
            (secure-close socket)
            (setf socket nil))
          (setf resubmit-p t)))))

(defmethod run-task ((task task-get-folder))
  (with-slots (local-path
               remote-path
               remote-root
               items
               items-done
               user
               finished-p
               resubmit-p
               canceled-p
               info
               id) task
    (setf resubmit-p nil)
    (if (not items)
        (progn
          (setf finished-p t)
          (lodds.event:push-event :info (list "folder"
                                              remote-path
                                              "sucessfully downloaded to"
                                              local-path)))
        (destructuring-bind (file checksum size) (pop items)
          (let* ((left (length items))
                 (done (length items-done))
                 (total (+ left done)))
            (setf info (format nil "[Folder Download] (File ~a/~a):~a"
                               done
                               total
                               file)))
          (setf items-done (append (list (list file checksum size))
                                   items-done))
          (unless checksum
            ;; the file might have changed, if so we should be able to
            ;; get the checksum from remote-path again, if this also
            ;; fails checksum will just be nil, so the following if
            ;; will catch that.
            (setf checksum (lodds:get-checksum-from-path remote-path user)))
          ;; remove size from load since the new task will add it again
          (decf-load task size)
          (let ((on-error-hook
                  (lambda (file-task)
                    (declare (ignore file-task))
                    (if (lodds.event:callback-exists-p :folder-download-error)
                        (lodds.event:push-event :folder-download-error id)
                        ;; just skip the file
                        (submit-task task)))))
            (if checksum
                (submit-task (make-instance
                              'task-get-file-from-users
                              :name "get-file-from-users (folder)"
                              :checksum checksum
                              :local-file-path (ensure-directories-exist
                                                (concatenate 'string
                                                             local-path
                                                             (subseq file (length remote-root))))
                              ;; resubmit current task-get-folder when file
                              ;; download is complete
                              :on-finish-hook (lambda (file-task)
                                                (declare (ignore file-task))
                                                (submit-task task))
                              :on-error-hook on-error-hook
                              :on-cancel-hook on-error-hook))
                (funcall on-error-hook nil)))))))

(defmethod run-task ((task task-request))
  (with-slots (socket
               finished-p
               resubmit-p) task
    (setf finished-p t)
    (multiple-value-bind (error request)
        (lodds.low-level-api:parse-request socket)
      (if (> error 0)
          (error "low-level-api:parse-request Returned error ~a~%" error)
          (case (car request)
            (:file
             (destructuring-bind (checksum start end) (cdr request)
               (submit-task
                (make-instance 'task-request-file
                               :name "request-file"
                               :socket socket
                               :checksum checksum
                               :start start
                               :end end))
               (setf socket nil)))
            (:info
             (progn
               (submit-task
                (make-instance 'task-request-info
                               :name "request-info"
                               :socket socket
                               :timestamp (cadr request)))
               (setf socket nil)))
            (:send-permission
             (if (lodds.event:callback-exists-p :send-permission)
                 (destructuring-bind (size timeout filename)
                     (cdr request)
                   (lodds.event:push-event :send-permission
                                           (list (put-task-on-hold
                                                  (make-instance
                                                   'task-request-send-permission
                                                   :name "request-send-permission"
                                                   :socket socket
                                                   :size size
                                                   :timeout timeout
                                                   :filename filename))))
                   (setf socket nil))
                 (lodds.event:push-event :send-permission
                                         (list "received and denied (no callback added)")))))))))

(defmethod run-task ((task task-request-file))
  (with-slots (socket
               checksum
               start
               end
               written
               filename
               file-stream
               finished-p
               resubmit-p
               load
               max-load
               info) task
    (unless filename
      (unless (setf filename (lodds.watcher:get-file-info checksum))
        (lodds.event:push-event :error
                                (list "requested file could not be found"))
        (setf finished-p t)
        (return-from run-task))
      (setf file-stream (open filename
                              :direction :input
                              :element-type '(unsigned-byte 8)))
      (let ((size (- end start)))
        (setf load size
              max-load size)
        (unless (eql start 0)
          (file-position file-stream start))
        (setf info (format nil "[Upload] (~a):~a"
                           (lodds.core:format-size size)
                           filename))))
    (let ((transfered
            (load-chunk file-stream
                        (usocket:socket-stream socket)
                        (- (- end start)
                           written))))
      (incf written transfered)
      (decf-load task transfered))
    (finish-output file-stream)
    (if (eql (- end start) written)
        (setf finished-p t)
        (setf resubmit-p t))))

(defmethod run-task ((task task-request-info))
  (with-slots (socket
               timestamp
               finished-p
               resubmit-p) task
    (setf finished-p t)
    (apply #'lodds.low-level-api:respond-info
           (usocket:socket-stream socket)
           (lodds:generate-info-response timestamp))))

(defmethod run-task ((task task-request-send-permission))
  (with-slots (socket
               size
               filename
               file-stream
               read-bytes
               finished-p
               resubmit-p
               load
               max-load
               info) task
    ;; open up file-stream if not open yet
    (unless file-stream
      (setf file-stream (open filename
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8)))
      (unless (eql 0 (lodds.low-level-api:respond-send-permission
                      (usocket:socket-stream socket)))
        (setf finished-p t))
      (setf load size
            max-load size))
    ;; transfer the file
    (let ((transfered
            (load-chunk (usocket:socket-stream socket)
                        file-stream
                        (- size read-bytes)
                        (lambda ()
                          (lodds.core:input-rdy-p socket 1)))))
      (incf read-bytes transfered)
      (decf-load task transfered))
    (if (eql size read-bytes)
        (setf finished-p t)
        (setf resubmit-p t))))

(defmethod run-task ((task task-info))
  (with-slots (name
               user
               ip
               port
               timestamp
               user-load
               last-change
               load
               finished-p
               resubmit-p) task
    (setf finished-p t)
    (let ((client-info (lodds:get-user-info user))
          (event nil))
      (if client-info
          (with-accessors ((old-load lodds:c-load)
                           (old-last-change lodds:c-last-change))
              client-info
            (when (or (not (eql old-load user-load))
                      (<= old-last-change last-change))
              (setf event :client-updated)))
          (progn
            (setf client-info
                  (make-instance 'lodds:client-info
                                 :c-name user
                                 :c-last-message timestamp
                                 :c-ip ip
                                 :c-port port
                                 :c-last-change 0
                                 :c-load user-load)
                  (gethash user (lodds:clients lodds:*server*))
                  client-info)
            (lodds.event:push-event :client-added
                                    (list user
                                          user-load
                                          last-change))))
      (let ((locked (bt:acquire-lock (lodds:c-lock client-info) nil)))
        (if locked
            ;; only go on if we locked, if not, just drop the update, we
            ;;will update on the next advertise. unwind-protect to be sure
            ;;we unlock that lock.
            (unwind-protect
                 (progn
                   (setf (lodds:c-last-message client-info) timestamp
                         (lodds:c-load client-info) user-load)
                   (when (<= (lodds:c-last-change client-info)
                             last-change)
                     (lodds.listener:update-client-list client-info)))
              (bt:release-lock (lodds:c-lock client-info)))
            (lodds.event:push-event :info (list :dropped task))))
      (when event
        (lodds.event:push-event event
                                (list user
                                      user-load
                                      last-change))))))

(defmethod run-task ((task task-send-file))
  (with-slots (ip
               port
               filepath
               timeout
               socket
               file-stream
               size
               written
               finished-p
               resubmit-p
               load
               max-load
               info
               time-waited) task
    ;; open up file-stream if not open yet
    (unless file-stream
      (setf file-stream (open filepath
                              :element-type '(unsigned-byte 8)))
      (setf size (file-length file-stream)))
    ;; open up socket
    (unless socket
      (setf socket (usocket:socket-connect ip
                                           port
                                           :element-type '(unsigned-byte 8)
                                           :timeout 1))
      (lodds.event:push-event :debug
                              (list "Asking for send permission"))
      (let ((ret 0))
        ;; send a send-permission request
        (setf ret
              (lodds.low-level-api:get-send-permission (usocket:socket-stream socket)
                                                       size
                                                       timeout
                                                       (file-namestring filepath)))
        (unless (eql 0 ret)
          (error "get-send-permission returned: ~a" ret))))
    (setf resubmit-p t)
    (if (< time-waited timeout)
        ;; wait timeout seconds for a response from the client, if
        ;; after 1 second we got nothing, just resubmit the task and
        ;; try again later. if time is already equal to timeout, we
        ;; have successfull waited and can start transfer.
        (case (lodds.low-level-api:handle-send-permission socket 1)
          (0 (progn ;; success, set time to timeout
               (setf time-waited timeout)
               (setf load size
                     max-load size)
               (setf info (format nil "[Send File] (Sending):~a" filepath))))
          (3 (progn ;; on timeout wait another second
               (incf time-waited)
               (if (>= time-waited timeout)
                   (error "Timeout. No Response from user, aborting Send File.")
                   (setf info (format nil "[Send File] (Waiting for accept ~a/~a):~a"
                                      (lodds.core:format-seconds time-waited)
                                      (lodds.core:format-seconds timeout)
                                      filepath)))))
          (t (error "handle-send-permission returned error")))
        ;; transfer the file
        (let ((transfered
                (load-chunk file-stream
                            (usocket:socket-stream socket)
                            (- size written))))
          (incf written transfered)
          (decf-load task transfered)
          (finish-output file-stream)
          (if (eql size written)
              (setf finished-p t)
              (setf resubmit-p t))))))
