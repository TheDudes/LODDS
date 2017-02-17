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

(defmethod initialize-instance :after ((task task) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (tasks lock) (lodds:get-subsystem :tasker)
    (with-slots (id) task
      (bt:with-lock-held (lock)
        (setf id (copy-seq (string (gensym "id"))))
        (loop :while (gethash id tasks)
              :do (setf id (copy-seq (string (gensym "id")))))
        (setf (gethash id tasks) task)
        id))))

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a"
            (slot-value object 'name))))

(defmethod print-object ((object task-user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name
                 user) object
      (format stream "~a :user ~a"
              name
              user))))

(defmethod print-object ((object task-request-file) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name
                 checksum
                 start
                 end) object
      (format stream "~a :checksum ~a :start ~a :end ~a"
              name
              (concatenate 'string (subseq checksum 0 7) "...")
              start
              end))))

(defmethod print-object ((object task-request-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name
                 timestamp) object
      (format stream "~a :timestamp ~a"
              name
              timestamp))))

(defmethod print-object ((object task-request-send-permission) stream)
  (print-unreadable-object (object stream :type t :identity t)
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
  (print-unreadable-object (object stream :type t :identity t)
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
  (print-unreadable-object (object stream :type t :identity t)
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
  (print-unreadable-object (object stream :type t :identity t)
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
  (print-unreadable-object (object stream :type t :identity t)
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
                 resubmit-p) task
      (setf aktive-p t)
      (handler-case
          (progn
            (call-next-method)
            (if resubmit-p
                (submit-task task)
                (when finished-p
                  (finish-task task)
                  (lodds.event:push-event :tasker
                                          (list "Task Finished:"
                                                task)))))
        (error (err)
          (lodds.event:push-event :tasker
                                  (list "Task Failed"
                                        task
                                        err))))
      (setf aktive-p nil)))

  (:method ((task task))
    (declare (ignorable task))
    (error "overwrite run-task with ur task!")))

(defgeneric finish-task (task)
  (:documentation "Generic Function which will be called if task has
  finished and will be deleted")

  (:method ((task task))
    nil)

  (:method :before ((task task))
    (with-slots (id on-finish-hook) task
      (with-slots (lock tasks) (lodds:get-subsystem :tasker)
        (bt:with-recursive-lock-held (lock)
          (remhash id tasks))
        (when on-finish-hook
          (funcall on-finish-hook task)))))

  (:method ((task task-get-file-from-user))
    (with-slots (socket local-file-stream) task
      (when socket
        (usocket:socket-close socket))
      (when local-file-stream
        (close local-file-stream))))

  (:method ((task task-get-file-from-users))
    (with-slots (socket local-file-stream) task
      (when socket
        (usocket:socket-close socket))
      (when local-file-stream
        (close local-file-stream))))

  (:method ((task task-request))
    (with-slots (socket) task
      (when socket (usocket:socket-close socket))))

  (:method ((task task-request-file))
    (with-slots (socket file-stream) task
      (when socket
        (usocket:socket-close socket))
      (when file-stream
        (close file-stream))))

  (:method ((task task-request-info))
    (with-slots (socket) task
      (when socket
        (usocket:socket-close socket))))

  (:method ((task task-request-send-permission))
    (with-slots (socket file-stream) task
      (when socket
        (usocket:socket-close socket))
      (when file-stream
        (close file-stream))))

  (:method ((task task-send-file))
    (with-slots (socket file-stream) task
      (when socket
        (usocket:socket-close socket))
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
      (let ((lparallel:*kernel* kernel))
        (lparallel:end-kernel :wait t)
        (setf tasks (make-hash-table :test #'equalp)
              tasks-on-hold (make-hash-table :test #'equalp)
              alive-p nil)
        (lodds.event:push-event (lodds.subsystem:name subsys)
                                (list "stopped!"))))))

(defmethod initialize-instance :after ((task task-get-file-from-user) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (ip port size load user checksum) task
    (lodds.core:split-user-identifier (u-name u-ip u-port) user
      (setf ip u-ip
            port (parse-integer u-port)))
    ;; if size was not given, just download the whole file
    (let ((new-size (car (lodds:get-file-info checksum user))))
      (setf load new-size
            size new-size))))

(defmethod initialize-instance :after ((task task-get-file-from-users) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (size part-size load checksum) task
    (let ((n-size (third (first (lodds:get-file-info checksum)))))
      (setf size n-size
            load n-size
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
  (with-slots (user remote-path items load) task
    (setf items (lodds:get-folder-info remote-path user))
    (setf load (reduce #'+ items :key #'third))))

(defun load-chunk (stream-from stream-to size &optional (chunk-size (ash 1 21)))
  (let ((transfered (if (> size chunk-size)
                        chunk-size
                        size)))
    (lodds.core:copy-stream stream-from
                            stream-to
                            transfered)
    transfered))

(defun open-file (file-path)
  (open file-path :direction :output
                  :if-does-not-exist :create
                  :element-type '(unsigned-byte 8)
                  ;; TODO: here we could ask the user, overwrite?
                  :if-exists :supersede))

(defun request-file (ip port checksum start end)
  (let ((socket (usocket:socket-connect ip port
                                        :element-type '(unsigned-byte 8))))
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
               finished-p) task
    (unless socket
      (setf socket (request-file ip port checksum 0 size)))
    (unless local-file-stream
      (setf local-file-stream (open-file local-file-path)))
    (let ((transfered (load-chunk (usocket:socket-stream socket)
                                  local-file-stream
                                  (- size read-bytes))))
      (decf-load task transfered)
      (incf read-bytes
            transfered))
    (finish-output local-file-stream)
    (if (eql size read-bytes)
        (progn
          (setf resubmit-p nil
                finished-p t)
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
        (values ip port))
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
               finished-p) task
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
      (decf-load task written)
      (incf read-bytes written)
      (incf read-bytes-part written))
    (finish-output local-file-stream)
    (if (eql size read-bytes)
        (progn
          (lodds.event:push-event :info (list "downloaded file"
                                              local-file-path))
          (setf resubmit-p nil
                finished-p t))
        (progn
          (setf  resubmit-p t)
          (when (eql read-bytes-part part-size)
            (incf current-part)
            (setf read-bytes-part 0)
            (setf local-file-stream nil)
            (setf resubmit-p t))))))

(defmethod run-task ((task task-get-folder))
  (with-slots (local-path
               remote-path
               remote-root
               items
               items-done
               user
               finished-p
               resubmit-p) task
    (setf resubmit-p nil)
    (if items
        (destructuring-bind (file checksum size) (pop items)
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
          (if checksum
              (let ((task (make-instance 'task-get-file-from-users
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
                                                           (submit-task task)))))
                (submit-task task))
              (error "The file ~a from user ~a with checksum ~a does not exist anymore."
                     file user checksum)))
        (progn
          (setf finished-p t
                resubmit-p nil)
          (lodds.event:push-event :info (list "folder"
                                              remote-path
                                              "sucessfully downloaded to"
                                              local-path))))))

(defmacro with-socket (socket close-socket-p &body body)
  `(let ((is-closed nil))
     (labels ((cleanup-socket ()
                (when ,socket
                  (handler-case
                      (close (usocket:socket-stream ,socket))
                    (error (e)
                      (lodds.event:push-event :error
                                              (list "could not close socket stream" e))))
                  (handler-case
                      (usocket:socket-close ,socket)
                    (error (e)
                      (lodds.event:push-event :error
                                              (list "could not close socket" e))))
                  (setf is-closed t))))
       (handler-case
           (progn
             (unless ,socket
               (error "Could not access socket, nil."))
             ,@body)
         (end-of-file ()
           (lodds.event:push-event :error
                                   (list "end-of-file on tasker"))
           (cleanup-socket))
         (error (e)
           (lodds.event:push-event :error
                                   (list "unknown error on tasker:" e))
           (cleanup-socket)))
       ,(when close-socket-p
          `(unless is-closed
             (cleanup-socket))))))

(defmethod run-task ((task task-request))
  (with-slots (socket
               finished-p
               resubmit-p) task
    (setf resubmit-p nil
          finished-p t)
    (multiple-value-bind (error request)
        (lodds.low-level-api:parse-request (usocket:socket-stream socket))
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
               load) task
    ;; TODO: chunk-size to settings
    (let ((chunk-size (ash 1 21))) ;; 2 MB
      (unless filename
        (unless (setf filename (lodds.watcher:get-file-info checksum))
          (lodds.event:push-event :error
                                  (list "requested file could not be found"))
          (setf resubmit-p nil
                finished-p t)
          (return-from run-task))
        (setf file-stream (open filename
                                :direction :input
                                :element-type '(unsigned-byte 8)))
        (setf load (- end start))
        (unless (eql start 0)
          (file-position file-stream start)))
      (let ((left-to-upload (- (- end start)
                               written)))
        (lodds.core:copy-stream file-stream
                                (usocket:socket-stream socket)
                                (if (> left-to-upload chunk-size)
                                    (progn (incf written chunk-size)
                                           (decf-load task chunk-size)
                                           chunk-size)
                                    (progn (incf written left-to-upload)
                                           (decf-load task left-to-upload)
                                           left-to-upload)))
        (finish-output file-stream)
        (if (eql (- end start) written)
            (setf resubmit-p nil
                  finished-p t)
            (setf resubmit-p t))))))

(defmethod run-task ((task task-request-info))
  (with-slots (socket
               timestamp
               finished-p
               resubmit-p) task
    (setf resubmit-p nil
          finished-p t)
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
               load) task
    ;; TODO: chunk-size to settings
    (let ((chunk-size (ash 1 21))) ;; 2 MB
      ;; open up file-stream if not open yet
      (unless file-stream
        (setf file-stream (open filename
                                :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8)))
        (unless (eql 0 (lodds.low-level-api:respond-send-permission
                        (usocket:socket-stream socket)))
          (setf resubmit-p nil
                finished-p t))
        (setf load size))
      ;; transfer the file
      (let ((left-to-receive (- size read-bytes)))
        (lodds.core:copy-stream (usocket:socket-stream socket)
                                file-stream
                                (if (> left-to-receive chunk-size)
                                    (progn (incf read-bytes chunk-size)
                                           (decf-load task chunk-size)
                                           chunk-size)
                                    (progn (incf read-bytes left-to-receive)
                                           (decf-load task left-to-receive)
                                           left-to-receive)))
        (if (eql size read-bytes)
            (setf resubmit-p nil
                  finished-p t)
            (setf resubmit-p t))))))

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
    (setf resubmit-p nil
          finished-p t)
    (let ((client-info (lodds:get-user-info user)))
      (if client-info
          (with-accessors ((old-load lodds:c-load)
                           (old-last-change lodds:c-last-change))
              (lodds:get-user-info user)
            (when (or (not (eql old-load user-load))
                      (<= old-last-change last-change))
              (lodds.event:push-event :client-updated
                                      (list user
                                            user-load
                                            last-change))))
          (progn
            (setf client-info
                  (make-instance 'lodds:client-info
                                 :c-name user
                                 :c-last-message timestamp
                                 :c-ip ip
                                 :c-port port
                                 :c-last-change 0
                                 :c-load user-load))
            (setf (gethash user (lodds:clients lodds:*server*))
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
                 (handler-case
                     (progn
                       (setf (lodds:c-last-message client-info) timestamp
                             (lodds:c-load client-info) user-load)
                       (when (<= (lodds:c-last-change client-info)
                                 last-change)
                         (lodds.listener:update-client-list client-info)))
                   (error (e)
                     (lodds.event:push-event :error (list "error inside update-client-list"
                                                          e))))
              (bt:release-lock (lodds:c-lock client-info)))
            (lodds.event:push-event :info (list :dropped task)))))))

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
               load) task
    ;; TODO: chunk-size to settings
    (let ((chunk-size (ash 1 21))) ;; 2 MB
      ;; open up file-stream if not open yet
      (unless file-stream
        (setf file-stream (open filepath
                                :element-type '(unsigned-byte 8)))
        (setf size (file-length file-stream))
        (setf load size))
      ;; open up socket
      (unless socket
        (setf socket (usocket:socket-connect ip
                                             port
                                             :element-type '(unsigned-byte 8)))
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
            (error "get-send-permission returned: ~a" ret))
          ;; wait timeout seconds for a response from the client
          (setf ret
                (lodds.low-level-api:handle-send-permission socket
                                                            timeout))
          (unless (eql 0 ret)
            (error "handle-send-permission returned: ~a" ret))))
      ;; transfer the file
      (let ((left-to-send (- size written)))
        (lodds.core:copy-stream file-stream
                                (usocket:socket-stream socket)
                                (if (> left-to-send chunk-size)
                                    (progn (incf written chunk-size)
                                           (decf-load task chunk-size)
                                           chunk-size)
                                    (progn (incf written left-to-send)
                                           (decf-load task left-to-send)
                                           left-to-send)))
        (finish-output file-stream)
        (if (eql size written)
            (setf resubmit-p nil
                  finished-p t)
            (setf resubmit-p t))))))
