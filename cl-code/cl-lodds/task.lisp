(in-package #:lodds.task)

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
              size
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

(defgeneric run-task (task)
  (:documentation "Generic Function which will be called if task was
  added to thread-pool"))

(defmethod run-task ((tsk task))
  (declare (ignorable tsk))
  (error "overwrite run-task with ur task!"))

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
    (let ((id (copy-seq (string (gensym "task-id")))))
      (loop :while (gethash id tasks-on-hold)
            :do (setf id task))
      (setf (gethash id tasks-on-hold) task)
      id)))

(defun submit-task-from-hold (task-id)
  "Removes task specified by the given task-id from hold and submits
  it to the tasker. If the given task was found and submittet t is
  returned, otherwise nil"
  (with-slots (tasks-on-hold) (lodds:get-subsystem :tasker)
    (let ((task (gethash task-id tasks-on-hold)))
      (when task
        (remhash task-id tasks-on-hold)
        (submit-task task)
        t))))

(defun remove-task-from-hold (task-id)
  "Removes task specified by the given task-id from hold and returns
  it, if the task is not found nil is returned"
  (with-slots (tasks-on-hold) (lodds:get-subsystem :tasker)
    (let ((task (gethash task-id tasks-on-hold)))
      (when task
        (remhash task-id tasks-on-hold)
        task))))

(defmethod lodds.subsystem:start ((subsys tasker))
  (with-accessors ((kernel kernel)
                   (channel channel)
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
  (with-accessors ((kernel kernel)
                   (alive-p lodds.subsystem:alive-p)) subsys
    (when alive-p
      (let ((lparallel:*kernel* kernel))
        (lparallel:end-kernel :wait t)
        (setf alive-p nil)
        (lodds.event:push-event (lodds.subsystem:name subsys)
                                (list "stopped!"))))))

(defmethod initialize-instance :after ((task task-get-file-from-user) &rest initargs)
  (with-slots (ip
               port
               size) task
    (let ((user (getf initargs :user))
          (checksum (getf initargs :checksum)))
      (lodds.core:split-user-identifier (u-name u-ip u-port) user
        (setf ip u-ip
              port (parse-integer u-port)))
      ;; if size was not given, just download the whole file
      (lodds:update-load
       (setf size
             (car (lodds:get-file-info checksum user)))))))

(defmethod initialize-instance :after ((task task-get-file-from-users) &rest initargs)
  (let ((n-size (third (first (lodds:get-file-info (getf initargs :checksum))))))
    (lodds:update-load n-size)
    (with-slots (size part-size) task
      (setf size n-size)
      (setf part-size
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
  (let* ((user (getf initargs :user))
         (remote-path (getf initargs :remote-path))
         (items (lodds:get-folder-info remote-path user)))
    (lodds:update-load (reduce #'+ items :key #'third))
    (setf (slot-value task 'items)
          items)))

(defun load-chunk (stream-from stream-to size &optional (chunk-size (ash 1 21)))
  (let ((transfered (if (> size chunk-size)
                        chunk-size
                        size)))
    (lodds.core:copy-stream stream-from
                            stream-to
                            transfered)
    (lodds:update-load (- transfered))
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
               on-finish-hook) task
    (labels ((cleanup (&optional error-occured error)
               (if error-occured
                   (progn
                     (lodds.event:push-event :error
                                             (list "on get file from user"
                                                   local-file-path ":" error))
                     (lodds:update-load (- read-bytes size)))
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
                (submit-task task)))
        (error (e)
          (cleanup t e))))))

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
               on-finish-hook) task
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
                  (submit-task task))))
        (error (e)
          (cleanup t t)
          (lodds:update-load (- read-bytes size))
          (lodds.event:push-event :error
                                  (list "on get file from users" local-file-path ":" e)))))))

(defmethod run-task ((task task-get-folder))
  (with-slots (local-path
               remote-path
               remote-root
               items
               user
               on-finish-hook) task
    (if items
        (destructuring-bind (file checksum size) (pop items)
          (unless checksum
            ;; the file might have changed, if so we should be able to
            ;; get the checksum from remote-path again, if this also
            ;; fails checksum will just be nil, so the following if
            ;; will catch that.
            (setf checksum (lodds:get-checksum-from-path remote-path user)))
          ;; remove size from load since the new task will add it again
          (lodds:update-load (- size))
          (if checksum
              (submit-task
               (make-instance 'task-get-file-from-users
                              :name "get-file"
                              :checksum checksum
                              :local-file-path (ensure-directories-exist
                                                (concatenate 'string
                                                             local-path
                                                             (subseq file (length remote-root))))
                              ;; resubmit current task-get-folder when file
                              ;; download is complete
                              :on-finish-hook (lambda ()
                                                (submit-task task))))
              (error "The file ~a from user ~a with checksum ~a does not exist anymore."
                     file user checksum)))
        (progn
          (when on-finish-hook
            (funcall on-finish-hook))
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
  (with-slots (socket) task
    (with-socket socket nil
      (multiple-value-bind (error request)
          (lodds.low-level-api:parse-request (usocket:socket-stream socket))
        (if (> error 0)
            (error "low level api Returned error ~a~%" error)
            (let ((task (case (car request)
                          (:file
                           (destructuring-bind (checksum start end) (cdr request)
                             (make-instance 'task-request-file
                                            :name "request-file"
                                            :socket socket
                                            :checksum checksum
                                            :start start
                                            :end end)))
                          (:info
                           (make-instance 'task-request-info
                                          :name "request-info"
                                          :socket socket
                                          :timestamp (cadr request)))
                          (:send-permission
                           (progn
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
                                                                   :filename filename)))))
                                 (progn
                                   (lodds.event:push-event :send-permission
                                                           (list "received and denied (no callback added)"))
                                   (usocket:socket-close socket)))
                             nil)))))
              (when task
                (submit-task task))))))))

(defmethod run-task ((task task-request-file))
  (with-slots (socket
               checksum
               start
               end
               written
               filename
               file-stream) task
    (labels ((cleanup (&optional error-occured)
               (unless error-occured
                 (lodds.event:push-event :info (list "uploaded file"
                                                     filename)))
               (when socket
                 (usocket:socket-close socket))
               (when file-stream
                 (close file-stream))))
      (handler-case
          ;; TODO: chunk-size to settings
          (let ((chunk-size (ash 1 21))) ;; 2 MB
            (unless filename
              (unless (setf filename (lodds.watcher:get-file-info checksum))
                (lodds.event:push-event :error
                                        (list "requested file could not be found"))
                (cleanup t)
                (return-from run-task))
              (setf file-stream (open filename
                                      :direction :input
                                      :element-type '(unsigned-byte 8)))
              (lodds:update-load (- end start))
              (unless (eql start 0)
                (file-position file-stream start)))
            (let ((left-to-upload (- (- end start)
                                     written)))
              (lodds.core:copy-stream file-stream
                                      (usocket:socket-stream socket)
                                      (if (> left-to-upload chunk-size)
                                          (progn (incf written chunk-size)
                                                 (lodds:update-load (- chunk-size))
                                                 chunk-size)
                                          (progn (incf written left-to-upload)
                                                 (lodds:update-load (- left-to-upload))
                                                 left-to-upload)))
              (finish-output file-stream)
              (if (eql (- end start) written)
                  (cleanup)
                  (submit-task task))))
        (error (e)
          (cleanup t)
          (lodds:update-load (- written (- end start)))
          (lodds.event:push-event :error
                                  (list "on request file" filename ":" e)))))))

(defmethod run-task ((task task-request-info))
  (with-slots (socket
               timestamp) task
    (with-socket socket t
      (apply #'lodds.low-level-api:respond-info
             (usocket:socket-stream socket)
             (lodds:generate-info-response timestamp)))))

(defmethod run-task ((task task-request-send-permission))
  (with-slots (socket
               size
               filename
               file-stream
               read-bytes) task
    (labels ((cleanup (&optional error-occured)
               (unless error-occured
                 (lodds.event:push-event :info (list "receiving "
                                                     filename
                                                     "through send-permission completed")))
               (when socket
                 (usocket:socket-close socket))
               (when file-stream
                 (close file-stream))))
      (handler-case
          ;; TODO: chunk-size to settings
          (let ((chunk-size (ash 1 21))) ;; 2 MB
          (lodds.event:push-event :debug (list "here bitch"))
            ;; open up file-stream if not open yet
            (unless file-stream
              (setf file-stream (open filename
                                      :direction :output
                                      :if-exists :supersede
                                      :element-type '(unsigned-byte 8)))
              (unless (eql 0 (lodds.low-level-api:respond-send-permission
                              (usocket:socket-stream socket)))
                (cleanup t))
              (lodds:update-load size))
            ;; transfer the file
            (let ((left-to-receive (- size read-bytes)))
              (lodds.core:copy-stream (usocket:socket-stream socket)
                                      file-stream
                                      (if (> left-to-receive chunk-size)
                                          (progn (incf read-bytes chunk-size)
                                                 (lodds:update-load (- chunk-size))
                                                 chunk-size)
                                          (progn (incf read-bytes left-to-receive)
                                                 (lodds:update-load (- left-to-receive))
                                                 left-to-receive)))
              (if (eql size read-bytes)
                  (cleanup)
                  (submit-task task))))
        (error (e)
          (cleanup t)
          (lodds:update-load (- (- size read-bytes)))
          (lodds.event:push-event :error
                                  (list "on request send-permission" filename":" e)))))))

(defmethod run-task ((task task-info))
  (with-slots (name
               user
               ip
               port
               timestamp
               last-change
               load) task
    (let ((client-info (lodds:get-user-info user)))
      (if client-info
          (with-accessors ((old-load lodds:c-load)
                           (old-last-change lodds:c-last-change))
              (lodds:get-user-info user)
            (when (or (not (eql old-load load))
                      (<= old-last-change last-change))
              (lodds.event:push-event :client-updated
                                      (list user
                                            load
                                            last-change))))
          (progn
            (setf client-info
                  (make-instance 'lodds:client-info
                                 :c-name user
                                 :c-last-message timestamp
                                 :c-ip ip
                                 :c-port port
                                 :c-last-change 0
                                 :c-load load))
            (setf (gethash user (lodds:clients lodds:*server*))
                  client-info)
            (lodds.event:push-event :client-added
                                    (list user
                                          load
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
                             (lodds:c-load client-info) load)
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
               written) task
    (labels ((cleanup (&optional error-occured)
               (unless error-occured
                 (lodds.event:push-event :info (list "send file complete"
                                                     filepath)))
               (when socket
                 (usocket:socket-close socket))
               (when file-stream
                 (close file-stream))))
      (handler-case
          ;; TODO: chunk-size to settings
          (let ((chunk-size (ash 1 21))) ;; 2 MB
            ;; open up file-stream if not open yet
            (unless file-stream
              (setf file-stream (open filepath
                                      :element-type '(unsigned-byte 8)))
              (setf size (file-length file-stream)))
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
                  (error "handle-send-permission returned: ~a" ret))
                (lodds:update-load size)))
            ;; transfer the file
            (let ((left-to-send (- size written)))
              (lodds.core:copy-stream file-stream
                                      (usocket:socket-stream socket)
                                      (if (> left-to-send chunk-size)
                                          (progn (incf written chunk-size)
                                                 (lodds:update-load (- chunk-size))
                                                 chunk-size)
                                          (progn (incf written left-to-send)
                                                 (lodds:update-load (- left-to-send))
                                                 left-to-send)))
              (finish-output file-stream)
              (if (eql size written)
                  (cleanup)
                  (submit-task task))))
        (error (e)
          (cleanup t)
          (lodds:update-load (- (- size written)))
          (lodds.event:push-event :error
                                  (list "on send file" filepath ":" e)))))))
