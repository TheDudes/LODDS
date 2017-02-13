(in-package #:lodds.task)

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a"
            (slot-value object 'name))))

(defmethod print-object ((object task-client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name
                 client-name) object
      (format stream "~a :client ~a"
              name
              client-name))))

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

(defmethod initialize-instance :after ((task lodds.task:task-get-file-from-user) &rest initargs)
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

(defmethod initialize-instance :after ((task lodds.task:task-get-file-from-users) &rest initargs)
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

(defmethod initialize-instance :after ((task lodds.task:task-get-folder) &rest initargs)
  (let* ((user (getf initargs :user))
         (remote-path (getf initargs :remote-path))
         (items (lodds:get-folder-info remote-path user)))
    (lodds:update-load (reduce #'+ items :key #'third))
    (setf (slot-value task 'folder-items)
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

(defmethod lodds.task:run-task ((task lodds.task:task-get-file-from-user))
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
                (lodds.task:submit-task task)))
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

(defmethod lodds.task:run-task ((task lodds.task:task-get-file-from-users))
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
                  (lodds.task:submit-task task))))
        (error (e)
          (cleanup t t)
          (lodds:update-load (- read-bytes size))
          (lodds.event:push-event :error
                                  (list "on get file from users" local-file-path ":" e)))))))

(defmethod lodds.task:run-task ((task lodds.task:task-get-folder))
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
                                   (list "end-of-file on handler"))
           (cleanup-socket))
         (error (e)
           (lodds.event:push-event :error
                                   (list "unknown error on handler:" e))
           (cleanup-socket)))
       ,(when close-socket-p
          `(unless is-closed
             (cleanup-socket))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request))
  (with-slots (socket) task
    (with-socket socket nil
      (multiple-value-bind (error request)
          (lodds.low-level-api:parse-request (usocket:socket-stream socket))
        (if (> error 0)
            (error "low level api Returned error ~a~%" error)
            (lodds.task:submit-task
             (case (car request)
               (:file
                (destructuring-bind (checksum start end) (cdr request)
                  (make-instance 'lodds.task:task-request-file
                                 :name "request-file"
                                 :socket socket
                                 :checksum checksum
                                 :start start
                                 :end end)))
               (:info
                (make-instance 'lodds.task:task-request-info
                               :name "request-info"
                               :socket socket
                               :timestamp (cadr request)))
               (:send-permission
                (destructuring-bind (size timeout filename) (cdr request)
                  (make-instance 'lodds.task:task-request-send-permission
                                 :name "request-send-permission"
                                 :socket socket
                                 :size size
                                 :timeout timeout
                                 :filename filename))))))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-file))
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
          (let ((chunk-size (ash 1 21))) ;; 2 MB
            (unless filename
              (unless (setf filename (lodds.watcher:get-file-info checksum))
                (lodds.event:push-event :error
                                        (list "requested file could not be found"))
                (cleanup t)
                (return-from lodds.task:run-task))
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
                  (lodds.task:submit-task task))))
        (error (e)
          (cleanup t)
          (lodds:update-load (- written (- end start)))
          (lodds.event:push-event :error
                                  (list "on request file" filename ":" e)))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-info))
  (with-slots (socket
               timestamp) task
    (with-socket socket t
      (apply #'lodds.low-level-api:respond-info
             (usocket:socket-stream socket)
             (lodds:generate-info-response timestamp)))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-send-permission))
  (with-slots (socket
               size
               filename) task
    (with-socket socket t
      ;; TODO: ask user here for file path
      (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
        (lodds.low-level-api:respond-send-permission (usocket:socket-stream socket)
                                                     file-stream
                                                     size)))))

(defmethod lodds.task:run-task ((task lodds.task:task-client-info))
  (with-slots (name
               client-name
               ip
               port
               timestamp
               last-change
               load) task
    (let ((client-info (lodds:get-user-info client-name)))
      (if client-info
          (with-accessors ((old-load lodds:c-load)
                           (old-last-change lodds:c-last-change))
              (lodds:get-user-info client-name)
            (when (or (not (eql old-load load))
                      (<= old-last-change last-change))
              (lodds.event:push-event :client-updated
                                      (list client-name
                                            load
                                            last-change))))
          (progn
            (setf client-info
                  (make-instance 'lodds:client-info
                                 :c-name client-name
                                 :c-last-message timestamp
                                 :c-ip ip
                                 :c-port port
                                 :c-last-change 0
                                 :c-load load))
            (setf (gethash client-name (lodds:clients lodds:*server*))
                  client-info)
            (lodds.event:push-event :client-added
                                    (list client-name
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
