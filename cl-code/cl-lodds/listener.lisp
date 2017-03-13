(in-package lodds.listener)

(defun update-client-list (client)
  (let ((socket nil))
    (unwind-protect
         (with-accessors ((name lodds:c-name)
                          (load lodds:c-load)
                          (ip lodds:c-ip)
                          (port lodds:c-port)
                          (last-change lodds:c-last-change)
                          (table-hash lodds:c-file-table-hash)
                          (table-name lodds:c-file-table-name)) client
           (setf socket (usocket:socket-connect ip port
                                                :timeout 1
                                                :element-type '(unsigned-byte 8)))
           (let ((error (lodds.low-level-api:get-info
                         (usocket:socket-stream socket) last-change)))
             (unless (eql 0 error)
               (error "low level api threw error ~a in get-info" error)))
           (multiple-value-bind (error type timestamp changes)
               (lodds.low-level-api:handle-info socket)
             (unless (eql 0 error)
               (error "low level api threw error ~a in handle-info" error))
             (when (eql type :all)
               (setf table-hash (make-hash-table :test 'equal)
                     table-name (make-hash-table :test 'equal)))
             (loop :for (typ cs size name) :in changes
                   :if (eql typ :add)
                   :do (let ((val (gethash cs table-hash)))
                         (unless (find name val :test #'string=)
                           (setf (gethash cs table-hash)
                                 (cons name val)))
                         (setf (gethash name table-name)
                               (list cs size)))
                   :else
                   :do (let ((new-val (remove name (gethash cs table-hash)
                                              :test #'string=)))

                         (if new-val
                             (setf (gethash cs table-hash)
                                   new-val)
                             (remhash cs table-hash))
                         (remhash name table-name)))
             (setf last-change timestamp)
             (lodds.event:push-event :list-update
                                     name
                                     type
                                     timestamp
                                     changes)))
      (when socket
        (usocket:socket-close socket)))))

(defun remove-old-clients (&optional (current-time (lodds.core:get-timestamp)))
  "removes all clients older than client-timeout"
  (let ((clients (lodds:clients lodds:*server*)))
    (maphash (lambda (key client)
               (when (> (- current-time (lodds:c-last-message client))
                        (lodds.config:get-value :client-timeout))
                 (remhash key clients)
                 (lodds.event:push-event :client-removed
                                         key)))
             clients)))

(defun handle-message (message)
  (multiple-value-bind (error result) (lodds.low-level-api:read-advertise message)
    (unless (eql error 0)
      (lodds.event:push-event :error
                              (format nil
                                      "low-level-api:read-advertise returned ~a"
                                      error))
      (return-from handle-message))
    (lodds.event:push-event :listener result)
    (let ((current-time (lodds.core:get-timestamp)))
      (remove-old-clients current-time)
      ;; add client
      (destructuring-bind (ip port timestamp-l-c user-load user) result
        (lodds.task:submit-task
         (make-instance 'lodds.task:task-info
                        :name "update-info"
                        :user user
                        :ip ip
                        :port port
                        :timestamp current-time
                        :last-change timestamp-l-c
                        :user-load user-load))))))

(defun get-next-message (socket)
  (let* ((buffer-size 2048) ;; TODO: move to config
         (buffer (make-array buffer-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (if (lodds.core:input-rdy-p socket 1)
        (multiple-value-bind (recv n remote-host remote-port)
            (usocket:socket-receive socket buffer buffer-size)
          (declare (ignore recv remote-host remote-port))
          (if (plusp n)
              (flexi-streams:octets-to-string
               (subseq buffer 0 n))
              (error "listener:get-next-message: receive error: ~A" n)))
        (remove-old-clients))))

(defun run ()
  (let ((socket nil))
    (labels ((get-socket ()
               (usocket:socket-connect
                nil nil
                :local-host (lodds.core:get-broadcast-address
                             (lodds.config:get-value :interface))
                :local-port (lodds.config:get-value :broadcast-port)
                :protocol :datagram)))
      (handler-case
          (progn
            (setf socket (get-socket))
            (loop :while (lodds.subsystem:alive-p (lodds:get-subsystem :listener))
                  :do (let ((msg (get-next-message socket)))
                        (when msg
                          (handle-message msg)))))
        (error (e)
          (declare (ignore e))
          (when socket
            (usocket:socket-close socket))))
      (when socket
        (usocket:socket-close socket))
      (let ((clients (lodds:clients lodds:*server*)))
        (maphash (lambda (key client)
                   (declare (ignore client))
                   (remhash key clients)
                   (lodds.event:push-event :client-removed
                                           key))
                 clients)))))
