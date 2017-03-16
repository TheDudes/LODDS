#|

This File contains the Listener, which is the counterpart to the
Advertiser. His job is to listen for broadcast messages and update the
client infos on the lodds-server object once he gets new information.

|#

(in-package lodds.listener)

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
          (lodds.event:push-event :listener
                                  e)
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
