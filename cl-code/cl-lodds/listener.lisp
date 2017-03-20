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
      (destructuring-bind (ip port timestamp-l-c user-load user) result
        (let ((client-info (lodds:get-user-info user)))
          (unless client-info
            ;; add client
            (setf client-info
                  (make-instance 'lodds:client-info
                                 :c-name user
                                 :c-last-message current-time
                                 :c-ip ip
                                 :c-port port
                                 :c-last-change 0
                                 :c-load user-load)
                  (gethash user (lodds:clients lodds:*server*))
                  client-info)
            (lodds.event:push-event :client-added
                                    user
                                    user-load
                                    timestamp-l-c))
          (let ((old-load (lodds:c-load client-info)))
            (setf (lodds:c-last-message client-info) current-time
                  (lodds:c-load client-info) user-load)
            (let ((user-has-new-changes-p
                    (<= (lodds:c-last-change client-info)
                        timestamp-l-c)))
              (when user-has-new-changes-p
                (lodds.event-loop:with-event-loop ((lodds:get-event-loop))
                  (lodds.event-loop:ev-update-user-info user)))
              (when (or user-has-new-changes-p
                        (not (eql old-load user-load)))
                (lodds.event:push-event :client-updated
                                        user
                                        user-load
                                        timestamp-l-c)))))))))

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

(defun run (listener)
  (let ((socket nil))
    (with-slots (alive-p) listener
      (handler-case
          (progn
            (setf socket (usocket:socket-connect
                          nil nil
                          :local-host (lodds.core:get-broadcast-address
                                       (lodds.config:get-value :interface))
                          :local-port (lodds.config:get-value :broadcast-port)
                          :protocol :datagram))
            (loop :while alive-p
                  :do (let ((msg (get-next-message socket)))
                        (when msg
                          (handle-message msg)))))
        (error (e)
          (lodds.event:push-event :listener
                                  e)))
      (when socket
        (usocket:socket-close socket)
        (setf socket nil))
      (let ((clients (lodds:clients lodds:*server*)))
        (maphash (lambda (key client)
                   (declare (ignore client))
                   (remhash key clients)
                   (lodds.event:push-event :client-removed
                                           key))
                 clients)))))

(defun start ()
  (let ((listener (lodds:get-listener)))
    (with-slots (thread alive-p) listener
      (unless alive-p
        (setf thread
              (bt:make-thread (lambda ()
                                (setf alive-p t)
                                (run listener)
                                (setf alive-p nil))
                              :name "Lodds - Listener"))))))

(defun stop ()
  (setf (slot-value (lodds:get-listener) 'alive-p) nil))
