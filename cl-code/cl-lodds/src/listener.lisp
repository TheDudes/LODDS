#|

This File contains the Listener, which is the counterpart to the
Advertiser. His job is to listen for broadcast messages and update the
user infos on the lodds-server object once he gets new information.

|#

(in-package lodds.listener)

(defun handle-message (message)
  (let ((current-time (lodds.core:get-timestamp)))
    (destructuring-bind (ip port timestamp-l-c user-load user)
        (lodds.low-level-api:read-advertise message)
      (let ((user-info (lodds:get-user-info user)))
        (unless user-info
          ;; add user
          (setf user-info
                (make-instance 'lodds:user-info
                               :name user
                               :last-message current-time
                               :ip (usocket:dotted-quad-to-vector-quad ip)
                               :port port
                               :last-change 0
                               :load user-load)
                (gethash user (lodds:users lodds:*server*))
                user-info)
          (lodds.event:push-event :user-added
                                  user
                                  user-load
                                  timestamp-l-c))
        (let ((old-load (lodds:user-load user-info)))
          (setf (lodds:user-last-message user-info) current-time
                (lodds:user-load user-info) user-load)
          (let ((user-has-new-changes-p
                  (<= (lodds:user-last-change user-info)
                      timestamp-l-c)))
            (when user-has-new-changes-p
              (lodds.task:task-run (make-instance 'lodds.task:task-get-info
                                                  :tasks (lodds:get-tasks)
                                                  :user user)))
            (when (or user-has-new-changes-p
                      (not (eql old-load user-load)))
              (lodds.event:push-event :user-updated
                                      user
                                      user-load
                                      timestamp-l-c))))))))

(defun get-next-message (socket)
  (let* ((buffer-size 2048) ;; TODO: move to config
         (buffer (make-array buffer-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (when (lodds.core:input-rdy-p socket 1)
      (multiple-value-bind (recv n remote-host remote-port)
          (usocket:socket-receive socket buffer buffer-size)
        (declare (ignore recv remote-host remote-port))
        (if (plusp n)
            (lodds.core:octets-to-string
             (subseq buffer 0 n))
            (error "listener:get-next-message: receive error: ~A" n))))))

(defun run (listener)
  (let ((socket nil))
    (with-slots (alive-p) listener
      (handler-case
          (progn
            (setf socket (usocket:socket-connect
                          nil nil
                          :local-host
                          #-os-windows (lodds.core:get-broadcast-address
                                        (lodds.config:get-value :interface))
                          #+os-windows #(0 0 0 0)
                          :local-port (lodds.config:get-value :broadcast-port)
                          :protocol :datagram))
            (loop :while alive-p
                  :do (let ((msg (get-next-message socket)))
                        (when msg
                          (handler-case
                              (handle-message msg)
                            (error (err)
                              (lodds.event:push-event
                               :error (format nil "listener: ~a" err))))))))
        (error (e)
          (lodds.event:push-event :listener-error
                                  e)))
      (when socket
        (usocket:socket-close socket)
        (setf socket nil))
      (let ((users (lodds:users lodds:*server*)))
        (maphash (lambda (key user)
                   (declare (ignore user))
                   (remhash key users)
                   (lodds.event:push-event :user-removed key))
                 users)))))

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
