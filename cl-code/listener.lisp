(in-package lodds.listener)

(defun handle-message (server message)
  (format t "broadcast: ~a ~a~%" (lodds.core:get-timestamp) message)
  (multiple-value-bind (error result) (lodds.low-level-api:read-advertise message)
    (unless (eql error 0)
      (error "TODO: remove me: ERROR from read-advertise != 0~%"))
    (let ((current-time (lodds.core:get-timestamp))
          (clients (lodds:clients server)))
      ;; remove all clients older then :client-timeout
      (maphash (lambda (key val)
                 (when (> (- current-time (car val))
                          (lodds:client-timeout server))
                   (remhash key clients)))
               clients)
      ;; add client
      (setf (gethash (car (last result)) (lodds:clients server))
            (cons current-time result)))))

(defun get-next-message (socket)
  (let* ((buffer-size 2048) ;; TODO: move to config
         (buffer (make-array buffer-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (multiple-value-bind (return-sockets real-time)
        (usocket:wait-for-input socket :timeout 1)
      (declare (ignore return-sockets))
      (if (not real-time)
          (get-next-message socket)
          (multiple-value-bind (recv n remote-host remote-port)
              (usocket:socket-receive socket buffer buffer-size)
            (declare (ignore recv remote-host remote-port))
            (if (plusp n)
                (flexi-streams:octets-to-string
                 (subseq buffer 0 n))
                (error "listener:get-next-message: receive error: ~A" n)))))))

(defun run (server)
  (let ((socket (usocket:socket-connect
                 nil nil
                 :local-host (lodds:get-broadcast-address "enp0s25")
                 :local-port (lodds:broadcast-port server)
                 :protocol :datagram)))
    (unwind-protect
         (let ((running t))
           (loop
              :while running
              :do (handler-case
                      (handle-message server
                                      (get-next-message socket))
                    (lodds:shutdown-condition () (setf running nil))
                    (error (e)
                      (format t "got error: ~a~%" e)
                      (setf running nil)))))
      (usocket:socket-close socket)
      (format t "Listener stopped!~%")
      (setf (lodds:listener server) nil))))
