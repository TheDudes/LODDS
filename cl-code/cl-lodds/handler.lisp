(in-package #:lodds.handler)

(defun run ()
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (usocket:socket-listen
                         (lodds:get-ip-address (lodds:interface lodds:*server*))
                         (lodds:handler-port lodds:*server*)
                         :reuse-address t
                         :element-type '(unsigned-byte 8)))
           (loop :while (lodds.subsystem:alive-p (lodds:get-subsystem :handler))
                 :do (when (and (usocket:wait-for-input socket
                                                        :timeout 1
                                                        :ready-only t)
                                (eql :read (usocket:socket-state socket)))
                       (lodds.task:submit-task
                        (make-instance 'lodds.task:task-request
                                       :name "request"
                                       :socket (usocket:socket-accept socket
                                                                      :element-type '(unsigned-byte 8)))))))
      (when socket
        (usocket:socket-close socket)))))

