(in-package #:lodds.handler)

(defun run ()
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (usocket:socket-listen
                         (lodds.core:get-ip-address (lodds:interface lodds:*server*))
                         (lodds:handler-port lodds:*server*)
                         :reuse-address t
                         :element-type '(unsigned-byte 8)))
           (loop :while (lodds.subsystem:alive-p (lodds:get-subsystem :handler))
                 :do (when (and (lodds.core:input-rdy-p socket 1)
                                (eql :read (usocket:socket-state socket)))
                       (lodds.task:submit-task
                        (make-instance 'lodds.task:task-request
                                       :name "request"
                                       :socket (let ((socket
                                                       (usocket:socket-accept socket
                                                                              :element-type '(unsigned-byte 8))))
                                                 ;; TODO: move specified timeout to settings
                                                 (setf (usocket:socket-option socket :receive-timeout) 1)
                                                 socket))))))
      (when socket
        (usocket:socket-close socket)))))

