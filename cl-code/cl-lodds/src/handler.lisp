#|

This file contains the Handler, his job is to listen for incomming
connections. When there is a connection he will submit a task-requst
thread to the tasker, which then handles the request.

|#

(in-package #:lodds.handler)

(defun open-socket (handler)
  (handler-case
      (usocket:socket-listen
       (lodds.core:get-ip-address (lodds.config:get-value :interface))
       (lodds.config:get-value :port)
       :reuse-address t
       :element-type '(unsigned-byte 8))
    (error (e)
      (lodds.event:push-event :error
                              (format nil "Error starting handler(~a)" e))
      (sleep 1)
      (when (slot-value handler 'alive-p)
        (open-socket handler)))))

(defun run (handler)
  (with-slots (alive-p) handler
    (usocket:with-server-socket (socket (open-socket handler))
      #+os-windows (setf (slot-value handler 'socket)
                         socket)
      (loop :while alive-p
            :when
            #-os-windows (and (lodds.core:input-rdy-p socket 1)
                              (eql :read (usocket:socket-state socket)))
            #+os-windows t
            :do
            (let ((client-socket
                    (handler-case
                        (usocket:socket-accept socket
                                               :element-type '(unsigned-byte 8))
                      (usocket:connection-aborted-error ()
                        (lodds.event:push-event :error
                                                "Connection aborted error on socket-accept")
                        nil))))
              (when client-socket
                (lodds.core:set-socket-timeout client-socket
                                               (lodds.config:get-value :socket-timeout))
                (lodds.task:task-run
                 (make-instance 'lodds.task:task-request
                                :tasks (lodds:get-tasks)
                                :socket client-socket))))))))

(defun start ()
  (let ((handler (lodds:get-handler)))
    (with-slots (thread alive-p) handler
      (unless alive-p
        (setf thread
              (bt:make-thread (lambda ()
                                (setf alive-p t)
                                (run handler)
                                (setf alive-p nil))
                              :name "Lodds - Handler"))))))

(defun stop ()
  (setf (slot-value (lodds:get-handler) 'alive-p) nil)
  #+os-windows
  (handler-case (with-slots (socket) (lodds:get-handler)
                  (when socket
                    (usocket:socket-close socket)
                    (setf socket nil)))
    (error (e)
      (lodds.event:push-event :error "error on closing handler socket" e))))
