(in-package #:lodds.handler)

(defmacro with-socket (socket &body body)
  `(unwind-protect
        (handler-case
            (progn
              (unless ,socket
                (error "WTF WHY IS MY SOCKET NIL"))
              ,@body)
          (end-of-file ()
            (lodds.event:push-event :error
                                    (list :end-of-file)))
          (error (e)
            (lodds.event:push-event :error
                                    (list e))))
     (when ,socket
       (close (usocket:socket-stream ,socket))
       (usocket:socket-close ,socket))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-file))
  (with-accessors ((socket lodds.task:request-socket)
                   (checksum lodds.task:request-checksum)
                   (start lodds.task:request-start)
                   (end lodds.task:request-end)) task
    (with-socket socket
      (let ((filename (lodds.watcher:get-file-info checksum)))
        (if filename
            (with-open-file (file-stream filename
                                         :direction :input)
              (lodds.low-level-api:respond-file (usocket:socket-stream socket)
                                                file-stream
                                                start end))
            (lodds.event:push-event :handler
                                    (list :error :local-file-not-found)))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-info))
  (with-accessors ((socket lodds.task:request-socket)
                   (timestamp lodds.task:request-timestamp)) task
    (with-socket socket
      (apply #'lodds.low-level-api:respond-info
             (usocket:socket-stream socket)
             (lodds:generate-info-response timestamp)))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-send-permission))
  (with-accessors ((socket lodds.task:request-socket)
                   (size lodds.task:request-size)
                   (timeout lodds.task:request-timeout)
                   (filename lodds.task:request-filename)) task
    (declare (ignore timeout))
    (with-socket socket
      ;; TODO: ask user here for file path
      (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                   :direction :output
                                   :if-exists :supersede)
        (lodds.low-level-api:respond-send-permission (usocket:socket-stream socket)
                                                     file-stream
                                                     size)))))

(defun handler-callback (socket)
  "handles incomming connections and starts threads to handle
  requests"
  (multiple-value-bind (error request)
      (lodds.low-level-api:parse-request (usocket:socket-stream socket))
    (if (eql 0 error)
        (let ((new-task nil))
          (case (car request)
            (:file
             (destructuring-bind (checksum start end) (cdr request)
               (setf new-task
                     (make-instance 'lodds.task:task-request-file
                                    :name "request-file"
                                    :request-socket socket
                                    :request-checksum checksum
                                    :request-start start
                                    :request-end end))))
            (:info
             (setf new-task
                   (make-instance 'lodds.task:task-request-info
                                  :name "request-info"
                                  :request-socket socket
                                  :request-timestamp (cadr request))))
            (:send-permission
             (destructuring-bind (size timeout filename) (cdr request)
               (setf new-task
                     (make-instance 'lodds.task:task-request-send-permission
                                    :name "request-send-permission"
                                    :request-socket socket
                                    :request-size size
                                    :request-timeout timeout
                                    :request-filename filename)))))
          (lodds.event:push-event :task new-task))
        (error "low level api Returned error ~a~%" error))))

(defun run ()
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (usocket:socket-listen
                         (lodds:get-ip-address (lodds:interface lodds:*server*))
                         (lodds:handler-port lodds:*server*)
                         :reuse-address t))
           (loop (handler-callback (usocket:socket-accept socket))))
      (when socket
        (usocket:socket-close socket)))))
