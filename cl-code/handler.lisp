(in-package #:lodds.handler)

(defun handle-file-request (socket request)
  (destructuring-bind (checksum start end) request
    (let ((filename (lodds.watcher:get-file-info checksum)))
      (if filename
          (with-open-file (file-stream filename
                                       :direction :input)
            (lodds.low-level-api:respond-file (usocket:socket-stream socket)
                                              file-stream
                                              start end))
          (lodds.event:push-event :handler
                                  (list :error :local-file-not-found))))))

(defun handle-info-request (socket request)
  (apply #'lodds.low-level-api:respond-info
         (usocket:socket-stream socket)
         (apply #'lodds:generate-info-response request)))

(defun handle-send-permission-request (socket request)
  (destructuring-bind (size timeout filename) request
    (declare (ignore timeout))
    ;; TODO: ask user here for file path
    (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                 :direction :output
                                 :if-exists :supersede)
      (lodds.low-level-api:respond-send-permission (usocket:socket-stream socket)
                                                   file-stream
                                                   size))))

(defun handler-callback (socket)
  "handles incomming connections and starts threads to handle
  requests"
  (handler-case
      (multiple-value-bind (error request)
          (lodds.low-level-api:parse-request (usocket:socket-stream socket))
        (if (eql 0 error)
            (let ((fn (case (car request)
                        (:file #'handle-file-request)
                        (:info #'handle-info-request)
                        (:send-permission #'handle-send-permission-request))))
              (lodds.event:push-event :handler request)
              (funcall fn socket (cdr request)))
            (error "low level api Returned error ~a~%" error)))
    ;; TODO: error handling
    (end-of-file ()
      (lodds.event:push-event :handler
                              (list :error :end-of-file)))
    (error (e)
      (lodds.event:push-event :handler
                              (list :error e))))
  ;; TODO: handler should spawn threads which will close stream/socket
  (close (usocket:socket-stream socket))
  (usocket:socket-close socket))

(defun run ()
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (usocket:socket-listen
                         (lodds:get-ip-address (stmx:$ (lodds:interface lodds:*server*)))
                         (lodds:handler-port lodds:*server*)
                         :reuse-address t))
           (loop
              (handler-callback (usocket:socket-accept socket))))
      (when socket
        (usocket:socket-close socket)))))
