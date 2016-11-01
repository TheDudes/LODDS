(in-package #:lodds.handler)

(define-condition shutdown (error)
  nil)

(defun handle-file-request (server socket request)
  (destructuring-bind (checksum start end) request
    (let ((filename (lodds:get-file-info server checksum)))
      (if filename
          (with-open-file (file-stream filename
                                       :direction :input)
            (lodds.low-level-api:respond-file (usocket:socket-stream socket)
                                              file-stream
                                              start end))
          (format t "TODO: could not find file!!~%")))))

(defun handle-info-request (server socket request)
  (apply #'lodds.low-level-api:respond-info
         (usocket:socket-stream socket)
         (apply #'lodds:generate-info-response server request)))

(defun handle-send-permission-request (server socket request)
  (destructuring-bind (size timeout filename) request
    (declare (ignore timeout))
    ;; TODO: ask user here for file path
    (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                 :direction :output
                                 :if-exists :supersede)
      (lodds.low-level-api:respond-send-permission (usocket:socket-stream socket)
                                                   file-stream
                                                   size))))

(defun handler-callback (server socket)
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
              (funcall fn server socket (cdr request)))
            (error "low level api Returned error ~a~%" error)))
    ;; TODO: error handling
    (end-of-file ()
      (format t "TODO: tcp: got end of file~%"))
    (error (e)
      (format t "TODO: tcp: error occured: ~a~%" e)))
  ;; TODO: handler should spawn threads which will close stream/socket
  (close (usocket:socket-stream socket))
  (usocket:socket-close socket))

(defun handler-loop (server)
  (let ((socket (usocket:socket-listen
                 (lodds:get-ip-address (lodds:interface server))
                 (lodds:handler-port server)
                 :reuse-address t)))
    (unwind-protect
         (let ((running t))
           (loop
              :while running
              :do (handler-case
                      (handler-callback server
                                        (usocket:socket-accept socket))
                    (usocket:bad-file-descriptor-error ()
                      (setf running nil))
                    (shutdown () (setf running nil))
                    (error (e)
                      (format t "got error: ~a~%" e)
                      (setf running nil)))))
      (close (usocket:socket-stream socket))
      (usocket:socket-close socket)
      (format t "Handler stopped!~%")
      (setf (lodds:handler server) nil))))

(defun start (server)
  (let ((handler (lodds:handler server)))
    (if handler
        (format t "HANDLER already running.~%")
        (setf (lodds:handler server)
              (bt:make-thread
               (lambda ()
                 (handler-loop server))
               :name "Handler")))))

(defun stop (server)
  (let ((handler (lodds:handler server)))
    (if handler
        (bt:interrupt-thread handler
                             (lambda ()
                               (signal (make-condition 'shutdown))))
        (format t "handler not running!~%"))))


