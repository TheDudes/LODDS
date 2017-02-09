(in-package #:lodds.handler)

(defmacro with-socket (socket close-socket-p &body body)
  `(let ((is-closed nil))
     (labels ((cleanup-socket ()
                (when ,socket
                  (handler-case
                      (close (usocket:socket-stream ,socket))
                    (error (e)
                      (lodds.event:push-event :error
                                              (list "could not close socket stream" e))))
                  (handler-case
                      (usocket:socket-close ,socket)
                    (error (e)
                      (lodds.event:push-event :error
                                              (list "could not close socket" e))))
                  (setf is-closed t))))
       (handler-case
           (progn
             (unless ,socket
               (error "Could not access socket, nil."))
             ,@body)
         (end-of-file ()
           (lodds.event:push-event :error
                                   (list "end-of-file on handler"))
           (cleanup-socket))
         (error (e)
           (lodds.event:push-event :error
                                   (list "unknown error on handler:" e))
           (cleanup-socket)))
       ,(when close-socket-p
          `(unless is-closed
             (cleanup-socket))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request))
  (let ((socket (lodds.task:request-socket task)))
    (with-socket socket nil
      (multiple-value-bind (error request)
          (lodds.low-level-api:parse-request (usocket:socket-stream socket))
        (if (> error 0)
            (error "low level api Returned error ~a~%" error)
            (lodds.task:submit-task
             (case (car request)
               (:file
                (destructuring-bind (checksum start end) (cdr request)
                  (make-instance 'lodds.task:task-request-file
                                 :name "request-file"
                                 :request-socket socket
                                 :request-checksum checksum
                                 :request-start start
                                 :request-end end)))
               (:info
                (make-instance 'lodds.task:task-request-info
                               :name "request-info"
                               :request-socket socket
                               :request-timestamp (cadr request)))
               (:send-permission
                (destructuring-bind (size timeout filename) (cdr request)
                  (make-instance 'lodds.task:task-request-send-permission
                                 :name "request-send-permission"
                                 :request-socket socket
                                 :request-size size
                                 :request-timeout timeout
                                 :request-filename filename))))))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-file))
  (with-accessors ((socket lodds.task:request-socket)
                   (checksum lodds.task:request-checksum)
                   (start lodds.task:request-start)
                   (end lodds.task:request-end)
                   (written lodds.task:request-written)
                   (filename lodds.task:request-filename)
                   (file-stream lodds.task:request-file-stream)) task
    (labels ((cleanup (&optional error-occured)
               (unless error-occured
                 (lodds.event:push-event :info (list "uploaded file"
                                                     filename)))
               (when socket
                 (usocket:socket-close socket))
               (when file-stream
                 (close file-stream))))
      (handler-case
          (let ((chunk-size (ash 1 21))) ;; 2 MB
            (unless filename
              (unless (setf filename (lodds.watcher:get-file-info checksum))
                (lodds.event:push-event :error
                                        (list "requested file could not be found"))
                (cleanup t)
                (return-from lodds.task:run-task))
              (setf file-stream (open filename
                                      :direction :input
                                      :element-type '(unsigned-byte 8)))
              (lodds:update-load (- end start))
              (unless (eql start 0)
                (file-position file-stream start)))
            (let ((left-to-upload (- (- end start)
                                     written)))
              (lodds.core:copy-stream file-stream
                                      (usocket:socket-stream socket)
                                      (if (> left-to-upload chunk-size)
                                          (progn (incf written chunk-size)
                                                 (lodds:update-load (- chunk-size))
                                                 chunk-size)
                                          (progn (incf written left-to-upload)
                                                 (lodds:update-load (- left-to-upload))
                                                 left-to-upload)))
              (finish-output file-stream)
              (if (eql (- end start) written)
                  (cleanup)
                  (lodds.task:submit-task task))))
        (error (e)
          (cleanup t)
          (lodds:update-load (- written (- end start)))
          (lodds.event:push-event :error
                                  (list "on request file" filename ":" e)))))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-info))
  (with-accessors ((socket lodds.task:request-socket)
                   (timestamp lodds.task:request-timestamp)) task
    (with-socket socket t
      (apply #'lodds.low-level-api:respond-info
             (usocket:socket-stream socket)
             (lodds:generate-info-response timestamp)))))

(defmethod lodds.task:run-task ((task lodds.task:task-request-send-permission))
  (with-accessors ((socket lodds.task:request-socket)
                   (size lodds.task:request-size)
                   (timeout lodds.task:request-timeout)
                   (filename lodds.task:request-filename)) task
    (declare (ignore timeout))
    (with-socket socket t
      ;; TODO: ask user here for file path
      (with-open-file (file-stream (concatenate 'string "/tmp/" filename)
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
        (lodds.low-level-api:respond-send-permission (usocket:socket-stream socket)
                                                     file-stream
                                                     size)))))

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
                                       :request-socket (usocket:socket-accept socket
                                                                              :element-type '(unsigned-byte 8)))))))
      (when socket
        (usocket:socket-close socket)))))
