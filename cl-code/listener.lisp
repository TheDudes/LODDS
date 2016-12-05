(in-package lodds.listener)

(defun update-client-list (client)
  (let ((socket nil)
        (stream nil))
    (unwind-protect
         (with-accessors ((name lodds:c-name)
                          (ip lodds:c-ip)
                          (port lodds:c-port)
                          (last-change lodds:c-last-change)
                          (table-hash lodds:c-file-table-hash)
                          (table-name lodds:c-file-table-name)) client
           (setf socket (usocket:socket-connect ip port :timeout 1)
                 stream (usocket:socket-stream socket))
           (let ((error (lodds.low-level-api:get-info stream last-change)))
             (unless (eql 0 error)
               (error "low level api threw error ~a in get-info" error)))
           (multiple-value-bind (error type timestamp changes)
               (lodds.low-level-api:handle-info stream)
             (unless (eql 0 error)
               (error "low level api threw error ~a in handle-info" error))
             (lodds.event:push-event :list-update
                                     (list name type timestamp changes))
             (when (eql type :all)
               (setf table-hash (make-hash-table :test 'equal)
                     table-name (make-hash-table :test 'equal)))
             (loop :for (typ cs size name) :in changes
                   :if (eql typ :add)
                   :do (let ((val (gethash cs table-hash)))
                         (unless (find name val :test #'string=)
                           (setf (gethash cs table-hash)
                                 (cons name val)))
                         (setf (gethash name table-name)
                               (list cs size)))
                   :else
                   :do (let ((new-val (remove name (gethash cs table-hash)
                                              :test #'string=)))

                         (if new-val
                             (setf (gethash cs table-hash)
                                   new-val)
                             (remhash cs table-hash))
                         (remhash name table-name)))
             (setf last-change timestamp)))
      (when stream
        (close stream))
      (when socket
        (usocket:socket-close socket)))))

(defmethod lodds.task:run-task ((task lodds.task:task-client-info))
  (with-accessors ((cn lodds.task:client-name)
                   (ci lodds.task:client-ip)
                   (cp lodds.task:client-port)
                   (cmt lodds.task:client-message-timestamp)
                   (clc lodds.task:client-last-change)
                   (cl lodds.task:client-load)) task
    (let ((client-info (gethash cn (lodds:clients lodds:*server*))))
      (unless client-info
        (setf client-info (make-instance 'lodds:client-info
                                         :c-name cn
                                         :c-last-message cmt
                                         :c-ip ci
                                         :c-port cp
                                         :c-last-change 0
                                         :c-load cl)
              (gethash cn (lodds:clients lodds:*server*)) client-info))
      (let ((locked (bt:acquire-lock (lodds:c-lock client-info) nil)))
        (if locked
            ;; only go on if we locked, if not, just drop the update, we
            ;;will update on the next advertise. unwind-protect to be sure
            ;;we unlock that lock.
            (unwind-protect
                 (handler-case
                     (progn
                       (setf (lodds:c-last-message client-info) cmt
                             (lodds:c-load client-info) cl)
                       (when (< (lodds:c-last-change client-info)
                                clc)
                         (update-client-list client-info)))
                   (error (e)
                     (format t "got error inside update-client-list ~a~%" e)))
              (bt:release-lock (lodds:c-lock client-info)))
            (lodds.event:push-event :info (list :dropped task)))))))

(defun handle-message (message)
  (multiple-value-bind (error result) (lodds.low-level-api:read-advertise message)
    (unless (eql error 0)
      (error "TODO: remove me: ERROR from low-level-api: ~a~%" error))
    (lodds.event:push-event :listener result)
    (let ((current-time (lodds.core:get-timestamp))
          (clients (lodds:clients lodds:*server*)))
      ;; remove all clients older then :client-timeout
      (maphash (lambda (key client)
                 (when (> (- current-time (lodds:c-last-message client))
                          (lodds:client-timeout lodds:*server*))
                   (remhash key clients)))
               clients)
      ;; add client
      (destructuring-bind (ip port timestamp-l-c load name) result
        (lodds.event:push-event :task
                                (make-instance 'lodds.task:task-client-info
                                               :name "update-client-info"
                                               :client-name name
                                               :client-ip ip
                                               :client-port port
                                               :client-message-timestamp current-time
                                               :client-last-change timestamp-l-c
                                               :client-load load))))))

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

(defun run ()
  (let ((socket nil))
    (unwind-protect
         (progn
           (setf socket (usocket:socket-connect
                         nil nil
                         :local-host (lodds:get-broadcast-address
                                      (lodds:interface lodds:*server*))
                         :local-port (lodds:broadcast-port lodds:*server*)
                         :protocol :datagram))
           (loop (handle-message (get-next-message socket))))
      (when socket
        (usocket:socket-close socket)))))
