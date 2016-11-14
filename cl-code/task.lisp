(in-package #:lodds.task)

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a"
            (name object))))

(defmethod print-object ((object task-client) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((name name)
                     (c-name client-name)) object
      (format stream "~a :client ~a"
              name
              c-name))))

(defmethod print-object ((object task-request-file) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((name name)
                     (checksum request-checksum)
                     (start request-start)
                     (end request-end)) object
      (format stream "~a :checksum ~a :start ~a :end ~a"
              name
              (concatenate 'string (subseq checksum 0 7) "...")
              start
              end))))

(defmethod print-object ((object task-request-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((name name)
                     (timestamp request-timestamp)) object
      (format stream "~a :timestamp ~a"
              name
              timestamp))))

(defmethod print-object ((object task-request-send-permission) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((name name)
                     (size request-size)
                     (timeout request-timeout)
                     (filename request-filename)) object
      (format stream "~a :size ~a :timeout ~a :filename ~a"
              name
              size
              timeout
              filename))))

(defgeneric run-task (task)
  (:documentation "Generic Function which will be called if task was
  added to thread-pool"))

(defmethod run-task ((tsk task))
  (declare (ignorable tsk))
  (error "overwrite run-task with ur task!"))

(defun handle-task (event)
  (let* ((tasker (lodds:get-subsystem :tasker))
         (lparallel:*kernel* (kernel tasker)))
    (lparallel:submit-task (channel tasker)
                           #'run-task
                           (cadr event))))

(defmethod lodds.subsystem:start ((subsys tasker))
  (with-accessors ((kernel kernel)
                   (channel channel)
                   (alive-p lodds.subsystem:alive-p)) subsys
    (if alive-p
        (lodds.event:push-event :tasker (list "already running!"))
        (let ((lparallel:*kernel* (lparallel:make-kernel 10
                                                         :bindings `((lodds:*server* . ,lodds:*server*)))))
          (setf kernel  lparallel:*kernel*
                channel (lparallel:make-channel)
                alive-p t)
          (lodds.event:add-callback :tasker
                                    #'handle-task
                                    :event-type :task)
          (lodds.event:push-event :tasker
                                  (list "started!"))))))

(defmethod lodds.subsystem:stop ((subsys tasker))
  (with-accessors ((kernel kernel)
                   (alive-p lodds.subsystem:alive-p)) subsys
    (when alive-p
      (let ((lparallel:*kernel* kernel))
        (lparallel:end-kernel :wait t)
        (lodds.event:remove-callback :tasker :event-type :task)
        (setf alive-p nil)
        (lodds.event:push-event (lodds.subsystem:name subsys)
                                (list "stopped!"))))))
