(in-package #:lodds.subsystem)

(define-condition shutdown-condition (error)
  nil)

(defmethod print-object ((object subsystem) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name thread alive-p) object
      (format stream "~a :alive-p ~a :thread ~a"
              name alive-p thread))))

(defgeneric start (subsystem)
  (:documentation
   "Starts the given subsystem."))

(defgeneric stop (subsystem)
  (:documentation
   "Stops the given subsystem."))

(defun save-start (subsystem)
  (labels ((save-init-fn ()
             (unwind-protect
                  (progn
                    (setf (alive-p subsystem) t)
                    (loop
                       :while (alive-p subsystem)
                       :do (handler-case
                               (funcall (init-fn subsystem))
                             (shutdown-condition ()
                               (setf (alive-p subsystem) nil))
                             (error (e)
                               (lodds.event:push-event (name subsystem)
                                                       (list :error e))
                               (setf (alive-p subsystem) nil)))))
               (lodds.event:push-event (name subsystem)
                                       (list "stopped!"))
               (let ((cfn (cleanup-fn subsystem)))
                 (when cfn
                   (funcall cfn)))
               (setf (alive-p subsystem) nil))))
    (if (alive-p subsystem)
        (lodds.event:push-event (lodds.subsystem:name subsystem)
                                (list "already running!"))
        (progn
          (setf
           (thread subsystem)
           (bt:make-thread #'save-init-fn
                           :name (format nil "LODDS-~a" (name subsystem))))
          (lodds.event:push-event (lodds.subsystem:name subsystem)
                                  (list "started!"))))))

(defmethod start ((subsys subsystem))
  (when subsys
    (save-start subsys)))

(defmethod stop ((subsys subsystem))
  (when (and subsys
             (alive-p subsys))
    (bt:interrupt-thread
     (thread subsys)
     (lambda ()
       (signal (make-condition 'shutdown-condition))))))
