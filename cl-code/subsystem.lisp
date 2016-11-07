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
                               (apply (init-fn subsystem) subsystem (init-args subsystem))
                             (lodds.subsystem:shutdown-condition ()
                               (setf (alive-p subsystem) nil))
                             (error (e)
                               (format t "got uncaught error from subsystem ~a: ~a~%"
                                       (name subsystem)
                                       e)
                               (setf (alive-p subsystem) nil)))))
               (format t "~a stopped!~%" (name subsystem))
               (setf (alive-p subsystem) nil))))
    (if (alive-p subsystem)
        (format t "Subsystem ~a is already Running!~%" (name subsystem))
        (setf
         (thread subsystem)
         (bt:make-thread #'save-init-fn
                         :name (format nil "LODDS-~a" (name subsystem)))))))

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

