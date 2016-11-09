(in-package #:lodds.event)

(defun add-callback (evt-queue name fn &key (overwrite nil))
  (labels ((set-value ()
             (setf (gethash name (callbacks evt-queue)) fn)))
    (restart-case
        (if overwrite
            (set-value)
            (multiple-value-bind (value exists)
                (gethash name (callbacks evt-queue))
              (if exists
                  (error "Callback ~a already Exists and is set to ~a!"
                         name value)
                  (set-value))))
      (overwrite-callback ()
        (set-value))
      (dont-overwrite-callback ()
        nil))))

(defun remove-callback (evt-queue name)
  (setf (gethash name (callbacks evt-queue))
        nil))

(defun push-event (evt-queue subsystem event)
  (stmx:atomic
   (stmx.util:put (queue evt-queue)
                  (list subsystem event))))

(defun run (subsystem)
  (loop
     (let ((event (stmx.util:take (queue subsystem))))
       (loop :for cb :being :the :hash-value :of (callbacks subsystem)
          :with args = (callback-args subsystem)
          :do (if args
                  (apply cb args event)
                  (apply cb event))))))