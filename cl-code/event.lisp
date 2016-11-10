(in-package #:lodds.event)

(defun add-callback (name fn &key (overwrite nil))
  (let ((evt-queue (lodds:get-subsystem :event-queue)))
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
          nil)))))

(defun remove-callback (name)
  (setf (gethash name (callbacks (lodds:get-subsystem :event-queue)))
        nil))

(defun push-event (subsystem event)
  (stmx:atomic
   (stmx.util:put (queue (lodds:get-subsystem :event-queue))
                  (list subsystem event))))

(defun run ()
  (loop
    (let* ((event-queue (lodds:get-subsystem :event-queue)))
      (if event-queue
          (loop
            :for cb :being :the :hash-value :of (callbacks event-queue)
            :do (apply cb (stmx.util:take (queue event-queue))))
          (error "Event-Queue is nil!"))))) ;; just wait until event-queue was added to subsystems
