(in-package #:lodds.event)

(defclass event-queue (lodds:subsystem)
  ((queue :accessor queue
          :initform (make-instance 'stmx.util:tfifo )
          :type stmx.util:tfifo
          :documentation "the actual queue containing events")
   (callbacks :accessor callbacks
              :initform (make-hash-table :test 'equalp)
              :type hash-table
              :documentation "hash-table containing callbacks which
              will be called by the event-queue if a event occurs")
   (callback-args :accessor callback-args
                  :initform nil
                  :type list
                  :documentation "Arguments passed to callbacks,
                  callback will be called like the following:

                  (apply #'callback-args subsystem event-type)

                  so these args are the first arguments!
                  if CALLBACK-ARGS is nil, they are left out")))

(defun add-callback (evt-queue name fn &key (overwrite nil))
  (restart-case
      (if overwrite
          (setf (gethash name (callbacks evt-queue))
                fn)
          (multiple-value-bind (value exists)
              (if exists
                  (when (error "Callback ~a already Exists and is set to ~a!"
                               name value)
                    (setf (gethash name (callbacks evt-queue))
                          fn)))))
    (overwrite-callback ()
      t)
    (dont-overwrite-callback ()
      nil)))

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
