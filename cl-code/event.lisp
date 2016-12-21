(in-package #:lodds.event)

(defmacro update-callback (name fn alist-accessor)
  `(if (assoc ,name ,alist-accessor)
       (restart-case
           (error "Callback ~a already exists and is set to ~a!"
                  ,name (cdr (assoc ,name ,alist-accessor)))
         (overwrite-callback ()
           (setf (cdr (assoc ,name ,alist-accessor))
                 fn))
         (dont-overwrite-callback ()
           nil))
       (setf ,alist-accessor
             (append (list (cons ,name ,fn))
                     ,alist-accessor))))

(defun add-callback (name fn &key (event-type nil))
  "adds the given callback to the event-loop. NAME is a keyword which
  identifies the given callback. This is helpfull to overwrite or
  remove callbacks. FN is the callback function itself, it will be
  called with at least 2 arguments, the event-type and information
  about the event. If EVENT-TYPE is given, the given function FN will
  only be called for events with the specified EVENT-TYPE. If left
  out, FN will be called for every occuring event. EVENT-TYPE is, like
  NAME, a keyword. lodds:*server* needs to be bound, else a error will
  thrown."
  (let ((evt-queue (lodds:get-subsystem :event-queue)))
    (unless evt-queue
      (error "Could not find event-queue!"))
    (if event-type
        (let* ((ht (typed-callbacks evt-queue)))
          (update-callback name fn (gethash event-type ht)))
        (update-callback name fn (callbacks evt-queue)))))

(defun remove-callback (name &key event-type)
  "Removes a callback which refers to the given NAME. EVENT-TYPE must
  be specified if it was specified by adding the given callback with
  ADD-CALLBACK. lodds:*server* needs to be bound, else a error will be
  thrown."
  (let ((evt-queue (lodds:get-subsystem :event-queue)))
    (unless evt-queue
      (error "Could not find event-queue!"))
    (if event-type
        (let* ((ht (typed-callbacks evt-queue)))
          (unless (setf (gethash event-type ht)
                        (remove-if (lambda (cb)
                                     (string= (car cb)
                                              name))
                                   (gethash event-type ht)))
            (remhash event-type ht)))
        (setf (callbacks evt-queue)
              (remove-if (lambda (cb)
                           (string= (car cb)
                                    name))
                         (callbacks evt-queue))))))

(defun push-event (event-type event)
  "Pushes a given given EVENT of type EVENT-TYPE onto the
  event-queue. EVENT-TYPE is a keyword describing the event, EVENT can
  be anything."
  (lparallel.queue:push-queue (if (listp event)
                                  (cons event-type event)
                                  (list event-type event))
                              (queue (lodds:get-subsystem :event-queue))))

(defun handle-event (event event-queue)
  "handles a single event and calls it inside a RESTART-CASE. If a
  error occures its possible to RETRY, IGNORE or REMOVE the callback."
  (labels ((save-call (name fn &key event-type)
             (restart-case
                 (funcall fn event)
               (retry-calling-callback ()
                 ;; reload function from hashtable and try again
                 (destructuring-bind (new-name . new-fn)
                     (assoc name
                            (if event-type
                                (gethash event-type
                                         (typed-callbacks event-queue))
                                (callbacks event-queue)))
                   (save-call new-name new-fn :event-type event-type)))
               (ignore-callback ()
                 nil)
               (remove-callback ()
                 (remove-callback name :event-type event-type)))))
    (loop :for (name . fn) :in (callbacks event-queue)
          :do (save-call name fn))
    (loop :for (name . fn) :in  (gethash (car event)
                                         (typed-callbacks event-queue))
          :do (save-call name fn :event-type (car event)))))

(defun cleanup ()
  "If Event-queue gets stopped this function will be called, it will
handle all left events and then return"
  (let ((event-queue (lodds:get-subsystem :event-queue)))
    (when event-queue
      (let ((q (queue event-queue)))
        (loop :while (not (lparallel.queue:queue-empty-p q))
              :do (handle-event (lparallel.queue:pop-queue q)
                                event-queue))))))

(defun run ()
  "init function for Event-Queue subsystem, will run until stopped."
  (loop :while (lodds.subsystem:alive-p (lodds:get-subsystem :event-queue))
        :do (let ((event-queue (lodds:get-subsystem :event-queue)))
              (if event-queue
                  (handle-event (lparallel.queue:pop-queue (queue event-queue))
                                event-queue)
                  (error "Event-Queue is nil!")))
        :finally (cleanup)))
