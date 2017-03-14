#|

This file contains the event-queue implementation. The event-queue
(the class) is a wrapper around lparallel:queue with utility functions
to add/remove callbacks. The idea is simple, a consumer attaches a
callback for an event he is interested in which the event-queue calls
once the event occures.

For example:

lets say a producer puts money into bank accounts and then pushes
:on-money-received events with the account and amount like this:

(push-event :on-money-received "some-account-id" 100)

Then a consumer (who wants to get notified on a :on-money-received
event) attaches a callback like so:
(add-callback :consumer-102
              (lambda (bank-account amount)
                (format t "i just received ~a on account ~a~%" bank-account amount))
              :on-money-received)

and if he wants to remove the callback he calls:
(remove-callback :consumer-102 :on-money-received)

Note: The callback will be called from the event thread, and should
not block too long, since it will block other events

|#

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

(defun add-callback (name fn &optional event-type)
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
        (update-callback name fn (gethash event-type
                                          (typed-callbacks evt-queue)))
        (update-callback name fn (callbacks evt-queue)))))

(defun remove-callback (name &optional event-type)
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

(defun push-event (event-type &rest args)
  "Pushes a given given EVENT of type EVENT-TYPE onto the
  event-queue. EVENT-TYPE is a keyword describing the event, EVENT can
  be anything."
  (lparallel.queue:push-queue (cons event-type args)
                              (queue (lodds:get-subsystem :event-queue))))

(defun handle-event (event event-queue)
  "handles a single event and calls it inside a RESTART-CASE. If a
  error occures its possible to RETRY, IGNORE or REMOVE the callback."
  (labels ((save-call (name cb &optional event-type)
             (restart-case
                 (if event-type
                     (apply cb (cdr event))
                     (funcall cb event))
               (retry-calling-callback ()
                 ;; reload function from hashtable and try again
                 (destructuring-bind (new-name . new-cb)
                     (assoc name
                            (if event-type
                                (gethash event-type
                                         (typed-callbacks event-queue))
                                (callbacks event-queue)))
                   (save-call new-name new-cb event-type)))
               (ignore-callback ()
                 nil)
               (remove-callback ()
                 (remove-callback name event-type)))))
    (loop :for (name . cb) :in (callbacks event-queue)
          :do (save-call name cb))
    (loop :for (name . cb) :in  (gethash (car event)
                                         (typed-callbacks event-queue))
          :do (save-call name cb (car event)))))

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

(defun callback-exists-p (event-type)
  (if (gethash event-type
               (typed-callbacks (lodds:get-subsystem :event-queue)))
      t
      nil))
