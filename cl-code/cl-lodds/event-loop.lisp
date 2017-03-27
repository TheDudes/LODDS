#|

TODO: Description

ev- functions/methods are running inside the event-loop, do not call
them from any other thread

|#

;; Event-loop methods to retrieve all kinds of data/information

(in-package #:lodds.event-loop)

(defmacro with-event-loop ((event-loop) &body body)
  `(progn
     (lparallel.queue:push-queue (lambda () ,@body)
                                 (slot-value ,event-loop 'queue))
     (as:trigger-notifier (slot-value ,event-loop 'hook-notifier))))

(defmacro ev-delay-interval (time on-event &body body)
  (let ((fn (gensym "fn")))
    `(labels ((,fn ()
                (as:delay
                  (lambda ()
                    ,@body
                    (,fn))
                  :event-cb
                  (lambda (event)
                    (declare (ignorable event))
                    ,on-event)
                  :time ,time)))
       (,fn))))

(defun ev-send ()
  (let ((interface (lodds.config:get-value :interface)))
    (if (not interface)
        (error "Could not advertise: Interface not set")
        (let ((broadcast-address (lodds.core:get-broadcast-address interface)))
          (if (not broadcast-address)
              (error "Could not get Broadcast Address")
              (case (lodds.low-level-api:send-advertise
                     broadcast-address
                     (lodds.config:get-value :broadcast-port)
                     (list (lodds.core:get-ip-address interface)
                           (lodds.config:get-value :port)
                           (lodds:get-timestamp-last-change)
                           (lodds:get-load)
                           (lodds.config:get-value :name)))
                (6 (error "Network unreachable"))
                (0 nil)))))))

(defun ev-send-advertise ()
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (unless (lodds.config:get-value :incognito-mode)
    (let ((err (handler-case (ev-send)
                 (error (e)
                   e))))
      (apply #'lodds.event:push-event
             (if err
                 (list :error
                       (format nil "Could not advertise: ~a" err))
                 (list :advertiser :send))))))

(defun ev-init-advertiser ()
  (ev-delay-interval (lodds.config:get-value :advertise-timeout)
      (lodds.event:push-event :error
                              "On ev-init-advertiser"
                              event)

    (ev-send-advertise)))

(defun ev-init-client-remover ()
  (ev-delay-interval (lodds.config:get-value :client-timeout)
      (lodds.event:push-event :error
                              "on removing old clients"
                              event)
    (lodds:remove-old-clients)))

(defun ev-init (event-loop)
  "Initializes the event-loop"
  (with-slots (queue stop-notifier hook-notifier) event-loop
    (setf hook-notifier
          (as:make-notifier
           (lambda ()
             (as:delay
               (lambda ()
                 (loop :while (not (lparallel.queue:queue-empty-p queue))
                       :do (funcall (lparallel.queue:pop-queue queue))))))
           :single-shot nil))
    (ev-init-advertiser)
    (ev-init-client-remover)
    (setf stop-notifier (as:make-notifier
                         (lambda ()
                           (lodds.event:push-event :info "ev-loop stopped")
                           (as:exit-event-loop))))))

(defun start (&optional (ev-loop (lodds:get-event-loop)))
  (with-slots (thread alive-p) ev-loop
    (unless alive-p
      (setf thread
            (bt:make-thread
             (lambda ()
               (setf alive-p t)
               (as:start-event-loop
                (lambda () (ev-init ev-loop)))
               (setf alive-p nil))
             :name "Lodds - Event Loop")))))

(defun stop (&optional (ev-loop (lodds:get-event-loop)))
  (with-slots (stop-notifier) ev-loop
    (unless (as:notifier-freed-p stop-notifier)
      (as:trigger-notifier stop-notifier))))
