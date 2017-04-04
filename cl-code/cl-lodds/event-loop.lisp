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

(defmacro ev-delay-interval (time &body body)
  (let ((fn (gensym "fn")))
    `(labels ((,fn ()
                (as:delay
                  (lambda ()
                    (handler-case
                        (progn
                          ,@body
                          (,fn))
                      (error (e)
                        (event-cb e))))
                  :event-cb #'event-cb
                  :time ,time))
              (event-cb (event)
                (lodds.event:push-event :error
                                        event
                                        ,(format nil "~{~a~}" body))
                (,fn)))
       (,fn))))

(defun ev-send (ip broadcast-ip)
  (unless ip
    (error "Could not get ip"))
  (unless broadcast-ip
    (error "Could not get broadcast-ip"))
  (case (lodds.low-level-api:send-advertise
         broadcast-ip
         (lodds.config:get-value :broadcast-port)
         (list ip
               (lodds.config:get-value :port)
               (lodds:get-timestamp-last-change)
               (lodds:get-load)
               (lodds.config:get-value :name)))
    (6 (error "Network unreachable"))
    (0 nil)))

(defun ev-send-advertise (ip broadcast-ip)
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  (unless (lodds.config:get-value :incognito-mode)
    (let ((err (handler-case (ev-send ip broadcast-ip)
                 (error (e)
                   e))))
      (apply #'lodds.event:push-event
             (if err
                 (list :error
                       (format nil "Could not advertise: ~a" err))
                 (list :advertiser :send))))))

(defun ev-init-advertiser ()
  (let* ((interface (lodds.config:get-value :interface))
         (ip (lodds.core:get-ip-address interface))
         (broadcast-ip (lodds.core:get-broadcast-address interface)))
    (ev-delay-interval (lodds.config:get-value :advertise-timeout)
      (ev-send-advertise ip broadcast-ip))))

(defun ev-init-client-remover ()
  (ev-delay-interval (lodds.config:get-value :client-timeout)
    (lodds:remove-old-clients)))

(defun ev-init-task-cleaner ()
  (ev-delay-interval (lodds.config:get-value :task-cleanup-timeout)
    (lodds.task:tasks-cleanup)))

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
    (ev-init-task-cleaner)
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
    (when (and stop-notifier
               (not (as:notifier-freed-p stop-notifier)))
      (as:trigger-notifier stop-notifier))))
