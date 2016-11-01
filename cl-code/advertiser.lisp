(in-package #:lodds.advertiser)

(define-condition shutdown (error)
  nil)

(defun try-send (server)
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (labels ((send ()
             (let ((interface (lodds:interface server)))
               (lodds.low-level-api:send-advertise
                (lodds:get-broadcast-address interface)
                (lodds:broadcast-port server)
                (list (lodds:get-ip-address interface)
                      (lodds:handler-port server)
                      (lodds:get-timestamp-last-change server)
                      (lodds:current-load server)
                      (lodds:name server))))))
    (case (send)
      ;; TODO: real error handling
      (6 (restart-case (error "Network unreachable")
           (retry-sending-advertise ()
             (try-send server))
           (stop-advertising ()
             nil)))
      (0 t)
      (t (error "Unknown error occured on ADVERTISER")))))

(defun advertiser-loop (server)
  (let ((running t))
    (loop
       :while running
       :do (handler-case
            (progn
              ;; repull timeout to get changes
              (try-send server)
              (sleep (lodds:advertise-timeout server)))
            (shutdown () (setf running nil))
            (error (e)
                   (format t "got error: ~a~%" e)
                   (setf running nil)))))
  (format t "Advertiser stopped!~%")
  (setf (lodds:advertiser server) nil))

(defun start (server)
  (let ((advertiser (lodds:advertiser server)))
    (if advertiser
        (format t "ADVERTISER already running.~%")
        (setf (lodds:advertiser server)
              (bt:make-thread
               (lambda ()
                 (advertiser-loop server))
               :name "Advertiser")))))

(defun stop (server)
  (let ((advertiser (lodds:advertiser server)))
    (if advertiser
        (bt:interrupt-thread advertiser
                             (lambda ()
                               (signal (make-condition 'shutdown))))
        (format t "advertiser not running!~%"))))
