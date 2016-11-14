(in-package #:lodds.advertiser)

(defun try-send ()
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (labels ((send ()
             (let ((interface (lodds:interface lodds:*server*)))
               (lodds.low-level-api:send-advertise
                (lodds:get-broadcast-address interface)
                (lodds:broadcast-port lodds:*server*)
                (list (lodds:get-ip-address interface)
                      (lodds:handler-port lodds:*server*)
                      (lodds:get-timestamp-last-change)
                      (lodds:current-load lodds:*server*)
                      (lodds:name lodds:*server*))))))
    (let ((result (send)))
      (case result
        ;; TODO: real error handling
        (6 (restart-case (error "Network unreachable")
             (retry-sending-advertise ()
               (try-send))
             (stop-advertising ()
               nil)))
        (0 (lodds.event:push-event :advertiser (list :sent)))
        (t (error
            "Unknown error occured in low-level-api (~a) on ADVERTISER"
            result))))))

(defun run ()
  (loop (try-send) ;; repull timeout to get changes
        (sleep (lodds:advertise-timeout lodds:*server*))))
