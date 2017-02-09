(in-package #:lodds.advertiser)

(defun try-send ()
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (labels ((send ()
             (let ((interface (lodds:interface lodds:*server*)))
               (unless interface
                 (return-from send 8))
               (let ((broadcast-address (lodds:get-broadcast-address interface)))
                 (unless broadcast-address
                   (return-from send 7))
                 (lodds.low-level-api:send-advertise
                  broadcast-address
                  (lodds:broadcast-port lodds:*server*)
                  (list (lodds:get-ip-address interface)
                        (lodds:handler-port lodds:*server*)
                        (lodds:get-timestamp-last-change)
                        (lodds:current-load lodds:*server*)
                        (lodds:name lodds:*server*)))))))
    (let ((result (send)))
      (restart-case
          (case result
            ;; TODO: real error handling
            (8 (error "Interface not set"))
            ;; TODO: real error handling
            (7 (error "Could not read Broadcast Address"))
            ;; TODO: real error handling
            (6 (error "Network unreachable"))
            (0 (lodds.event:push-event :advertiser (list :sent)))
            (t (error
                "Unknown error occured in low-level-api (~a) on ADVERTISER"
                result)))
        (retry-sending-advertise ()
          (try-send))
        (stop-advertising ()
          nil)))))

(defun run ()
  (loop :while (lodds.subsystem:alive-p (lodds:get-subsystem :advertiser))
        :do (progn
              (try-send) ;; repull timeout to get changes
              (sleep (lodds:advertise-timeout lodds:*server*)))))
