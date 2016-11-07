(in-package #:lodds.advertiser)

(defun try-send (server)
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (labels ((send ()
             (let ((interface (stmx:$ (lodds:interface server))))
               (lodds.low-level-api:send-advertise
                (lodds:get-broadcast-address interface)
                (lodds:broadcast-port server)
                (list (lodds:get-ip-address interface)
                      (lodds:handler-port server)
                      (lodds:get-timestamp-last-change server)
                      (stmx:$ (lodds:current-load server))
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

(defun run (subsystem server)
  (loop
     ;; repull timeout to get changes
     (try-send server)
     (sleep (lodds:advertise-timeout server))))
