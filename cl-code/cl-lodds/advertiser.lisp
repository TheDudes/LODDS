#|

This files contains the advertiser, which job it is to send broadcast
messages so that others know where they can find us and get
information about our load, last change etc.

|#

(in-package #:lodds.advertiser)

(defun try-send ()
  "handles advertisements, will adversite server on broadcast
  network. This function is getting called by START-ADVERTISING. Will
  run inside seperate Thread (spawned by START-ADVERTISING)."
  ;; TODO: this could fail
  (labels ((send ()
             (let ((interface (lodds.config:get-value :interface)))
               (unless interface
                 (return-from send 8))
               (let ((broadcast-address (lodds.core:get-broadcast-address interface)))
                 (unless broadcast-address
                   (return-from send 7))
                 (lodds.low-level-api:send-advertise
                  broadcast-address
                  (lodds.config:get-value :broadcast-port)
                  (list (lodds.core:get-ip-address interface)
                        (lodds.config:get-value :port)
                        (lodds:get-timestamp-last-change)
                        (lodds:get-load)
                        (lodds.config:get-value :name)))))))
    (let ((result (send)))
      (restart-case
          (case result
            ;; TODO: real error handling
            (8 (error "Interface not set"))
            ;; TODO: real error handling
            (7 (error "Could not read Broadcast Address"))
            ;; TODO: real error handling
            (6 (error "Network unreachable"))
            (0 (lodds.event:push-event :advertiser :sent))
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
              (handler-case
                  (try-send)
                (error (e)
                  (lodds.event:push-event :error
                                          (format nil
                                                  "Could not advertise (~a)"
                                                  e))))
              (sleep (lodds.config:get-value :advertise-timeout)))))
