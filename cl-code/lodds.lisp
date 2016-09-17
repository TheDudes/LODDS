;;;; lodds.lisp

(in-package #:lodds)

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop :for interface :in (ip-interfaces:get-ip-interfaces-by-flags
                            '(:iff-up :iff-running))
     :collect (ip-interfaces:ip-interface-name interface)))

(defun get-interface-info (interface)
  "returns the specified interface"
  (find interface (ip-interfaces:get-ip-interfaces-by-flags
                   '(:iff-up :iff-running))
        :key #'ip-interfaces:ip-interface-name
        :test #'string=))

(defun get-broadcast-address (interface)
  "returns the broadcast address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (ip-interfaces:ip-interface-broadcast-address
   (get-interface-info interface)))

(defun get-ip-address (interface)
  "returns the ip address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (ip-interfaces:ip-interface-address
   (get-interface-info interface)))

(defun broadcast-listener (buffer table lock)
  (format t "broadcast: ~a~%" (flexi-streams:octets-to-string buffer))
  (multiple-value-bind (error result) (read-advertise buffer)
    (format t "read-advertise returned with: ~a ~a~%" error result)
    (unless (eql error 0)
      (format t "ERROR in broadcast-listener!!~%"))
    (bt:with-recursive-lock-held (lock)
      (format t "last: ~a, gethash: ~a~%"
              (car (last result))
              (gethash (car (last result)) table))
      (setf (gethash (car (last result)) table)
            (cons (get-timestamp) result))
      ;; TODO: removing this nil is causing a error, i have no idea why :(
      nil)))

(defun start-listening (broadcast-address port)
  "this function will start listening on the given broadcast address,
   it will spawn a seperate thread which updates a hashtable. The
   Hashtable is returned."
  (let ((table (make-hash-table :test #'equalp))
        (lock (bt:make-recursive-lock)))
    (multiple-value-bind (thread socket)
        (usocket:socket-server broadcast-address  port
                               (lambda (buffer)
                                 (broadcast-listener buffer table lock))
                               nil
                               :in-new-thread t
                               :protocol :datagram)
      (values lock
              thread
              socket
              table))))
