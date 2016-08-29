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
