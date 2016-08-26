;;;; cl-code-low-level-api.lisp

;; TODO: replaced parse-integer with cl-strings:parse-number ?

(in-package #:cl-code-low-level-api)

;; broadcast family

(defun send-advertise (broadcast-host broadcast-port ad-info)
  "'(ip port timestamp name)"
  (destructuring-bind (ip port timestamp name) ad-info
    (let ((sock (usocket:socket-connect nil nil :protocol :datagram))
          (data (flexi-streams:string-to-octets
                 (format nil "~a ~a ~a ~a~%"
                         (usocket:vector-quad-to-dotted-quad ip)
                         port timestamp name))))
      (setf (usocket:socket-option sock :broadcast) t)
      (usocket:socket-send sock data (length data)
                           :host broadcast-host
                           :port broadcast-port)))
  0)

(defun read-advertise (message)
  (destructuring-bind (ip port timestamp . name)
      (cl-strings:split
       (flexi-streams:octets-to-string message))
    (values 0
            (list (usocket:dotted-quad-to-vector-quad ip)
                  (parse-integer port)
                  (parse-integer timestamp)
                  (cl-strings:join name)))))

;; get family

(defun get-file (socket-stream checksum start end)
  (format socket-stream "get file ~a ~a ~a~%" checksum start end)
  0)

(defun get-info-up (socket-stream timestamp)
  (format socket-stream "get info up ~a~%" timestamp)
  0)

(defun get-info-load (socket-stream)
  (format socket-stream "get info load~%")
  0)

(defun get-send-permission (socket-stream size timeout filename)
  (format socket-stream "get send-permission ~a ~a ~a~%"
          size timeout filename)
  0)

;; response family

(defun respond-file (socket-stream file-stream start end)
  (unless (eql start 0)
    (file-position file-stream start))
  (cl-code-core:copy-stream socket-stream file-stream (- end start))
  0)

(defun respond-info-up (socket-stream type timestamp file-infos)
  (format socket-stream "~a ~a ~a~%"
          timestamp
          (if (eql type :all)
              "all"
              "upd")
          (length file-infos))
  (loop
     :for (type checksum size name) :in file-infos
     :do (format socket-stream "~a ~a ~a ~a~%"
                 (if (eql type :add)
                     "add"
                     "del")
                 checksum
                 size
                 name))
  0)

(defun respond-info-load (socket-stream load)
  (format socket-stream "~a~%" load)
  0)

(defun respond-send-permission (socket-stream file-stream size)
  (format socket-stream "OK~%")
  (copy-stream socket-stream file-stream size)
  0)

;; handle family

(defun handle-file (socket-stream file-stream size)
  (copy-stream socket-stream file-stream size)
  0)

(defun handle-info-up (socket-stream)
  (destructuring-bind (type timestamp count)
        (cl-strings:split
         (read-line socket-stream))
    (values 0
            (cond
              ((equalp type "add") :add)
              ((equalp type "del") :del)
              (t (error "TODO: handle-info-up add|del error")))
            (parse-integer timestamp)
            (loop
               :for line = (read-line socket-stream)
               :repeat (parse-integer count)
               :collect (destructuring-bind (type checksum size . name) (cl-strings:split line)
                          (list (cond
                                  ((equalp type "add") :add)
                                  ((equalp type "del") :del)
                                  (t (error "TODO: handle-info-up add|del error")))
                                checksum
                                (parse-integer size)
                                (cl-strings:join name :separator " ")))))))

(defun handle-info-load (socket-stream)
  (values
   0
   (parse-integer (read-line socket-stream))))

(defun handle-send-permission (socket timeout file-stream)
  (if (usocket:wait-for-input socket :timeout timeout)
      (copy-stream file-stream
                   (usocket:socket-stream socket))
      ;; TODO: error code
      -1))
