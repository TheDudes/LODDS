;;;; cl-code-low-level-api.lisp

;; TODO: replaced parse-integer with cl-strings:parse-number ?

#|

This file contains the low-level-api implementation of _NAME_, functions
are split into three 'families'. The 'get' family will initiate a
_NAME_ based communication by requesting data. The 'respond' family
functions will be the counter-part to them, responding with information.
The information is then read by the 'handle' family functions.

All functions will return a number. If zero no error occured and
everything went fine, on error a error code will be returned.
TODO: Where to lookup error codes?
Some functions return multiple values which can be handle with a
multiple-value-bind.

|#

(multiple-value-bind)
(in-package #:cl-code-low-level-api)

;; broadcast family

(defun send-advertise (broadcast-host broadcast-port ad-info)
  "will format ad-info and write it to a udp-broadcast socket
with given broadcast-host and broadcast-port.
ad-info is a list containing ip port timestamp and name, in that order.
for example: '(#(192 168 2 255) 12345 87654321 \"username\")"
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
  "counter-part to send-advertise, will parse the given message (byte
vector) and return a list out of ip port timestamp and name.
for example: '(#(192 168 2 255) 12345 87654321 \"username\")"
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
  "will format and write a 'get file' request onto socket-stream requesting
the specified (checksum) file's content from start till end"
  (format socket-stream "get file ~a ~a ~a~%" checksum start end)
  0)

(defun get-info-up (socket-stream timestamp)
  "will format and write a 'get info up' request onto socket-stream requesting
a information update. timestamp describes the last requested information the
client currently holds. timestamp should be a fixnum"
  (format socket-stream "get info up ~a~%" timestamp)
  0)

(defun get-info-load (socket-stream)
  "will format and write a 'get-info-load' request onto socket-stream requesting
information about the current load (given in outstanding bytes)
DEPRECATED"
  (format socket-stream "get info load~%")
  0)

(defun get-send-permission (socket-stream size timeout filename)
  "will format and write a 'get-send-permission' request onto socket-stream
requesting send permission. size is a fixnum describing the file size of the
to-be-transfered file. The requested client then has 'timeout' seconds to
respond with either a OK or a connection close. filename is a string containing
the filename."
  (format socket-stream "get send-permission ~a ~a ~a~%"
          size timeout filename)
  0)

;; response family

(defun respond-file (socket-stream file-stream start end)
  "response to a get-file writing data from file-stream to socket-stream.
file-stream will be positioned at start, and only transfer till end."
  (unless (eql start 0)
    (file-position file-stream start))
  (cl-code-core:copy-stream socket-stream file-stream (- end start))
  0)

(defun respond-info-up (socket-stream type timestamp file-infos)
  "response to a 'get info up' request. Will format type timestamp and
file-infos and write it onto socket-stream. type can be either :all or :upt.
timestamp is a fixnum some like a 'revision', describing the current state.
file-infos a list containing lists with type, checksum, size and name describing
all files. type will either be :add or :del and checksum is a sha-256 of the
file's content. size is, as the name suggests, the file size and name the
relative pathname.
TODO: relative pathname link to spec"
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
  "response to a 'get info load' request, writes the given load onto
socket-stream."
  (format socket-stream "~a~%" load)
  0)

(defun respond-send-permission (socket-stream file-stream size)
  "response to a 'get send-permission', will send a OK and copy the
socket-stream content (max size bytes) to file-stream."
  (format socket-stream "OK~%")
  (copy-stream socket-stream file-stream size)
  0)

;; handle family

(defun handle-file (socket-stream file-stream size)
  "handles a successfull 'get file' request and writes the incomming
file content from socket-stream to file-stream. Size describes the
maximum bytes read/written"
  (copy-stream socket-stream file-stream size)
  0)

(defun handle-info-up (socket-stream)
  "handles a successfull 'get info up' request and returns (as second
value) a list containing the parsed data. The list has the same format
as 'file-infos' argument from respond-info-up function"
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
  "handles a successfull 'get info load' request and returns (as second
value) a number describing the parsed load"
  (values
   0
   (parse-integer (read-line socket-stream))))

(defun handle-send-permission (socket timeout file-stream)
  "handles a successfull 'get send-permission' request and waits
maximum 'timeout' seconds for a OK. On success it writes data from
file-stream to socket.
TODO: implement parse of OK."
  (if (usocket:wait-for-input socket :timeout timeout)
      (copy-stream file-stream
                   (usocket:socket-stream socket))
      ;; TODO: error code
      -1))
