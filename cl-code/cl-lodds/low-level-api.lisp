#|

This file contains the low-level-api implementation of the LODDS protocol,
functions are split into three 'families'. The 'get' family will initiate a
LODDS based communication by requesting data. The 'respond' family functions
will be the counter-part to them, responding with information. The information
is then read by the 'handle' family functions.

All functions will return a number. If zero no error occured and
everything went fine, on error a error code will be returned.
TODO: Where to lookup error codes?
Some functions return multiple values which can be handle with a
multiple-value-bind.

|#

(in-package #:lodds.low-level-api)

(defvar *advertise-scanner*
  (cl-ppcre:create-scanner
   ;; name@ip:port timestamp load
   "^([a-z]|[A-Z]|[0-9]|_|-)+@(\\d{1,3}\\.){3}\\d{1,3}:\\d{1,5} \\d+ \\d+\\n$")
  "used to scan advertise strings to check if they are correct")

(defvar *get-scanner*
  (cl-ppcre:create-scanner
   ;; expects one of:
   ;; get file checksum start end
   ;; get info timestamp
   ;; get send-permission size timeout filename
   "^get ((file [^\\s]{40} \\d+ \\d+)|(info \\d+)|(send-permission \\d+ \\d+ [^\\n]+))$")
  "used to scan get requests to check if they are correct")

(defvar *info-head-scanner*
  (cl-ppcre:create-scanner
   ;; type timestamp count
   "^(upd|all) \\d+ \\d+$")
  "used to scan get info head to check if they are correct")

(defvar *info-body-scanner*
  (cl-ppcre:create-scanner
   ;; type checksum size realtive-filename
   "^(add|del) [^\\s]{40} \\d+ [^\\n]+$")
  "used to scan get info body to check if they are correct")

(defun write-to-stream (stream string &optional (flush-p t))
  "converts the given strin gto octets and writes it to the given
stream, will flush the stream with force-output when flusp-p is t"
  (write-sequence (flex:string-to-octets string)
                  stream)
  (when flush-p
    (force-output stream)))

(defun read-line-from-socket (socket)
  "read-line for socket-stream of type '(unsigned-byte 8)"
  (let ((line (list))
        (byte nil)
        (stream (usocket:socket-stream socket)))
    (loop :until nil
          :do (progn
                (setf byte (read-byte stream))
                (if (eql 10 byte)
                    (return-from read-line-from-socket
                      (map 'string #'code-char (reverse line)))
                    (push byte line))))))

;; broadcast family

(defun format-send-advertise (ad-info)
  (destructuring-bind (ip port timestamp load name) ad-info
    (format nil "~a@~a:~a ~a ~a~C"
            name
            (usocket:vector-quad-to-dotted-quad ip)
            port
            (if timestamp
                timestamp
                0)
            load
            #\linefeed)))

(defun send-advertise (broadcast-host broadcast-port ad-info)
  "will format ad-info and write it to a udp-broadcast socket
   with given broadcast-host and broadcast-port.
   ad-info is a list containing ip, port, timestamp, load and name,
   in that order. for example:
   '(#(192 168 2 255) 12345 87654321 9999 \"username\")"
  (let ((sock (usocket:socket-connect nil nil :protocol :datagram))
        (data (flexi-streams:string-to-octets
               (format-send-advertise ad-info))))
    (handler-case
        (progn
          (setf (usocket:socket-option sock :broadcast) t)
          (usocket:socket-send sock data (length data)
                               :host broadcast-host
                               :port broadcast-port)
          (usocket:socket-close sock)
          0) ;; everything went fine
      (usocket:network-unreachable-error ()
        6)))) ;; network-unreachable-error

(defun read-advertise (message)
  "counter-part to send-advertise, will parse the given message (byte
   vector) and return a list out of ip, port, timestamp, load and name.
   for example: '(#(192 168 2 255) 12345 9999 87654321 \"username\")
   will use *advertise-scanner* to check for syntax errors."
  (if (cl-ppcre:scan *advertise-scanner* message)
      (destructuring-bind (user timestamp load)
          (cl-strings:split message)
        (lodds.core:split-user-identifier (name ip port) user
          (values 0
                  (list ip
                        (parse-integer port)
                        (parse-integer timestamp)
                        (parse-integer load)
                        user))))
      2))

(defun parse-request (input)
  "parses a direct communication request. returns multiple values,
   the first is a number describing the error (or 0 on success) and
   one of the following lists, depending on request:
   (:file checksum start end)
   (:info timestamp)
   (:send-permission size timeout filename)
   will use *get-scanner* to to check for syntax errors
   INPUT can be string, octets or a usocket"
  (let ((line (ctypecase input
                (usocket:stream-usocket (read-line-from-socket input))
                (vector (flex:octets-to-string (subseq input 0 (- (length input) 1))
                                               :external-format :utf8)))))
    (unless (cl-ppcre:scan *get-scanner* line)
      (return-from parse-request 2))
    (destructuring-bind (get type . args)
        (cl-strings:split line)
      (declare (ignore get))
      (let ((requ-type (str-case type
                         ("file" :file)
                         ("info" :info)
                         ("send-permission" :send-permission))))
        (case requ-type
          (:file (values 0
                         (list :file
                               (car args)
                               (parse-integer (nth 1 args))
                               (parse-integer (nth 2 args)))))
          (:info (values 0
                         (list :info
                               (parse-integer (car args)))))
          (:send-permission (values 0
                                    (destructuring-bind (size timeout . filename)
                                        args
                                      (list :send-permission
                                            (parse-integer size)
                                            (parse-integer timeout)
                                            (cl-strings:join filename :separator " "))))))))))

;; get family
(defun format-get-file (checksum start end)
  (format nil "get file ~a ~a ~a~C" checksum start end #\linefeed))

(defun get-file (socket-stream checksum start end)
  "will format and write a 'get file' request onto socket-stream requesting
   the specified (checksum) file's content from start till end"
  (write-to-stream socket-stream (format-get-file checksum start end))
  0)

(defun format-get-info (timestamp)
  (format nil "get info ~a~C" timestamp #\linefeed))

(defun get-info (socket-stream timestamp)
  "will format and write a 'get info' request onto socket-stream requesting
   information about shared files. timestamp describes the last requested
   information the client currently holds. If timestamp is zero (0) it will
   request a full list of shared files from the client."
  (write-to-stream socket-stream (format-get-info timestamp))
  0)

(defun format-get-send-permission (size timeout filename)
  (format nil "get send-permission ~a ~a ~a~C"
          size
          timeout
          filename
          #\linefeed))

(defun get-send-permission (socket-stream size timeout filename)
  "will format and write a 'get-send-permission' request onto socket-stream
   requesting send permission. size is a fixnum describing the file size of the
   to-be-transfered file. The requested client then has 'timeout' seconds to
   respond with either a OK or a connection close. filename is a string containing
   the filename."
  (write-to-stream socket-stream
                   (format-get-send-permission size
                                               timeout
                                               filename))
  0)

;; response family

(defun format-respond-info (type timestamp file-infos)
  (with-output-to-string (stream)
    (format stream "~a ~a ~a~C"
            (if (eql type :all)
                "all"
                "upd")
            timestamp
            (length file-infos)
            #\linefeed)
    (loop :for (type checksum size name) :in file-infos
          :do (format stream "~a ~a ~a ~a~C"
                      (if (eql type :add)
                          "add"
                          "del")
                      checksum
                      size
                      name
                      #\linefeed))))

(defun respond-info (socket-stream type timestamp file-infos)
  "response to a 'get info' request. Will format type timestamp and
   file-infos and write it onto socket-stream. type can be either :all or :upt.
   timestamp is a fixnum somewhat like a 'revision', describing the current state.
   file-infos a list containing lists with type, checksum, size and name describing
   all files. type will either be :add or :del and checksum is a sha1 of the
   file's content. size is, as the name suggests, the file size. name is the
   relative pathname.
   TODO: relative pathname link to spec"
  (write-to-stream socket-stream
                   (format-respond-info type timestamp file-infos))
  0)

(defun format-respond-send-permission ()
  (format nil "OK~C" #\linefeed))

(defun respond-send-permission (socket-stream)
  "response to a 'get send-permission', will send a OK and copy the
   socket-stream content (max size bytes) to file-stream."
  (write-to-stream socket-stream
                   (format-respond-send-permission))
  0)

;; handle family

(defun handle-info (socket)
  "handles a successfull 'get info' request and returns (as second
   value) a list containing the parsed data. The list has the same format
   as 'file-infos' argument from respond-info function"
  (let ((line (read-line-from-socket socket)))
    (if (cl-ppcre:scan *info-head-scanner* line)
        (destructuring-bind (type timestamp count) (cl-strings:split line)
          (values 0
                  (list
                   (cond
                     ((equal type "all") :all)
                     ((equal type "upd") :upd)
                     (t (error "TODO: handle-info all|upd error ~a" type)))
                   (parse-integer timestamp)
                   (loop :repeat (parse-integer count)
                         :collect
                         (progn
                           (setf line (read-line-from-socket socket))
                           (if (cl-ppcre:scan *info-body-scanner* line)
                               (destructuring-bind (type checksum size . name)
                                   (cl-strings:split line)
                                 (list (cond
                                         ((equal type "add") :add)
                                         ((equal type "del") :del)
                                         (t (error "TODO: handle-info add|del error")))
                                       checksum
                                       (parse-integer size)
                                       (cl-strings:join name :separator " ")))
                               (return-from handle-info 2)))))))
        2)))

(defun handle-send-permission (socket timeout)
  "handles a successfull 'get send-permission' request and waits
   maximum 'timeout' seconds for a OK. On success it returns with 0
   and the socket should be rdy to send the file. If timeout is
   reached 5 will be returned"
  (handler-case
      (if (lodds.core:input-rdy-p socket timeout)
          (if (string= "OK"
                       (read-line-from-socket socket))
              0
              2)
          3)
    (end-of-file (e)
      (declare (ignore e))
      1)))
