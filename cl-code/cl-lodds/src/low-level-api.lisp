#|

This file contains the low-level-api implementation of the LODDS protocol,
functions are split into three 'families'. The 'get' family will initiate a
LODDS based communication by requesting data. The 'respond' family functions
will be the counter-part to them, responding with information. The information
is then read by the 'handle' family functions.

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
   "^get ((file ([a-f]|[A-F]|[0-9]){40} \\d+ \\d+)|(info \\d+)|(send-permission \\d+ \\d+ [^\\n]+))$")
  "used to scan get requests to check if they are correct")

(defvar *info-head-scanner*
  (cl-ppcre:create-scanner
   ;; type timestamp count
   "^(upd|all) \\d+ \\d+$")
  "used to scan get info head to check if they are correct")

(defvar *info-body-scanner*
  (cl-ppcre:create-scanner
   ;; type checksum size realtive-filename
   "^(add|del) ([a-f]|[A-F]|[0-9]){40} \\d+ [^\\n]+$")
  "used to scan get info body to check if they are correct")

(define-condition malformed-data (error) ())

(define-condition malformed-advertise (malformed-data) ())
(define-condition malformed-get (malformed-data) ())
(define-condition malformed-info-head (malformed-data) ())
(define-condition malformed-info-body (malformed-data) ())

(define-condition timeout-reached (error) ())

(defun write-string-to-stream (stream string &optional (flush-p t))
  "converts the given string to octets and writes it to the given
stream, will flush the stream with force-output when flusp-p is t"
  (write-sequence (lodds.core:string-to-octets string)
                  stream)
  (when flush-p
    (force-output stream)))

(defun read-line-from-stream (stream &optional (eof-error-p t) eof-value)
  "read a single line from given stream of type '(unsigned-byte 8).
The returned string (utf-8) won't contain the newline character. If
eof-error-p is nil, the line until eof will be returned, or eof-value
if eof occured on first read-byte"
  (let ((line nil)
        (byte nil))
    (loop
      (setf byte (read-byte stream eof-error-p eof-value))
      (if (or (and (not eof-error-p)
                   (eql eof-value byte))
              (eql 10 byte))
          (return-from read-line-from-stream
            (if line
                (lodds.core:octets-to-string line)
                eof-value))
          (vector-push-extend byte
                              (or line
                                  (setf line
                                        (make-array 0
                                                    :element-type '(unsigned-byte 8)
                                                    :adjustable t
                                                    :fill-pointer t))))))))

;; broadcast family

(defun format-send-advertise (ad-info)
  (destructuring-bind (ip port timestamp load name) ad-info
    (format nil "~a@~a:~a ~a ~a~C"
            name
            (usocket:vector-quad-to-dotted-quad ip)
            port
            (or timestamp 0)
            load
            #\linefeed)))

(defun send-advertise (broadcast-host broadcast-port ad-info)
  "will format ad-info and write it to a udp-broadcast socket
   with given broadcast-host and broadcast-port.
   ad-info is a list containing ip, port, timestamp, load and name,
   in that order. for example:
   '(#(192 168 2 255) 12345 87654321 9999 \"username\")"
  (let ((sock (usocket:socket-connect nil nil :protocol :datagram))
        (data (lodds.core:string-to-octets
               (format-send-advertise ad-info))))
    (setf (usocket:socket-option sock :broadcast) t)
    (usocket:socket-send sock data (length data)
                         :host broadcast-host
                         :port broadcast-port)
    (usocket:socket-close sock)))

(defun read-advertise (message)
  "counter-part to send-advertise, will parse the given message (byte
   vector) and return a list out of ip, port, timestamp, load and name.
   for example: '(#(192 168 2 255) 12345 9999 87654321 \"username\")
   will use *advertise-scanner* to check for syntax errors and signal
   a 'malformed-advertise error on mismatch"
  (unless (cl-ppcre:scan *advertise-scanner* message)
    (error 'malformed-advertise))
  (destructuring-bind (user timestamp load)
      (cl-strings:split message)
    (lodds.core:split-user-identifier (name ip port) user
      (list ip
            (parse-integer port)
            (parse-integer timestamp)
            (parse-integer load)
            user))))

(defun parse-request (stream)
  "parses a direct communication request. returns one of the following
  lists, depending on request:
   (:file checksum start end)
   (:info timestamp)
   (:send-permission size timeout filename)
   will use *get-scanner* to to check for syntax errors and might
   signal 'malformed-get error in case the scanner does not match"
  (let ((line (read-line-from-stream stream)))
    (unless (cl-ppcre:scan *get-scanner* line)
      (error 'malformed-get))
    (destructuring-bind (get type . args)
        (cl-strings:split line)
      (declare (ignore get))
      (let ((req-type (lodds.core:str-case type
                         ("file" :file)
                         ("info" :info)
                         ("send-permission" :send-permission))))
        (cons req-type
              (case req-type
                (:file (list (car args)
                             (parse-integer (nth 1 args))
                             (parse-integer (nth 2 args))))
                (:info (list (parse-integer (car args))))
                (:send-permission (destructuring-bind (size timeout . filename)
                                      args
                                    (list (parse-integer size)
                                          (parse-integer timeout)
                                          (cl-strings:join filename :separator " "))))))))))

;; get family
(defun format-get-file (checksum start end)
  (format nil "get file ~a ~a ~a~C" checksum start end #\linefeed))

(defun get-file (stream checksum start end)
  "will format and write a 'get file' request onto stream requesting
   the specified (checksum) file's content from start till end"
  (write-string-to-stream
   stream
   (format-get-file checksum start end)))

(defun format-get-info (timestamp)
  (format nil "get info ~a~C" timestamp #\linefeed))

(defun get-info (stream timestamp)
  "will format and write a 'get info' request onto stream requesting
   information about shared files. timestamp describes the last requested
   information the user currently holds. If timestamp is zero (0) it will
   request a full list of shared files from the user."
  (write-string-to-stream
   stream
   (format-get-info timestamp)))

(defun format-get-send-permission (size timeout filename)
  (format nil "get send-permission ~a ~a ~a~C"
          size
          timeout
          filename
          #\linefeed))

(defun get-send-permission (stream size timeout filename)
  "will format and write a 'get-send-permission' request onto stream
   requesting send permission. size is a fixnum describing the file size of the
   to-be-transfered file. The requested user then has 'timeout' seconds to
   respond with either a OK or a connection close. filename is a string containing
   the filename."
  (write-string-to-stream
   stream
   (format-get-send-permission size
                               timeout
                               filename)))

;; response family

(defparameter *head-fail* nil)
(defparameter *body-fail* nil)

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
                      (ecase type
                        (:add "add")
                        (:del "del"))
                      checksum
                      size
                      name
                      #\linefeed))))

(defun respond-info (stream type timestamp file-infos)
  "response to a 'get info' request. Will format type timestamp and
   file-infos and write it onto stream. type can be either :all or :upt.
   timestamp is a fixnum somewhat like a 'revision', describing the current state.
   file-infos a list containing lists with type, checksum, size and name describing
   all files. type will either be :add or :del and checksum is a sha1 of the
   file's content. size is, as the name suggests, the file size. name is the
   relative pathname.
   TODO: relative pathname link to spec"
  (write-string-to-stream
   stream
   (format-respond-info type timestamp file-infos)))

(defun format-respond-send-permission ()
  (format nil "OK~C" #\linefeed))

(defun respond-send-permission (stream)
  "response to a 'get send-permission', will send a OK and copy the
   stream content (max size bytes) to file-stream."
  (write-string-to-stream
   stream
   (format-respond-send-permission)))

;; handle family

(defun handle-info (stream)
  "handles a successfull 'get info' request and returns a list
  containing the parsed data. The list has the same format as
  'file-infos' argument from respond-info function"
  (let ((line (read-line-from-stream stream)))
    (unless (cl-ppcre:scan *info-head-scanner* line)
      (error 'malformed-info-head))
    (destructuring-bind (type timestamp count) (cl-strings:split line)
      (list (cond
              ((equal type "all") :all)
              ((equal type "upd") :upd))
            (parse-integer timestamp)
            (loop :repeat (parse-integer count)
                  :collect
                  (progn
                    (unless (cl-ppcre:scan *info-body-scanner*
                                           (setf line (read-line-from-stream stream)))
                      (error 'malformed-info-body))
                    (destructuring-bind (type checksum size . name)
                        (cl-strings:split line)
                      (list (cond
                              ((equal type "add") :add)
                              ((equal type "del") :del))
                            checksum
                            (parse-integer size)
                            (cl-strings:join name :separator " ")))))))))

(defun handle-send-permission (socket timeout)
  "handles a successfull 'get send-permission' request and waits
   maximum 'timeout' seconds for a OK. On error a 'malformed-data or a
   'timeout-reached error will be signaled"
  (if (lodds.core:input-rdy-p socket timeout)
      (unless (string= "OK" (read-line-from-stream (usocket:socket-stream socket)))
        (error 'malformed-data))
      (error 'timeout-reached)))
