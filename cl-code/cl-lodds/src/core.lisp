#|

This file contains all kinds of utility helper and wrapper
functions.

|#

(in-package #:lodds.core)

;;; "lodds.core" goes here. Hacks and glory await!

(defmacro with-server (server &body body)
  `(let* ((lodds:*server* ,server)
          (bt:*default-special-bindings*
            (append (list (cons 'lodds:*server* ,server))
                    bt:*default-special-bindings*)))
     ,@body))

(defun escape-wildcards (pathname)
  "escapes pathname wildcards to avoid wildcard error"
  (cl-fs-watcher:escape-wildcards pathname))

(defun ensure-directory-pathname (directory)
  (let ((pathname
          (uiop:ensure-directory-pathname
           (etypecase directory
             (pathname directory)
             (string
              (escape-wildcards
               (if (eql (aref directory (- (length directory) 1))
                        (uiop:directory-separator-for-host))
                   directory
                   (concatenate 'string
                                directory
                                (make-string 1 :initial-element
                                             (uiop:directory-separator-for-host))))))))))
    ;; expand :home, we can get the actual path from
    ;; (user-homedir-pathname)
    (if (eql (cadr (pathname-directory pathname)) :home)
        (make-pathname :directory
                       (append (pathname-directory (user-homedir-pathname))
                               (cddr (pathname-directory pathname)))
                       :name nil
                       :type nil
                       :defaults pathname)
        pathname)))

(defun generate-fake-checksum (&optional (length 40))
  (declare (optimize (debug 0) (safety 0)))
  (make-array length
              :element-type 'character
              :initial-contents
              (loop :repeat length
                    :collect (digit-char (random 16) 16))))

(defun generate-checksum (pathname)
  "generates sha1 sum out of given pathname, will return a string or
nil on error"
  (handler-case
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-file :sha1 pathname))
    (error (e)
      (declare (ignore e))
      nil)))

(defun get-file-size (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :if-does-not-exist nil
                          :element-type '(unsigned-byte 8))
    (if stream
        (file-length stream)
        0)))

(defun format-timestamp (&optional unix-timestamp)
  "Returns current date formatted as a string. if unix-timestamp is
  given it formats that.
  CL-USER> (generate-timestamp)
  => \"2017-02-28 13:02:24\"
  CL-USER> (lodds.core:format-timestamp (lodds.core:get-timestamp))
  => \"2017-02-28 13:02:59\""
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (if unix-timestamp
          (decode-universal-time
           (+ lodds.core::*unix-epoch-difference*
              unix-timestamp))
          (get-decoded-time))
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            yr mon day hr min sec) ))

(defun format-checksum (checksum)
  (when checksum
    (with-output-to-string (stream)
      (write-string (subseq checksum 0 7) stream)
      (write-string "..." stream))))

(defun format-pathname (pathname)
  (when pathname
    (with-output-to-string (stream)
      (write-string
       (if (> (length (pathname-directory pathname)) 1)
           (namestring (make-pathname
                        :directory (list :relative
                                         "..."
                                         (car
                                          (last
                                           (pathname-directory pathname))))
                        :defaults pathname))
           (file-namestring pathname))
       stream))))

(defun input-rdy-p (socket timeout)
  "returns t if socket has input rdy, nil if it timed out without any
  input"
  (multiple-value-bind (socket-rdy time-left)
      (usocket:wait-for-input socket :timeout timeout)
    (if (and socket-rdy time-left)
        t
        nil)))

(define-condition could-not-read-error (simple-error) ())
(define-condition out-of-data-error (simple-error) ())
(define-condition read-too-much-error (simple-error) ())

(defun copy-stream (stream-from stream-to size &optional digester)
  "will read from stream-from and write to stream-to size bytes"
  (when (= size 0)
    (return-from copy-stream 0))
  (let* ((written 0)
         (buffer-size (if (< size 4096)
                          size
                          4096))
         (buffer (make-array (list buffer-size)
                             :element-type '(unsigned-byte 8))))
    (loop :for read = (read-sequence buffer stream-from)
          :do
          (progn
            (incf written read)
            (cond
              ((= read 0)
               (error 'could-not-read-error
                      :format-control "Peer closed Socket"))
              ((< read buffer-size)
               (error 'out-of-data-error
                      :format-control
                      "Read less then buffer-size"))
              ((> written size)
               (error 'read-too-much-error
                      :format-control
                      "Read more than size on COPY-STREAM (should not be possible)"))
              ((= written size)
               (progn
                 (when digester
                   (ironclad:update-digest digester buffer :end read))
                 (write-sequence buffer stream-to :end read)
                 (return-from copy-stream written)))
              ((< written size)
               (let ((remaining (- size written)))
                 ;; in case the next read would be bigger then size
                 (when digester
                   (ironclad:update-digest digester buffer :end read))
                 (write-sequence buffer stream-to :end read)
                 (when (> buffer-size remaining)
                   ;; resize buffer to be fitting
                   (setf buffer-size remaining)
                   (setf buffer
                         (make-array (list buffer-size)
                                     :element-type '(unsigned-byte 8)))))))))))

;; got code from lisptips http://lisptips.com/page/4 modified it to my needs :)

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0)
  "used by get-timestamp to convert universal-time to unix")

(defun get-timestamp ()
  "returns a number representing the unix-timestamp"
  (- (get-universal-time) *unix-epoch-difference*))

(defmacro str-case (string-form &body cases)
  "'case' for strings, expands to cond clause with string=
   as compare function"
  (let ((result (gensym "result")))
    `(let ((,result ,string-form))
       (declare (ignorable ,result))
       (cond
         ,@(loop :for (str form) :in cases
                 :collect `((string= ,result ,str) ,form))))))

(let ((xb (ash 1 53)) ;; 8xb
      (tb (ash 1 43)) ;; 8tb
      (gb (ash 1 33)) ;; 8gb
      (mb (ash 1 23)) ;; 8mb
      (kb (ash 1 13)));; 8kb
  (defun format-size (size &optional (when-zero nil when-zero-given?))
    "formats given size (number) to a more readable format (string),
    when-zero can be given to return it instead of \"0Byt\""
    (if (and (eql 0 size)
             when-zero-given?)
        when-zero
        (cond
          ((> size xb) (format nil "~aPiB" (ash size -50)))
          ((> size tb) (format nil "~aTiB" (ash size -40)))
          ((> size gb) (format nil "~aGiB" (ash size -30)))
          ((> size mb) (format nil "~aMiB" (ash size -20)))
          ((> size kb) (format nil "~aKiB" (ash size -10)))
          (t           (format nil "~aByt" size))))))

(defun red-yellow-green-gradient-generator (count)
  (let ((red 255)
        (green 0)
        (step-size (/ 255 (/ count 2))))
    (flet ((fmt (red green)
             (format nil "#~2,'0X~2,'0X00" (round red) (round green))))
      (reverse
       (append
        (loop :while (< green 255)
              :do (incf green step-size)
              :when (> green 255)
              :do (setf green 255)
              :collect (fmt red green))
        (loop :while (> red 0)
              :do (decf red step-size)
              :when (< red 0)
              :do (setf red 0)
              :collect (fmt red green)))))))

(let ((lookup (make-array '(42)
                          :initial-contents (red-yellow-green-gradient-generator 42)
                          :adjustable nil)))
  (defun get-size-color (size)
    (let ((spot (integer-length size)))
      (if (> spot 41)
          (aref lookup 41)
          (aref lookup spot)))))

(defmacro split-user-identifier ((name ip port &optional (convert-types nil)) user &body body)
  (let ((ip+port (gensym "ip+port")))
    `(destructuring-bind (,name ,ip+port) (cl-strings:split ,user #\@)
       (declare (ignorable ,name))
       (destructuring-bind (,ip ,port) (cl-strings:split ,ip+port #\:)
         (declare (ignorable ,ip ,port))
         ,(if convert-types
              `(let ((,ip (usocket:dotted-quad-to-vector-quad ,ip))
                     (,port (parse-integer ,port)))
                 ,@body)
              `(progn
                 ,@body))))))

(defun split-path (path)
  "splits a path string into its directorires
  CL-USER: (split-path \"d4ryus@192.168.2.1.1:1234/some/shared/file.txt)
  => (\"d4ryus@192.168.2.1.1:1234/\" \"some/\" \"shared/\" \"file.txt\")"
  (remove-if (lambda (element) (eql 0 (length element)))
             (maplist
              (lambda (path)
                (if (cdr path)
                    (concatenate 'string (car path) "/")
                    (car path)))
              (cl-strings:split path #\/))))

(defun ensure-trailing-slash (string)
  "adds a / (slash) if missing"
  (if (equal #\/ (char string (- (length string) 1)))
      string
      (concatenate 'string string "/")))

(defun format-seconds (seconds)
  (format nil "~2,'0d:~2,'0d"
          (floor seconds 60)
          (mod seconds 60)))

(defun list-of-interfaces ()
  #-os-windows (ip-interfaces:get-ip-interfaces-by-flags
                '(:iff-up :iff-running))
  #+os-windows (ip-interfaces:get-ip-interfaces))

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop :for interface :in (list-of-interfaces)
        :unless (equalp #(127 0 0)
                        (subseq (ip-interfaces:ip-interface-address interface)
                                0
                                3))
        :collect (ip-interfaces:ip-interface-name interface)))

(defun get-interface-info (interface)
  "returns the specified interface, nil if interface was not found"
  (find interface (list-of-interfaces)
        :key #'ip-interfaces:ip-interface-name
        :test #'string=))

(defun get-broadcast-address (interface)
  "returns the broadcast address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (declare (ignorable interface))
  (let ((info (get-interface-info interface)))
    (when info
      #-os-windows (ip-interfaces:ip-interface-broadcast-address info)
      #+os-windows (map 'vector #'logior
                        (ip-interfaces:ip-interface-address info)
                        (map 'vector (lambda (bits)
                                       (logxor bits 255))
                             (ip-interfaces:ip-interface-netmask info))))))

(defun get-ip-address (interface)
  "returns the ip address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (let ((info (get-interface-info interface)))
    (when info
      (ip-interfaces:ip-interface-address info))))

(defun directory-exists (directory)
  "checks if given directory-exists"
  (cl-fs-watcher:escaped-directory-exists-p
   directory))

(defun file-exists (file)
  "checks if given file exists"
  (cl-fs-watcher:escaped-file-exists-p
   file))

(defun set-socket-timeout (socket timeout)
  (setf (usocket:socket-option socket :receive-timeout) timeout
        (usocket:socket-option socket :send-timeout) timeout))

(defun string-to-octets (string)
  (let ((flex:*default-eol-style* :LF))
    (flexi-streams:string-to-octets string
                                    :external-format :utf-8)))

(defun octets-to-string (octets)
  (let ((flex:*default-eol-style* :LF))
    (flexi-streams:octets-to-string octets
                                    :external-format :utf-8)))
