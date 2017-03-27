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

(defun generate-checksum (pathname)
  "generates sha1 sum out of given pathname, will return a string"
  (handler-case
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-file :sha1 (escape-wildcards pathname)))
    (error (e)
      (declare (ignore e))
      "0000000000000000000000000000000000000000")))

(defun input-rdy-p (socket timeout)
  "returns t if socket has input rdy, nil if it timed out without any
  input"
  (multiple-value-bind (socket-rdy time-left)
      (usocket:wait-for-input socket :timeout timeout)
    (if (and socket-rdy time-left)
        t
        nil)))

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
               (error "Peer closed Socket"))
              ((< read buffer-size)
               (error "Read less then buffer-size"))
              ((> written size)
               (error "Read more than size on COPY-STREAM (should not be possible)"))
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

(defun get-last-slash-position (pathname)
  (position #\/
            pathname
            :from-end t
            :end (1- (length pathname))))

(defun split-directory (directory-path)
  "splits a given directory path into two.

  CL-USER> (split-directory \"/some/long/path\")
  => \"/some/long\" \"/path\"

  returns multiple values, to get both use MULTIPLE-VALUE-BIND:
  CL-USER> (multiple-value-bind (path name)
              (split-directory \"/some/long/path\")
            (list path name))
  => (\"/some/long\" \"/path\")"
  (let ((slash-position (get-last-slash-position directory-path)))
    (values
     (subseq directory-path 0 slash-position)
     (subseq directory-path slash-position))))

(let ((xb (ash 1 53)) ;; 8xb
      (tb (ash 1 43)) ;; 8tb
      (gb (ash 1 33)) ;; 8gb
      (mb (ash 1 23)) ;; 8mb
      (kb (ash 1 13)));; 8kb
  (defun format-size (size)
    "formats given size (number) to a more readable format (string)"
    (cond
      ((> size xb) (format nil "~aPiB" (ash size -50)))
      ((> size tb) (format nil "~aTiB" (ash size -40)))
      ((> size gb) (format nil "~aGiB" (ash size -30)))
      ((> size mb) (format nil "~aMiB" (ash size -20)))
      ((> size kb) (format nil "~aKiB" (ash size -10)))
      (t           (format nil "~aByt" size)))))

(let ((tb    (ash 1 40))  ;; 1 tb
      (gb8   (ash 1 33))  ;; 8 gb
      (gb    (ash 1 30))  ;; 1 gb
      (mb512 (ash 1 29))  ;; 512 mb
      (mb256 (ash 1 28))  ;; 256 mb
      (mb128 (ash 1 27))  ;; 128 mb
      (mb64  (ash 1 26))  ;; 36 mb
      (mb32  (ash 1 25))  ;; 32 mb
      (mb    (ash 1 20))  ;; 1 mb
      (kb    (ash 1 10))) ;; 1 kb
  (defun get-size-color (size)
    (cond
      ((> size tb)    "#FF0000")
      ((> size gb8)   "#FF3300")
      ((> size gb)    "#ff6600")
      ((> size mb512) "#ff9900")
      ((> size mb256) "#FFCC00")
      ((> size mb128) "#FFFF00")
      ((> size mb64)  "#ccff00")
      ((> size mb32)  "#99ff00")
      ((> size mb)    "#66ff00")
      ((> size kb)    "#33ff00")
      (t              "#00FF00"))))

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

(defun add-missing-slash (string)
  "adds a / (slash) if missing"
  (if (cl-strings:ends-with string "/")
      string
      (concatenate 'string string "/")))

(defun remove-newline (string)
  "removes \r\n or \n from given string and returns the result string,
  if not \r\n or \n is found the string is returned untouched"
  (cond
    ((cl-strings:ends-with string
                           (format nil "~C~C" #\return #\linefeed))
     ;; drop \r\n
     (subseq string 0 (- (length string) 2)))
    ((cl-strings:ends-with string
                           (format nil "~C" #\linefeed))
     ;; drop  \n
     (subseq string 0 (- (length string) 1)))
    (t
     ;; just return it
     string)))

(defun format-seconds (seconds)
  (format nil "~2,'0d:~2,'0d"
          (floor seconds 60)
          (mod seconds 60)))

(defun get-interfaces ()
  "returns a list containing names of all up and running interfaces.
   names inside that list can be used to retrieve the broadcast or
   ip-address via 'get-broadcast-address' and 'get-ip-address'"
  (loop :for interface :in (ip-interfaces:get-ip-interfaces-by-flags
                            '(:iff-up :iff-running))
        :unless (equalp #(127 0 0 1)
                        (ip-interfaces:ip-interface-address interface))
        :collect (ip-interfaces:ip-interface-name interface)))

(defun get-interface-info (interface)
  "returns the specified interface, nil if interface was not found"
  (find interface (ip-interfaces:get-ip-interfaces-by-flags
                   '(:iff-up :iff-running))
        :key #'ip-interfaces:ip-interface-name
        :test #'string=))

(defun get-broadcast-address (interface)
  "returns the broadcast address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (let ((info (get-interface-info interface)))
    (when info
      (ip-interfaces:ip-interface-broadcast-address info))))

(defun get-ip-address (interface)
  "returns the ip address of the specified interface.
   to get a list of available interfaces use 'get-interfaces'"
  (let ((info (get-interface-info interface)))
    (when info
      (ip-interfaces:ip-interface-address info))))

(defun escaped-ensure-directories-exist (pathspec &rest args)
  "Calls ensure-directories-exists but pathspec is wrapped in a
escape-wildcards to make sure wildcards are escaped."
  (apply #'ensure-directories-exist
         (cl-fs-watcher:escape-wildcards pathspec)
         args))

(defun escaped-get-folder-name (directory)
  "Returns the folder describted by directory. If given directory is
  root (/) a empty string (\"\") is returned. Given directory _must_
  end with a slash (/), or the last entry will be interpreted as a
  file.

  CL-USER> (get-folder \"/home/someone/somehwere/folder\")
  => \"somewhere/\" ;; 'folder' will be seen as file, since / is
                    ;; missing

  CL-USER> (get-folder \"/home/someone/somehwere/\")
  => \"somewhere/\"

  CL-USER> (get-folder \"/\")
  => \"\""
  (let* ((folders (pathname-directory
                   (cl-fs-watcher:escape-wildcards directory)))
         (folder (car (last folders))))
    (if (> (length folders) 1)
        (lodds.core:add-missing-slash folder)
        "")))

(defun directory-exists (directory)
  "checks if given directory-exists"
  (cl-fs-watcher:escaped-directory-exists-p
   directory))

(defun file-exists (file)
  "checks if given file exists"
  (cl-fs-watcher:escaped-file-exists-p
   file))

(defun get-absolute-path (directory)
  (uiop:native-namestring
   (car (directory (cl-fs-watcher:escape-wildcards directory)))))

(defun set-socket-timeout (socket timeout)
  (setf (usocket:socket-option socket :receive-timeout) timeout
        (usocket:socket-option socket :send-timeout) timeout))
