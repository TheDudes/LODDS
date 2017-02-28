;;;; core.lisp

(in-package #:lodds.core)

;;; "lodds.core" goes here. Hacks and glory await!

(defun generate-checksum (pathname)
  "generates sha1 sum out of given pathname, will return a string"
  (handler-case
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-file :sha1 pathname))
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

(defun copy-stream (stream-from stream-to size &optional check-input-fn)
  "will read from stream-from and write to stream-to size bytes"
  (let* ((written 0)
         (buffer-size (if (< size 4096)
                          size
                          4096))
         (buffer (make-array (list buffer-size)
                             :element-type '(unsigned-byte 8))))
    (loop :for read = (if (or (not check-input-fn)
                              (funcall check-input-fn))
                          (read-sequence buffer stream-from)
                          (return-from copy-stream written))
          :do
          (progn
            (incf written read)
            (cond
              ((= read 0)
               (error "Peer closed Socket"))
              ((< read buffer-size)
               (return-from copy-stream written))
              ((> written size)
               (error "Read more than size on COPY-STREAM (should not be possible)"))
              ((= written size)
               (progn
                 (write-sequence buffer stream-to :end read)
                 (return-from copy-stream written)))
              ((< written size)
               (let ((remaining (- size written)))
                 ;; in case the next read would be bigger then size
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
      ((> size xb) (format nil "~axb" (ash size -50)))
      ((> size tb) (format nil "~atb" (ash size -40)))
      ((> size gb) (format nil "~agb" (ash size -30)))
      ((> size mb) (format nil "~amb" (ash size -20)))
      ((> size kb) (format nil "~akb" (ash size -10)))
      (t           (format nil "~ab " size)))))

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

(defun get-folder (fullpath)
  "Returns the folder describted by fullpath. If given fullpath is
  root (/) a empty string (\"\") is returned. Given fullpath _must_
  end with a slash (/).
  CL-USER> (get-folder \"/home/someone/somehwere/\")
  => \"somewhere/\"

  CL-USER> (get-folder \"/\")
  => \"\""
  (let* ((folders (pathname-directory fullpath))
         (folder (car (last folders))))
    (if (> (length folders) 1)
        (lodds.core:add-missing-slash folder)
        "")))

(defun format-seconds (seconds)
  (format nil "~2,'0d:~2,'0d"
          (floor seconds 60)
          (mod seconds 60)))
