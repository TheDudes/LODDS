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

(defun copy-stream (stream-from stream-to size)
  "will read from stream-from and write to stream-to size bytes"
  (loop :with written = 0
        :with buffer-size = (if (< size 8192)
                                size
                                8192)
        :with buffer = (make-array (list buffer-size)
                                   :element-type '(unsigned-byte 8))
        :for read = (read-sequence buffer stream-from)
        :until (zerop read)
        :do (progn
              (if (>= (+ read written)
                      size)
                  (progn
                    (write-sequence buffer stream-to :end (- size written))
                    ;; TODO: error handling
                    (return))
                  (progn
                    (write-sequence buffer stream-to :end read)
                    (incf written read)
                    ;; in case the next read would be bigger then size
                    (when (> buffer-size (- size written))
                      ;; resize buffer to be fitting
                      (setf buffer-size (- size written))
                      (setf buffer (make-array (list buffer-size)
                                               :element-type '(unsigned-byte 8))))))
              ;; TODO: error handling
              (when (< read buffer-size)
                (return)))))

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
