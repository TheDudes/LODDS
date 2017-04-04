#|

This file contains all kinds of helper functions which are used
throughout the gui code

|#

(in-package #:lodds-qt)
(in-readtable :qtools)

(defmacro qdoto (instance &rest forms)
  (let ((inst (gensym "instance")))
    `(let ((,inst ,instance))
       ,@(loop :for (fn . arguments) :in forms
               :collect `(,fn ,(car arguments) ,inst ,@ (cdr arguments)))
       ,inst)))

(defun generate-timestamp (&optional unix-timestamp)
  "Returns current date formatted as a string. if unix-timestamp is
  given it formats that
  CL-USER> (generate-timestamp)
  => \"2017-02-28 13:02:24\"
  CL-USER> generate-timestamp (lodds.core:get-timestamp))
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

;; iterates over all children (QTreeWidgetItem) of a given parent
;; (QTreeWidgetItem) and binds the current child index to index and
;; the current child to child.
(defmacro do-childs ((child index parent) &body body)
  `(loop :for ,index :from 0 :below (q+:child-count ,parent)
         :collect (let ((,child (q+:child ,parent ,index)))
                    ,@body)))

(defun set-icon (widget icon-name fallback-text &optional (set-flat t) (supress-warning nil))
  "calls q+:set-icon on given widget, when the icon was found inside
the :resources-folder. If not q+:set-text will be used to set the
fallback-text. If set-flat is true (q+:set-flat widget t) will be
called, when the icon is found. Will also print a warning on stdout
and push a :info event if the icon is not found, set
supress-warning if you dont want that."
  (let ((icon-path (format nil
                           "~a~a"
                           (uiop:native-namestring
                            (lodds.config:get-value :resources-folder))
                           icon-name)))
    (if (lodds.core:file-exists icon-path)
        (with-finalizing* ((pixmap (q+:make-qpixmap icon-path))
                           (icon (q+:make-qicon pixmap)))
          (q+:set-icon widget icon)
          (when set-flat
            (q+:set-flat widget t)))
        (progn
          (unless supress-warning
            (let ((text (format nil "Could not find icon on ~a, falling back to text"
                                icon-path)))
              (lodds.event:push-event :info text)
              (format t "~a~%" text)))
          (q+:set-text widget fallback-text)))))
