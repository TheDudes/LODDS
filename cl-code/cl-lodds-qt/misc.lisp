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
  (let ((pathname (make-pathname
                   :name icon-name
                   :defaults (lodds.config:get-value :resources-folder))))
    (if (lodds.core:file-exists pathname)
        (with-finalizing* ((pixmap (q+:make-qpixmap
                                    (uiop:native-namestring pathname)))
                           (icon (q+:make-qicon pixmap)))
          (q+:set-icon widget icon)
          (when set-flat
            (q+:set-flat widget t)))
        (progn
          (unless supress-warning
            (let ((text (format nil "Could not find icon on ~a, falling back to text"
                                pathname)))
              (lodds.event:push-event :info text)
              (format t "~a~%" text)))
          (q+:set-text widget fallback-text)))))

(defun format-dropped-links (drop-event)
  (let* ((dropped-link (q+:const-data
                        (q+:data (q+:mime-data drop-event)
                                 "text/uri-list")))
         (links (cl-strings:split dropped-link
                                  (format nil "~C~C"
                                          #\return #\linefeed))))
    (mapcar (lambda (link)
              (q+:qurl-from-percent-encoding
               (subseq link
                       #+os-windows 8
                       #-os-windows 7)))
            (remove-if-not (lambda (link)
                             (cl-strings:starts-with link "file://"))
                           links))))

(defun escape-html (html)
  "Copied from Lionchat Code (thanks Shinmera)"
  (with-output-to-string (stream)
    (loop for c across html
          do (case c
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (#\& (write-string "&amp;" stream))
               (#\Newline (write-string "<br>" stream))
               (T (write-char c stream))))))

(defun get-namestring-type (namestring)
  (let* ((dot-pos (position #\. namestring :from-end t))
         (icon (when (and dot-pos
                          (> (length namestring) (+ 1 dot-pos)))
                 (subseq namestring(+ 1 dot-pos)))))
    icon))

(defun load-filetype-icon (filetype &optional (on-failure (q+:make-qicon)))
  (let ((pathname (make-pathname
                   :name (or filetype "_blank")
                   :type "png"
                   :defaults (lodds.config:get-value :filetype-icon-folder))))
    (unless (uiop:file-exists-p pathname)
      (setf pathname (make-pathname :name "_blank"
                                    :defaults pathname)))
    (if (uiop:file-exists-p pathname)
        (q+:make-qicon (uiop:native-namestring pathname))
        on-failure)))

(defun add-row (parent &rest columns)
  "adds a new qtreewidget item below the given parent and sets the
columns content to the given columns (with q+:set-text, so columns has
to be a list of strings)"
  (let ((item (q+:make-qtreewidgetitem parent)))
    (q+:add-child parent item)
    (q+:set-flags item (q+:qt.item-is-enabled))
    (let ((i 0))
      (map nil (lambda (element)
                 (q+:set-text item i element)
                 (incf i))
           columns))
    item))

(defun select-color ()
  (with-finalizing ((dialog (q+:make-qcolordialog)))
    (when (eql 1 (q+:exec dialog))
      (q+:name (q+:current-color dialog)))))

(defun select-font ()
  (with-finalizing ((diag (q+:make-qfontdialog)))
    (when (eql 1 (q+:exec diag))
      (q+:selected-font diag))))

(defun select-directory (&optional convert-to-pathname)
  (let ((dir (q+:qfiledialog-get-existing-directory)))
    (when (> (length dir) 0)
      (if convert-to-pathname
          (lodds.core:ensure-directory-pathname dir)
          dir))))

(defun select-file (&optional convert-to-pathname)
  (let ((file (q+:qfiledialog-get-open-file-name)))
    (when (> (length file) 0)
      (if convert-to-pathname
          (pathname
           (lodds.core:escape-wildcards file))
          file))))

(defun select-save-file (&optional convert-to-pathname)
  (let ((file (q+:qfiledialog-get-save-file-name)))
    (when (> (length file) 0)
      (if convert-to-pathname
          (uiop:ensure-absolute-pathname
           (lodds.core:escape-wildcards file))
          file))))

(defun get-font (description)
  (if (or (null description) (eql 0 (length description)))
      (let ((font (q+:make-qfont)))
        (q+:from-string font
                        (or *system-default-font*
                            (error "Cannot retrieve default font, *defualt-font* not yet set")))
        font)
      (if (char= #\_ (char description 0))
          (let ((font (q+:make-qfont)))
            (q+:from-string font (subseq description 1))
            font)
          (q+:make-qfont description))))
