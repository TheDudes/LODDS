(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget download-multiple (QWidget)
  ((items :initarg :items)))

(define-subwidget (download-multiple folder)
    (q+:make-qlineedit download-multiple)
  (q+:set-text folder (uiop:native-namestring
                       (lodds.config:get-value :download-folder)))
  (let* ((completer (q+:make-qcompleter download-multiple))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-tool-tip folder
                     (format nil "Local folder where the selected files~%~
                                 and folders will be saved."))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer folder completer)
    (q+:set-minimum-width folder 300)))

(define-subwidget (download-multiple select-folder-button)
    (q+:make-qpushbutton "Open" download-multiple)
  (connect select-folder-button "pressed()"
           (lambda ()
             (let ((dir (q+:qfiledialog-get-existing-directory)))
               (when (> (length dir)
                        0)
                 (q+:set-text folder dir))))))

(define-subwidget (download-multiple layout)
    (q+:make-qformlayout download-multiple)
  (let ((folder-layout (q+:make-qhboxlayout))
        (total-size 0)
        (folders 0)
        (files 0)
        (tooltips (list)))
    (loop :for (type info) :in items
          :collect
          (case type
            (:dir
             (destructuring-bind (fullpath name user size items)
                 info
               (declare (ignore name user))
               (incf total-size size)
               (incf folders)
               (push (format nil "Folder: ~a, Files: ~a, Size: ~a (~:d bytes)"
                             fullpath items (lodds.core:format-size size) size)
                     tooltips)))
            (:file
             (destructuring-bind (checksum name size users)
                 info
               (incf total-size size)
               (incf files)
               (push (format nil "File: ~a, Checksum: ~a, Size: ~a (~:d bytes), Users: ~{~a~^,~}"
                             name checksum (lodds.core:format-size size) size users)
                     tooltips)))))
    (q+:set-tool-tip download-multiple
                     (reduce (lambda (c n)
                               (concatenate 'string
                                            c (list #\newline) n))
                             tooltips))
    (qdoto folder-layout
           (q+:add-widget folder)
           (q+:add-widget select-folder-button))
    (qdoto layout
           (q+:add-row "Total Download Size:"
                       (q+:make-qlabel (format nil "~a (~:d bytes)"
                                               (lodds.core:format-size total-size)
                                               total-size)
                                       download-multiple))
           (q+:add-row "Selected:"
                       (q+:make-qlabel (format nil "~a file~:p and ~a folder~:p"
                                               files
                                               folders)
                                       download-multiple))
           (q+:add-row "Download to:"
                        folder-layout))))

(defmethod download ((download-multiple download-multiple))
  (with-slots-bound (download-multiple download-multiple)
    (let ((directory (q+:text folder)))
      (cond
        ((eql 0 (length directory))
         (progn
           (make-instance 'dialog
                          :title "Error - No directory selected"
                          :text "Please select a directory")
           nil))
        ((not (lodds.core:directory-exists directory))
         (progn
           (make-instance 'dialog
                          :title "Error - Directory does not exists"
                          :text "Please select a directory which exists")
           nil))
        (t (let ((directory-pathname (lodds.core:ensure-directory-pathname
                                      directory)))
             (loop :for (type info) :in items
                   :do (case type
                         (:dir
                          (destructuring-bind (fullpath name user size items)
                              info
                            (declare (ignore name size items))
                            (lodds:get-folder fullpath
                                              directory-pathname
                                              user)))
                         (:file
                          (destructuring-bind (checksum name size users)
                              info
                            (declare (ignore size users))
                            (lodds:get-file (merge-pathnames (uiop:parse-unix-namestring
                                                              (lodds.core:escape-wildcards name))
                                                             directory-pathname)
                                            checksum))))
                   :finally (return t))))))))

(defun open-download-multiple-dialog (selected-items-infos)
  (make-instance 'dialog
                 :title "Download multiple"
                 :text "Downloading multiple files"
                 :widget
                 (make-instance 'download-multiple
                                :items selected-items-infos)
                 :ok-text "Download"
                 :on-success-fn
                 (lambda (widget)
                   (download widget))))
