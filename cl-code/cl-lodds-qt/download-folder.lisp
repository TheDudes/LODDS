(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget download-folder (QWidget)
  ((user :initarg :user)
   (full-dir :initarg :full-dir)
   (dir-name :initarg :dir-name)
   (size :initarg :size)
   (files :initarg :files)))

(define-subwidget (download-folder folder)
    (q+:make-qlineedit download-folder)
  (q+:set-text folder (uiop:native-namestring
                       (lodds.config:get-value :download-folder)))
  (let* ((completer (q+:make-qcompleter download-folder))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-tool-tip folder
                     (format nil "Local folder where the downloaded~%~
                                 folder will be saved."))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer folder completer)
    (q+:set-minimum-width folder 300)))

(define-subwidget (download-folder select-folder-button)
    (q+:make-qpushbutton "Open" download-folder)
  (connect select-folder-button "pressed()"
           (lambda ()
             (let ((dir (q+:qfiledialog-get-existing-directory)))
               (when (> (length dir)
                        0)
                 (q+:set-text folder dir))))))

(define-subwidget (download-folder layout)
    (q+:make-qformlayout download-folder)
  (let ((folder-layout (q+:make-qhboxlayout)))
    (qdoto folder-layout
           (q+:add-widget folder)
           (q+:add-widget select-folder-button))
    (qdoto layout
           (q+:add-row "Folder:"
                       (q+:make-qlabel dir-name
                                       download-folder))
           (q+:add-row "Size:"
                       (q+:make-qlabel (format nil "~a (~:d bytes)"
                                               (lodds.core:format-size size)
                                               size)
                                       download-folder))
           (q+:add-row "Files:"
                       (q+:make-qlabel (format nil "~a" files)
                                       download-folder))
           (q+:add-row "Download to:"
                        folder-layout))))

(defmethod download ((download-folder download-folder))
  (with-slots-bound (download-folder download-folder)
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
        (t (progn
             (lodds:get-folder full-dir
                               (uiop:ensure-absolute-pathname
                                (uiop:ensure-directory-pathname
                                 (cl-fs-watcher:escape-wildcards directory)))
                               user)
             t))))))

(defun open-download-folder-dialog (fullpath dir user size files)
  (make-instance 'dialog
                 :title "Download folder"
                 :text (format nil "Download folder ~a" fullpath)
                 :widget
                 (make-instance 'download-folder
                                :dir-name dir
                                :full-dir fullpath
                                :user user
                                :size size
                                :files files)
                 :ok-text "Download"
                 :on-success-fn
                 (lambda (widget)
                   (download widget))))
