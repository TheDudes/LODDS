(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget download-file (QWidget)
  ;; going to be set on initialize-instance, and then used by download
  ;; method. Just here to transfer the checksum over to the method
  ((checksm)))

(define-subwidget (download-file user)
    (q+:make-qcombobox download-file)
  (q+:set-tool-tip user
                   (format nil "If set to \"Any\" the file will be split~%~
                               up and each part will be downloaded from~%~
                               the 'best' (in terms of load) available~%~
                               user. If a user is selected, the file will~%~
                               be downloaded directly and only from him."))
  (q+:add-item user "Any"))

(define-subwidget (download-file filename)
    (q+:make-qlineedit download-file)
  (q+:set-tool-tip filename
                   (format nil "Local filename of the downloaded file.~%~
                               Can be changed to rename the file.")))

(define-subwidget (download-file folder)
    (q+:make-qlineedit download-file)
  (let* ((completer (q+:make-qcompleter download-file))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-tool-tip folder
                     (format nil "Local folder where the file will~%~
                                 be saved."))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer folder completer)
    (q+:set-minimum-width folder 300)))

(define-subwidget (download-file select-folder-button)
    (q+:make-qpushbutton "Open" download-file)
  (connect select-folder-button "pressed()"
           (lambda ()
             (let ((dir (select-directory)))
               (when dir
                 (q+:set-text folder dir))))))

(define-subwidget (download-file layout)
    (q+:make-qformlayout download-file)
  (let ((folder-layout (q+:make-qhboxlayout)))
    (qdoto folder-layout
           (q+:add-widget folder)
           (q+:add-widget select-folder-button))
    (qdoto layout
           (q+:add-row "User:" user)
           (q+:add-row "Filename:" filename)
           (q+:add-row "Download to:" folder-layout))))

(defmethod initialize-instance :after ((download-file download-file)
                                       &key
                                         checksum
                                         name
                                         users
                                         size)
  (with-slots-bound (download-file download-file)
    (dolist (usr users)
      (q+:add-item user usr))
    (setf checksm checksum)
    (q+:set-tool-tip download-file
                     (format nil "Checksum: ~a~%Filename: ~a~%Size: ~a (~:d bytes)"
                             checksum
                             name
                             (lodds.core:format-size size)
                             size))
    (q+:set-text filename name)
    (q+:set-text folder (uiop:native-namestring
                         (lodds.config:get-value :download-folder)))))

(defmethod download ((download-file download-file))
  (with-slots-bound (download-file download-file)
    (let ((user (q+:current-text user))
          (directory (q+:text folder))
          (filename (q+:text filename))
          (checksum checksm))
      (cond
        ((eql 0 (length directory))
         (progn
           (make-instance 'dialog
                          :title "Error - No Directory selected"
                          :text "Please select a directory")
           nil))
        ((not (lodds.core:directory-exists directory))
         (progn
           (make-instance 'dialog
                          :title "Error - Directory does not exists"
                          :text "Please select a directory which exists")
           nil))
        ((eql 0 (length filename))
         (progn
           (make-instance 'dialog
                          :title "Error - No filename given"
                          :text "Please select a filename")
           nil))
        (t (prog1 t
             (lodds:get-file (merge-pathnames (lodds.core:escape-wildcards filename)
                                              (lodds.core:ensure-directory-pathname directory))
                             checksum
                             (unless (string= user "Any")
                               user))))))))

(defun open-download-file-dialog (checksum name size users)
  (make-instance 'dialog
                 :title "Download file"
                 :text (format nil "Download file ~a" name)
                 :widget
                 (make-instance 'download-file
                                :name name
                                :users users
                                :checksum checksum
                                :size size)
                 :ok-text "Download"
                 :on-success-fn
                 (lambda (widget)
                   (download widget))))

