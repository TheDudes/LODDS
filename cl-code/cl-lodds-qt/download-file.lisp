(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget download-file (QWidget)
  ;; going to be set on initialize-instance, and then used by download
  ;; method. Just here to transfer the checksum over to the method
  ((checksm)))

(define-subwidget (download-file user)
    (q+:make-qcombobox download-file)
  (q+:add-item user "Any"))

(define-subwidget (download-file filename)
    (q+:make-qlineedit download-file))

(define-subwidget (download-file folder)
    (q+:make-qlineedit download-file)
  (let* ((completer (q+:make-qcompleter download-file))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer folder completer)
    (q+:set-minimum-width folder 300)))

(define-subwidget (download-file select-folder-button)
    (q+:make-qpushbutton "Open" download-file)
  (connect select-folder-button "pressed()"
           (lambda ()
             (let ((dir (q+:qfiledialog-get-existing-directory)))
               (when (> (length dir)
                        0)
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
           (q+:add-row "Folder:" folder-layout))))

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
                     (format nil "Checksum: ~a~%Filename: ~a~%Size: ~a (~a bytes)"
                             checksum
                             name
                             (lodds.core:format-size size)
                             size))
    (q+:set-text filename name)
    (q+:set-text folder (lodds.config:get-value :download-folder))))

(defmethod download ((download-file download-file))
  (with-slots-bound (download-file download-file)
    (let ((user (q+:current-text user))
          (directory (q+:text folder))
          (filename (q+:text filename))
          (checksum checksm))
      (when (> (length directory) 0)
        (setf directory (lodds.core:add-missing-slash directory)))
      (cond
        ((eql 0 (length directory))
         (progn
           (make-instance 'dialog
                          :title "Error - No Directory selected"
                          :text "Please select a Directory")
           nil))
        ((not (cl-fs-watcher:escaped-directory-exists-p directory))
         (progn
           (make-instance 'dialog
                          :title "Error - Directory does not exists"
                          :text "Please select a Directory which exists")
           nil))
        ((eql 0 (length filename))
         (progn
           (make-instance 'dialog
                          :title "Error - No Filename given"
                          :text "Please select a Filename")
           nil))
        (t (progn
             (lodds:get-file (concatenate 'string
                                          directory
                                          filename)
                             checksum
                             (unless (string= user "Any")
                               user))
             t))))))

(defun open-download-file-dialog (checksum name size users)
  (make-instance 'dialog
                 :title "Download File"
                 :text (format nil "Download File ~a" name)
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
