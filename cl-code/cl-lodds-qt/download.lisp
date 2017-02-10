(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget download (QWidget) ())

(define-subwidget (download download-file) (q+:make-qlineedit download))
(define-subwidget (download download-checksum) (q+:make-qlabel download))
(define-subwidget (download download-user-selection) (q+:make-qcombobox download)
  (q+:add-item download-user-selection "Any"))
(define-subwidget (download download-button) (q+:make-qpushbutton "Download" download))
(define-subwidget (download download-folder) (q+:make-qlineedit)
  (let* ((completer (q+:make-qcompleter download))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer download-folder completer)))

(define-signal (download start-download) (string))
(define-signal (download update-download) ())

(define-slot (download update-download) ()
  (declare (connected download (update-download)))
  ;; remove all entries from download-user-selection
  (loop :repeat (- (q+:count download-user-selection) 1)
        :do (q+:remove-item download-user-selection 1))
  (destructuring-bind (path checksum name users) *selected-file*
    (if path
        (progn
          ;; directory was clicked
          (qdoto download-file
                 (q+:set-text name)
                 (q+:set-enabled nil))
          (q+:set-text download-checksum path)
          (qdoto download-user-selection
                 ;; TODO: add selected user here
                 (q+:add-item users)
                 (q+:set-current-index 1)
                 (q+:set-enabled nil)))
        ;; file was clicked
        (progn
          (qdoto download-file
                 (q+:set-text name)
                 (q+:set-enabled t))
          (q+:set-text download-checksum checksum)
          (q+:set-enabled download-user-selection t)
          (dolist (user users)
            (q+:add-item download-user-selection user))))))

(define-slot (download start-download) ()
  (declare (connected download-button (pressed)))
  (let ((user (q+:current-text download-user-selection))
        (directory (q+:text download-folder))
        (filename (q+:text download-file))
        (checksum (q+:text download-checksum)))
    ;; add / if missing
    (when (> (length directory) 0)
      (setf directory
            (if (char= #\/ (char directory (- (length directory) 1)))
                directory
                (concatenate 'string directory "/"))))
    (cond
      ;; TODO: popup error
      ((eql 0 (length directory))
       (lodds.event:push-event :error (list "local directory not selected")))
      ((not (uiop:directory-exists-p directory))
       (lodds.event:push-event :error (list "local directory does not exist")))
      ((not (q+:is-enabled download-user-selection))
       ;; we got a folder download
       (lodds:get-folder checksum
                         (subseq checksum 0 (- (length checksum)
                                               (length filename)))
                         directory
                         user))
      ;; file download
      ((eql 0 (length filename))
       (lodds.event:push-event :error (list "no local filename given")))
      (t (lodds:get-file (concatenate 'string
                                      ;; in case trailing slash does not exists, add it
                                      directory
                                      filename)
                         checksum
                         (unless (string= user "Any")
                           user))))))

(define-initializer (download setup-widget)
  (qdoto (q+:make-qgridlayout download)
         ;; first row - checksum
         (q+:add-widget (q+:make-qlabel "Checksum:" download) 0 0)
         (q+:add-widget download-checksum 0 1 1 -1)
         ;; second row - local file location
         (q+:add-widget (q+:make-qlabel "Download to:" download) 1 0)
         (q+:add-widget download-folder 1 1 1 8)
         (q+:add-widget download-file 1 9 1 4)
         ;; third row - user selction and download button
         (q+:add-widget (q+:make-qlabel "User:" download) 2 0)
         (q+:add-widget download-user-selection 2 1 1 11)
         (q+:add-widget download-button 2 12 1 1)))
