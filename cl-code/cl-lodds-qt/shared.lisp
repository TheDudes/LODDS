(in-package #:lodds-qt)
(in-readtable :qtools)

;; shared columns
(defvar +shared-path+ 0)
(defvar +shared-widget+ 1)
(defvar +shared-widget-type+ 2)
(defvar +shared-fullpath+ 3)

(define-widget directories (QTreeWidget)
  ((dirs :initform (make-hash-table :test #'equal)
         :documentation "Shared directories with directory path as key
         and qtreewidgetitem as value")))

(defmethod set-spinner ((directories directories) entry)
  (unless (string= (q+:text entry +shared-widget-type+)
                   "spinner")
    (q+:set-text entry +shared-widget-type+ "spinner")
    (let ((old-widget (q+:item-widget directories entry +shared-widget+)))
      (q+:remove-item-widget directories entry +shared-widget+)
      (finalize old-widget))
    (let ((spinner (q+:make-qprogressbar directories)))
      (qdoto spinner
             (q+:set-maximum 0)
             (q+:set-minimum 0)
             (q+:set-format "...")
             (q+:set-tool-tip
              (format nil
                      "Directory ~a is currently busy~%~
                      This means some files where added or removed~%~
                      And the Directory Watcher is currently generating~%~
                      Checksums for the new files."
                      (q+:text entry +shared-path+))))
      (q+:set-item-widget directories
                          entry
                          +shared-widget+
                          spinner))))

(defmethod set-button ((directories directories) entry)
  (unless (string= (q+:text entry +shared-widget-type+)
                   "button")
    (q+:set-text entry +shared-widget-type+ "button")
    (let ((old-widget (q+:item-widget directories entry +shared-widget+)))
      (q+:remove-item-widget directories entry +shared-widget+)
      (finalize old-widget))
    (let ((button (q+:make-qpushbutton directories))
          (path (q+:text entry +shared-fullpath+)))
      (with-finalizing* ((pixmap (q+:make-qpixmap
                                  (format nil
                                          "~a~a"
                                          (lodds.config:get-value :resources-folder)
                                          "x.png")))
                         (icon (q+:make-qicon pixmap)))
        (qdoto button
               (q+:set-tool-tip (format nil
                                        "Click to unshare ~a"
                                        (q+:text entry +shared-path+)))
               (q+:set-icon icon)
               (q+:set-flat t)))
      (connect button "pressed()"
               (lambda ()
                 (when (lodds.watcher:folder-already-shared-p path)
                   (lodds.watcher:unshare-folder path))))
      (q+:set-item-widget directories
                          entry
                          +shared-widget+
                          button))
    (q+:update-geometries directories)))

(define-subwidget (directories timer) (q+:make-qtimer directories))

(define-slot (directories tick) ()
  (declare (connected timer (timeout)))
  (maphash (lambda (dir widget)
             (if (lodds.watcher:folder-busy-p dir)
                 (set-spinner directories widget)
                 (set-button directories widget)))
           dirs))

(defmethod add-new-dir-widget ((directories directories) dir)
  (let ((new-entry (q+:make-qtreewidgetitem directories)))
    (q+:set-text new-entry +shared-path+
                 (or (lodds.core:escaped-get-folder-name dir)
                     ""))
    (q+:set-tool-tip new-entry +shared-path+
                     (format nil "~a" dir))
    (q+:set-text new-entry +shared-fullpath+ (format nil "~a" dir))
    (setf (gethash dir (slot-value directories 'dirs)) new-entry)
    (set-spinner directories new-entry)))

(defmethod share-directories ((directories directories) dirs)
  (let ((dirs-with-slash (mapcar #'lodds.core:add-missing-slash
                                 dirs))
        (failed-dirs (list)))
    (loop :for dir :in dirs-with-slash
          :do
          (multiple-value-bind (shareable-p error)
              (lodds.watcher:folder-shareable-p dir)
            (if shareable-p
                (with-slots-bound (directories directories)
                  (lodds.watcher:share-folder dir)
                  (add-new-dir-widget directories dir))
                (push (list dir error) failed-dirs))))
    (when (> (length failed-dirs) 0)
      (make-instance 'dialog
                     :title (format nil "Error - Could not Share directories")
                     :text (format nil
                                   "Sorry, was not able to share the following directories:~%~:{~%~a (~a)~}"
                                   failed-dirs)))))

(define-signal (directories add-directory) (string))
(define-signal (directories remove-directory) (string))

(define-slot (directories add-directory) ((dir string))
  (declare (connected directories (add-directory string)))
  (unless (gethash dir dirs)
    (add-new-dir-widget directories dir)))

(define-slot (directories remove-directory) ((path string))
  (declare (connected directories (remove-directory string)))
  (loop :for i :from 0 :below (q+:top-level-item-count directories)
        :do (let ((child (q+:top-level-item directories i)))
              (when (string= path (q+:text child +shared-fullpath+))
                (q+:take-top-level-item directories i)
                (remhash path dirs)
                (return)))))

(define-override (directories drag-enter-event) (ev)
  (when (q+:has-urls (q+:mime-data ev))
    (q+:accept-proposed-action ev)))

(define-override (directories drag-move-event) (ev)
  (q+:accept-proposed-action ev))

(define-override (directories drop-event) (ev)
  (let* ((dropped-link (q+:const-data
                        (q+:data (q+:mime-data ev)
                                 "text/uri-list")))
         (links (cl-strings:split dropped-link
                                  (format nil "~C~C"
                                          #\return #\linefeed))))
    (share-directories directories
                       (mapcar (lambda (link)
                                 (subseq link 7))
                               (remove-if-not (lambda (link)
                                                (cl-strings:starts-with link "file://"))
                                              links)))))

(define-initializer (directories setup-widget)
  (qdoto directories
         (q+:set-object-name "Shared")
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 4)
         (q+:set-header-labels (list "Path" "" "" ""))
         (q+:set-alternating-row-colors t)
         (q+:set-accept-drops t)
         (q+:hide-column +shared-widget-type+)
         (q+:hide-column +shared-fullpath+))
  (qdoto (q+:header directories)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shared-path+
                             (q+:qheaderview.stretch))
         (q+:set-resize-mode +shared-widget+
                             (q+:qheaderview.resize-to-contents))))

(define-initializer (directories setup-add-directories)
  (loop :for dir :in (lodds.watcher:get-shared-folders)
        :do (add-new-dir-widget directories dir)))

(define-initializer (directories setup-callbacks)
  (lodds.event:add-callback :qt-directories
                            (lambda (event)
                              (signal! directories
                                       (add-directory string)
                                       (second event)))
                            :shared-directory)
  (lodds.event:add-callback :qt-directories
                            (lambda (event)
                              (signal! directories
                                       (remove-directory string)
                                       (second event)))
                            :unshared-directory))

(define-finalizer (directories cleanup-callbacks)
  (lodds.event:remove-callback :qt-directories :shared-directory)
  (lodds.event:remove-callback :qt-directories :unshared-directory))

(define-widget shared (QWidget) ())

(define-subwidget (shared shared-directories)
    (make-instance 'directories))

(define-subwidget (shared share-button)
    (q+:make-qpushbutton "Share Directory" shared)
  (connect share-button "pressed()"
           (lambda ()
             (let ((dir (q+:qfiledialog-get-existing-directory)))
               (when (> (length dir)
                        0)
                 (share-directories shared-directories (list dir)))))))

(define-initializer (directories setup-timer)
  (q+:start timer (lodds.config:get-value :directory-busy-check)))

(defmethod set-directory-busy-check-timeout ((shared shared) new-timeout)
  (with-slots-bound (shared shared)
    (q+:set-interval (slot-value shared-directories 'timer)
                     new-timeout)))

(define-initializer (shared setup-widget)
  (let ((layout (q+:make-qvboxlayout shared)))
    (qdoto layout
           (q+:add-widget shared-directories)
           (q+:add-widget share-button))))
