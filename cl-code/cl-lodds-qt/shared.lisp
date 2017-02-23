(in-package #:lodds-qt)
(in-readtable :qtools)

;; shared columns
(defvar +shared-path+ 0)
(defvar +shared-widget+ 1)

(define-widget directories (QTreeWidget)
  ((dirs :initform (make-hash-table :test #'equalp)
         :documentation "Shared directories with directory path as key
         and qtreewidgetitem as value")))

(defmethod share-directory ((directories directories) dir)
  (setf dir (lodds.core:add-missing-slash dir))
  (if (lodds.watcher:folder-already-shared-p dir)
      (make-instance 'dialog
                     :title (format nil "Error - ~a already Shared"
                                    dir)
                     :text (format nil
                                   "Sorry, cannot share ~a since its already shared"
                                   dir))
      (with-slots-bound (directories directories)
        (let ((new-entry (q+:make-qtreewidgetitem directories))
              (spinner (q+:make-qprogressbar directories)))
          (qdoto spinner
                 (q+:set-maximum 0)
                 (q+:set-minimum 0)
                 (q+:set-format "..."))
          (q+:set-item-widget directories new-entry +shared-widget+ spinner)
          (q+:set-text new-entry +shared-path+ dir)
          (setf (gethash dir dirs) new-entry))
        (lodds.watcher:share-folder dir))))

(define-signal (directories add-directory) (string))
(define-signal (directories remove-directory) (string))

(define-slot (directories add-directory) ((path string))
  (declare (connected directories (add-directory string)))
  (let ((widget (gethash path dirs)))
    (if widget
        (q+:remove-item-widget directories widget +shared-widget+)
        (let ((new-entry (q+:make-qtreewidgetitem directories)))
          (q+:set-text new-entry +shared-path+ path)
          (setf widget new-entry
                (gethash path dirs) new-entry)))
    (let ((remove-button (q+:make-qpushbutton "Unshare" directories)))
      (connect remove-button "pressed()"
               (lambda ()
                 (lodds.watcher:unshare-folder path)))
      (q+:set-item-widget directories
                          widget
                          +shared-widget+
                          remove-button))))

(define-slot (directories remove-directory) ((path string))
  (declare (connected directories (remove-directory string)))
  (loop :for i :from 0 :below (q+:top-level-item-count directories)
        :do (let ((child (q+:top-level-item directories i)))
              (when (string= path (q+:text child +shared-path+))
                (q+:take-top-level-item directories i)
                (return)))))

(define-override (directories drag-enter-event) (ev)
  (when (q+:has-urls (q+:mime-data ev))
    (q+:accept-proposed-action ev)))

(define-override (directories drag-move-event) (ev)
  (q+:accept-proposed-action ev))

(define-override (directories drop-event) (ev)
  (let ((dropped-link (q+:const-data
                       (q+:data (q+:mime-data ev)
                                "text/uri-list"))))
    (when (cl-strings:starts-with dropped-link "file://")
      (let ((filepath (subseq (lodds.core:remove-newline dropped-link) 7)))
        (cond
          ((uiop:file-exists-p filepath)
           (make-instance
            'dialog
            :title "Error - Cannot share File"
            :text "Its not possible to share a single File, select a directory please."))
          ((uiop:directory-exists-p filepath)
           (share-directory directories filepath))
          (t
           (make-instance
            'dialog
            :title "Error - Dont know what to do"
            :text "Whatever you dropped there is neither a dir nor a file.")))))))

(define-initializer (directories setup-widget)
  (qdoto directories
         (q+:set-object-name "Shared")
         (q+:set-selection-mode 0)
         (q+:set-column-count 2)
         (q+:set-header-labels (list "Path" ""))
         (q+:set-alternating-row-colors t)
         (q+:set-accept-drops t))
  (qdoto (q+:header directories)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shared-path+
                             (q+:qheaderview.stretch))
         (q+:set-resize-mode +shared-widget+
                             (q+:qheaderview.resize-to-contents))))

(define-initializer (directories setup-add-directories)
  (loop :for dir :in (lodds.watcher:get-shared-folders)
        :do (signal! directories (add-directory string) dir)))

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
                 (share-directory shared-directories dir))))))

(define-initializer (shared setup-widget)
  (let ((layout (q+:make-qvboxlayout shared)))
    (qdoto layout
           (q+:add-widget shared-directories)
           (q+:add-widget share-button))))
