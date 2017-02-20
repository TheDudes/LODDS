(in-package #:lodds-qt)
(in-readtable :qtools)

;; shared columns
(defvar +shared-path+ 0)
(defvar +shared-widget+ 1)

(define-widget shared (QWidget)
  ((directories :initform (make-hash-table :test #'equalp)
                :documentation "Shared directories with directory path
                as key and qtreewidgetitem as value")))

(define-subwidget (shared shared-directories) (q+:make-qtreewidget shared)
  (qdoto shared-directories
         (q+:set-column-count 2)
         (q+:set-header-labels (list "Path" ""))
         (q+:set-alternating-row-colors t))
  (qdoto (q+:header shared-directories)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shared-path+
                             (q+:qheaderview.stretch))
         (q+:set-resize-mode +shared-widget+
                             (q+:qheaderview.resize-to-contents))))

(define-subwidget (shared shared-share-button) (q+:make-qpushbutton "Share Directory" shared)
  (connect shared-share-button "pressed()"
           (lambda ()
             (let ((dir (q+:qfiledialog-get-existing-directory)))
               (when (> (length dir)
                        0)
                 (setf dir (concatenate 'string dir "/"))
                 (let ((new-entry (q+:make-qtreewidgetitem shared-directories))
                       (spinner (q+:make-qprogressbar shared-directories)))
                   (qdoto spinner
                          (q+:set-maximum 0)
                          (q+:set-minimum 0)
                          (q+:set-format "..."))
                   (q+:set-item-widget shared-directories new-entry +shared-widget+ spinner)
                   (q+:set-text new-entry +shared-path+ dir)
                   (setf (gethash dir directories) new-entry))
                 (bt:make-thread (lambda ()
                                   (lodds.watcher:share-folder dir))
                                 :name "Sharing Directory"))))))

(define-signal (shared add-directory) (string))
(define-signal (shared remove-directory) (string))

(define-slot (shared add-directory) ((path string))
  (declare (connected shared (add-directory string)))
  (let ((widget (gethash path directories)))
    (if widget
        (q+:remove-item-widget shared-directories widget +shared-widget+)
        (let ((new-entry (q+:make-qtreewidgetitem shared-directories)))
          (q+:set-text new-entry +shared-path+ path)
          (setf widget new-entry
                (gethash path directories) new-entry)))
    (let ((remove-button (q+:make-qpushbutton "Unshare" shared-directories)))
      (connect remove-button "pressed()"
               (lambda ()
                 (lodds.watcher:unshare-folder path)))
      (q+:set-item-widget shared-directories
                          widget
                          +shared-widget+
                          remove-button))))

(define-slot (shared remove-directory) ((path string))
  (declare (connected shared (remove-directory string)))
  (loop :for i :from 0 :below (q+:top-level-item-count shared-directories)
        :do (progn
              (let ((child (q+:top-level-item shared-directories i)))
                (when (string= path (q+:text child +shared-path+))
                  (q+:take-top-level-item shared-directories i)
                  (return))))))

(define-initializer (shared setup-widget)
  (let ((layout (q+:make-qvboxlayout shared)))
    (qdoto layout
           (q+:add-widget shared-directories)
           (q+:add-widget shared-share-button))))

(define-initializer (shared setup-callbacks)
  (lodds.event:add-callback :qt-shared
                            (lambda (event)
                              (signal! shared
                                       (add-directory string)
                                       (second event)))
                            :shared-directory)
  (lodds.event:add-callback :qt-shared
                            (lambda (event)
                              (signal! shared
                                       (remove-directory string)
                                       (second event)))
                            :unshared-directory))

;; init add folders
(define-initializer (shared setup-add-directories)
  (loop :for dir :in (lodds.watcher:get-shared-folders)
        :do (signal! shared (add-directory string) dir)))

(define-finalizer (shared cleanup-callbacks)
  (lodds.event:remove-callback :qt-shared :shared-directory)
  (lodds.event:remove-callback :qt-shared :unshared-directory))
