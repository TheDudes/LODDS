#|

Contains Shared widget related stuff, The Shared widget displays all
locally shared directories. (The one the lodds client shares with
others)

|#

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

(defun load-spinner (parent)
  (let ((pathname (make-pathname :name "spinner.svg"
                                 :defaults (lodds.config:get-value :resources-folder))))
    (if (uiop:file-exists-p pathname)
        (qdoto (q+:make-qsvgwidget parent)
               (q+:load (q+:make-qbytearray (uiop:read-file-string pathname))))
        (qdoto (q+:make-qprogressbar parent)
               (q+:set-maximum 0)
               (q+:set-minimum 0)
               (q+:set-format "...")))))

(defmethod set-spinner ((directories directories) entry)
  (unless (string= (q+:text entry +shared-widget-type+)
                   "spinner")
    (q+:set-text entry +shared-widget-type+ "spinner")
    (let ((old-widget (q+:item-widget directories entry +shared-widget+)))
      (q+:remove-item-widget directories entry +shared-widget+)
      (finalize old-widget))
    (q+:set-item-widget directories
                        entry
                        +shared-widget+
                        (qdoto (load-spinner directories)
                               (q+:set-tool-tip
                                (format nil
                                        "Directory ~a is currently busy~%~
                                        which means some files where added or removed~%~
                                        and the directory watcher is currently generating~%~
                                        checksums for the new files."
                                        (q+:text entry +shared-path+)))))))

(defmethod set-button ((directories directories) entry)
  (unless (string= (q+:text entry +shared-widget-type+)
                   "button")
    (q+:set-text entry +shared-widget-type+ "button")
    (let ((old-widget (q+:item-widget directories entry +shared-widget+)))
      (q+:remove-item-widget directories entry +shared-widget+)
      (finalize old-widget))
    (let ((button (q+:make-qpushbutton directories))
          (pathname (pathname (lodds.core:escape-wildcards
                               (q+:text entry +shared-fullpath+)))))
      (q+:set-tool-tip button (format nil
                                      "Click to unshare ~a"
                                      (q+:text entry +shared-path+)))
      (set-icon button "x.png" "Unshare")
      (connect button "pressed()"
               (lambda ()
                 (when (lodds.watcher:folder-already-shared-p pathname)
                   (lodds.watcher:unshare-folder pathname))))
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

(defmethod add-new-dir-widget ((directories directories) pathname)
  (let ((new-entry (q+:make-qtreewidgetitem directories))
        (relative-pathname (make-pathname :directory (cons :relative
                                                           (last (pathname-directory pathname)))
                                          :device nil
                                          :defaults pathname)))
    (qdoto new-entry
           (q+:set-text +shared-path+
                        (format nil "~a"
                                relative-pathname))
           (q+:set-tool-tip +shared-path+
                            (format nil
                                    "Click the button on the right ~
                                    to unshare ~a"
                                    relative-pathname))
           (q+:set-status-tip +shared-path+
                              (format nil "Directory: ~a" pathname))
           (q+:set-text +shared-fullpath+
                        (uiop:native-namestring pathname)))
    (setf (gethash pathname (slot-value directories 'dirs)) new-entry)
    (set-spinner directories new-entry)))

(defmethod share-directories ((directories directories) pathnames)
  (let ((failed-dirs (list)))
    (loop :for pathname :in pathnames
          :do
          (multiple-value-bind (shareable-p error)
              (lodds.watcher:folder-shareable-p pathname)
            (if shareable-p
                (progn
                  (lodds.watcher:share-folder pathname)
                  (add-new-dir-widget directories pathname))
                (push (list pathname error) failed-dirs))))
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
  (let ((pathname (pathname (lodds.core:escape-wildcards dir))))
    (unless (gethash pathname dirs)
      (add-new-dir-widget directories pathname))))

(define-slot (directories remove-directory) ((path string))
  (declare (connected directories (remove-directory string)))
  (loop :for i :from 0 :below (q+:top-level-item-count directories)
        :do (let ((child (q+:top-level-item directories i)))
              (when (equal path (q+:text child +shared-fullpath+))
                (q+:take-top-level-item directories i)
                (remhash (pathname (lodds.core:escape-wildcards path)) dirs)
                (return)))))

(define-override (directories drag-enter-event) (ev)
  (when (q+:has-urls (q+:mime-data ev))
    (q+:accept-proposed-action ev)))

(define-override (directories drag-move-event) (ev)
  (q+:accept-proposed-action ev))

(define-override (directories drop-event) (ev)
  (share-directories directories
                     (mapcar (lambda (namestring)
                               (lodds.core:ensure-directory-pathname
                                namestring))
                             (format-dropped-links ev))))

(define-initializer (directories setup-widget)
  (qdoto directories
         (q+:set-mouse-tracking t)
         (q+:set-object-name "Shared")
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 4)
         (q+:set-uniform-row-heights t)
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
  (loop :for pathname :in (lodds.watcher:get-shared-folders)
        :do (add-new-dir-widget directories pathname)))

(define-initializer (directories setup-callbacks)
  (lodds.event:add-callback :qt-directories
                            (lambda (dir)
                              (signal! directories
                                       (add-directory string)
                                       (uiop:native-namestring dir)))
                            :shared-directory)
  (lodds.event:add-callback :qt-directories
                            (lambda (dir)
                              (signal! directories
                                       (remove-directory string)
                                       (uiop:native-namestring dir)))
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
