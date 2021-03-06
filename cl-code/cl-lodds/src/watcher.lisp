#|

This file contains all watcher related stuff. The Watcher handles
watching directories, but most of the heavy lifting is done by the
cl-fs-watcher library. The watcher basically just a wrapper around the
cl-fs-watcher and handles all lodds related things, like generating
checksums on file changes, adding and deleting of files and keeping
tracked about the shared files.

|#

(in-package #:lodds.watcher)

(defmethod print-object ((object dir-watcher) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (root-dir-path root-dir-name file-table-name) object
      (format stream "~a~a :files ~a"
              root-dir-path
              root-dir-name
              (hash-table-count file-table-name)))))

(defmethod print-object ((object watcher) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (alive-p dir-watchers list-of-changes) object
      (format stream "Watcher :alive-p ~a :watchers ~a last-change: ~a"
              alive-p
              (length dir-watchers)
              (caar list-of-changes)))))

(defun get-file-stats (pathname)
  (values
   (if (lodds.config:get-value :fake-checksum)
       (lodds.core:generate-fake-checksum)
       (or (lodds.core:generate-checksum pathname)
           "0000000000000000000000000000000000000000"))
   (lodds.core:get-file-size pathname)))

(defun stop ()
  (let ((watcher (lodds:get-watcher)))
    (dolist (dir-watcher (dir-watchers watcher))
      (stop-dir-watcher dir-watcher nil))))

(defun add-file (dir-watcher pathname &optional (checksum nil) (size nil))
  "adds a file to the given dir-watcher, this function is called by
   HOOK to update the dir-watcher if a file was added or has changed"
  (unless (and checksum size)
    (multiple-value-bind (hash filesize) (get-file-stats pathname)
      (setf checksum hash
            size filesize)))
  (funcall (change-hook dir-watcher)
           (list (lodds.core:get-timestamp)
                 :add
                 checksum
                 size
                 (uiop:unix-namestring
                  (merge-pathnames
                   (enough-namestring pathname
                                      (root-dir-path dir-watcher))
                   (uiop:parse-unix-namestring "/")))))
  (setf (gethash pathname (file-table-name dir-watcher))
        (list checksum size))
  (let ((ft-hash (file-table-hash dir-watcher)))
    (setf (gethash checksum ft-hash)
          (cons pathname (gethash checksum ft-hash)))))

(defun remove-file (dir-watcher pathname)
  "removes a file from the dir-watcher, will be called by
   HOOK if a file was removed or has changed"
  (let* ((ft-name (file-table-name dir-watcher))
         (ft-hash (file-table-hash dir-watcher))
         (checksum (car (gethash pathname ft-name))))
    (let ((entry (gethash pathname ft-name)))
      (if (not entry)
          (format t "watcher:remove-file entry ~a not found~%"
                  pathname)
          (progn
            (funcall (change-hook dir-watcher)
                     (destructuring-bind (checksum size)
                         entry
                       (list (lodds.core:get-timestamp)
                             :del
                             checksum
                             size
                             (uiop:unix-namestring
                              (merge-pathnames
                               (enough-namestring pathname
                                                  (root-dir-path dir-watcher))
                               (uiop:parse-unix-namestring "/"))))))
            (unless (setf (gethash checksum ft-hash)
                          (remove pathname (gethash checksum ft-hash)
                                  :test #'equal))
              (remhash checksum ft-hash))
            (remhash pathname ft-name))))))

(defun update-file (dir-watcher pathname)
  "checks if the given file under PATHNAME changed (if the checksum is
   different to the current), and if so calls REMOVE-FILE and ADD-FILE"
  (multiple-value-bind (new-checksum new-size)
      (get-file-stats pathname)
    (unless (equal new-checksum
                   (car (gethash pathname (file-table-name dir-watcher))))
      ;; in case checksum changed, we need to update
      (remove-file dir-watcher pathname)
      (add-file dir-watcher pathname new-checksum new-size))))

(defun hook (dir-watcher pathname type)
  "will be called if some filesystem events occur inside the watched
   directory"
  (case type
    ;; on windows we wont get file-removed events for each file inside
    ;; a directory, which means we have to remove all files from a
    ;; directory by hand. On linux we get a event for each file when
    ;; the directory was removed, but not when it was renamed. This
    ;; will add a little overhead on non-windows systems, but should
    ;; be ok.
    (:directory-removed
     (loop :for file-pathname :being :the :hash-keys :of (file-table-name dir-watcher)
           :when (uiop:subpathp file-pathname pathname)
           :do (remove-file dir-watcher file-pathname)))
    (:file-added (add-file dir-watcher pathname))
    (:file-removed (remove-file dir-watcher pathname))
    (:file-changed (update-file dir-watcher pathname))
    (:on-deleted (stop-dir-watcher dir-watcher))))

(defmethod initialize-instance :after ((dir-watcher dir-watcher) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (root-dir-name root-dir-path) dir-watcher
    (let ((dir (cl-fs-watcher:dir dir-watcher)))
      (setf root-dir-name
            (make-pathname :directory (cons :relative
                                            (last (pathname-directory dir)))
                           :device nil
                           :defaults dir)
            root-dir-path
            (make-pathname :directory (butlast (pathname-directory dir))
                           :defaults dir)))))

(defun get-all-tracked-file-infos (dir-watcher)
  "returns info about all tracked files."
  (loop :for pathname :being :the :hash-keys :of (file-table-name dir-watcher)
        :using (hash-value info)
        :collect (append info
                         (list
                          (uiop:unix-namestring
                           (merge-pathnames
                            (enough-namestring pathname
                                               (root-dir-path dir-watcher))
                            (uiop:parse-unix-namestring "/")))))))

(defun stop-dir-watcher (dir-watcher &optional (run-change-hook-p t))
  "stops a dir-watcher and its event loop and removes the dir-watcher
  from the dir-watchers list, also checks if it was the last
  dir-watcher, if so deletes list-of-changes and sets alive-p to nil"
  (cl-fs-watcher:stop-watcher dir-watcher)
  (lodds.event:push-event :unshared-directory
                          (cl-fs-watcher:dir dir-watcher))
  (when run-change-hook-p
    (loop :for info :in (get-all-tracked-file-infos dir-watcher)
          :do (funcall (change-hook dir-watcher)
                       (apply #'list
                              (lodds.core:get-timestamp)
                              :del
                              info))))
  (let ((watcher (lodds:get-watcher)))
    (setf (dir-watchers watcher)
          (remove dir-watcher (dir-watchers watcher)))
    (unless (dir-watchers watcher)
      (bt:with-lock-held ((list-of-changes-lock watcher))
        (setf (list-of-changes watcher) nil
              (started-tracking watcher) 0
              (last-change watcher) (lodds.core:get-timestamp)))
      (setf (slot-value watcher 'alive-p) nil)
      (lodds.event:push-event :watcher "stopped!"))))

(defun get-file-info (checksum)
  "returns a list with information about the requested file. if file
  with requested checksum is not found nil will be returned"
  (car
   (loop :for dir-watcher :in (dir-watchers (lodds:get-watcher))
         :when (gethash checksum
                        (file-table-hash dir-watcher))
         :return it)))

(defun get-shared-folders ()
  "returns a list of all currently shared folders."
  (mapcar #'cl-fs-watcher:dir (dir-watchers (lodds:get-watcher))))

(defun folder-already-shared-p (pathname)
  (if (find pathname
            (get-shared-folders)
            :test #'equal)
      t
      (loop :for watcher :in (dir-watchers (lodds:get-watcher))
            :do (loop :for shared-pathname
                      :being :the :hash-key :of (cl-fs-watcher:directory-handles watcher)
                      :do (when (equal shared-pathname pathname)
                            (return-from folder-already-shared-p t))))))

(defun folder-shareable-p (folder-path)
  (let ((pathname (lodds.core:ensure-directory-pathname folder-path)))
    (cond
      ((not (lodds.core:directory-exists pathname ))
       (values nil "Folder does not exist"))
      ((folder-already-shared-p pathname)
       (values nil (format nil "Folder with name ~a already shared"
                           pathname)))
      ((> (length (dir-watchers (lodds:get-watcher)))
          42)
       (values nil "Cannot share anymore Directories"))
      (t (values pathname nil)))))

(defparameter *add-lock* (bt:make-recursive-lock "Dir Watchers Push Lock"))
(defun start-dir-watcher (pathname)
  (let* ((watcher (lodds:get-watcher))
         (change-hook (lambda (change)
                        (bt:with-lock-held ((list-of-changes-lock watcher))
                          (push change (list-of-changes watcher))
                          (setf (last-change watcher) (car change)))))
         (new-dir-watcher
           (make-instance 'dir-watcher
                          :change-hook change-hook
                          :dir pathname
                          :hook #'hook)))
    (setf (slot-value new-dir-watcher 'cl-fs-watcher:error-cb)
          (lambda (ev)
            (lodds.event:push-event :directory-error
                                    (format nil "Directory error on ~a~%~a"
                                            pathname
                                            ev))
            (format t "ERROR: cl-fs-watcher error on ~a:~a"
                    new-dir-watcher
                    ev)
            (stop-dir-watcher new-dir-watcher)))
    (cl-fs-watcher:start-watcher new-dir-watcher
                                 (list (cons 'lodds:*server* lodds:*server*)))
    (when (eql 0 (started-tracking watcher))
      (setf (started-tracking watcher)
            (lodds.core:get-timestamp)))
    (bt:with-recursive-lock-held (*add-lock*)
      (push new-dir-watcher
            (dir-watchers watcher)))
    (setf (slot-value watcher 'alive-p) t)
    (lodds.event:push-event :shared-directory
                            pathname)))

(defun share-folder (folder-path)
  "share a given folder, adds a watcher to handle updates.
  Check with FOLDER-SHAREABLE-P if folder can be shared first."
  ;; check if a folder like the given one exists already
  (multiple-value-bind (pathname error) (folder-shareable-p folder-path)
    (unless error
      (bt:make-thread
       (lambda ()
         (start-dir-watcher pathname))
       :name (format nil "Sharing Directory ~a"
                     pathname))
      pathname)))

(defun unshare-folder (folder-path)
  "unshare the given folder"
  (let ((watcher (lodds:get-watcher)))
    (let ((rem-watcher (find (lodds.core:ensure-directory-pathname folder-path)
                             (dir-watchers watcher)
                             :key #'cl-fs-watcher:dir
                             :test #'equal)))
      (if rem-watcher
          (stop-dir-watcher rem-watcher)
          (error "TODO: could not find watcher to unshare with given ~
                 folder-path")))))

(defun folder-busy-p (pathname)
  (let ((watcher (find pathname
                       (dir-watchers (lodds:get-watcher))
                       :key #'cl-fs-watcher:dir
                       :test #'equal)))
    (when watcher
      (cl-fs-watcher:busy-p watcher))))
