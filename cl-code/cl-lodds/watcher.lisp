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
   (lodds.core:generate-checksum pathname)
   (with-open-file (stream (lodds.core:escape-wildcards pathname)
                           :direction :input
                           :if-does-not-exist nil
                           :element-type '(unsigned-byte 8))
     (if stream
         (file-length stream)
         0))))

(defun stop ()
  (let ((watcher (lodds:get-watcher)))
    (dolist (dir-watcher (dir-watchers watcher))
      (stop-dir-watcher dir-watcher nil))))

(defun add-file (dir-watcher pathname &optional (checksum nil) (size nil))
  "adds a file to the given dir-watcher, this functions is called by
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
                 (subseq pathname (length (root-dir-path dir-watcher)))))
  (setf (gethash pathname (file-table-name dir-watcher))
        (list checksum size))
  (let* ((ft-hash (file-table-hash dir-watcher))
         (val (gethash checksum ft-hash)))
    (setf (gethash checksum ft-hash)
          (if val
              (cons pathname val)
              (list pathname)))))

(defun remove-file (dir-watcher pathname)
  "removes a file from the dir-watcher, will be called bei
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
                             (subseq pathname (length (root-dir-path dir-watcher))))))
            (let ((new-val (remove pathname (gethash checksum ft-hash)
                                   :test #'equal)))
              (if new-val
                  (setf (gethash checksum ft-hash)
                        new-val)
                  (remhash checksum ft-hash)))
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
    (:file-added (add-file dir-watcher pathname))
    (:file-removed (remove-file dir-watcher pathname))
    (:file-changed (update-file dir-watcher pathname))
    (:on-deleted (stop-dir-watcher dir-watcher))))

(defmethod initialize-instance :after ((w dir-watcher) &rest initargs)
  (declare (ignorable initargs))
  (multiple-value-bind (path name)
      (lodds.core:split-directory (cl-fs-watcher:dir w))
    (setf (slot-value w 'root-dir-name) name
          (slot-value w 'root-dir-path) path)))

(defun get-all-tracked-file-infos (dir-watcher)
  "returns info about all tracked files."
  (loop :for filename :being :the :hash-keys :of (file-table-name dir-watcher)
        :using (hash-value info)
        :collect (append info (list (subseq filename (length (root-dir-path dir-watcher)))))))

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

(defun folder-already-shared-p (folder-path)
  (if (or (find folder-path (get-shared-folders))
          (find (lodds.core:escaped-get-folder-name folder-path)
                (get-shared-folders)
                :test #'equal
                :key #'lodds.core:escaped-get-folder-name))
      t
      (loop :for watcher :in (dir-watchers (lodds:get-watcher))
            :do (loop :for dir
                      :being :the :hash-key :of (cl-fs-watcher:directory-handles watcher)
                      :do (when (equal dir folder-path)
                            (return-from folder-already-shared-p t))))))

(defun folder-shareable-p (folder-path)
  (let ((folder (lodds.core:get-absolute-path folder-path)))
    (cond
      ((lodds.core:file-exists folder)
       (values nil "Not able to share a File (only Folders possible)"))
      ((not (lodds.core:directory-exists folder))
       (values nil "Folder does not exist"))
      ((folder-already-shared-p folder)
       (values nil (format nil "Folder with name ~a already shared"
                           (lodds.core:escaped-get-folder-name folder))))
      ((> (length (dir-watchers (lodds:get-watcher)))
          42)
       (values nil "Cannot share anymore Directories"))
      (t (values t nil)))))

(defparameter *add-lock* (bt:make-recursive-lock "Dir Watchers Push Lock"))
(defun start-dir-watcher (folder-path)
  (let* ((watcher (lodds:get-watcher))
         (hook (lambda (change)
                 (bt:with-lock-held ((list-of-changes-lock watcher))
                   (push change (list-of-changes watcher))
                   (setf (last-change watcher) (car change)))))
         (new-dir-watcher
           (make-instance 'dir-watcher
                          :change-hook hook
                          :dir folder-path
                          :recursive-p t
                          :hook #'hook)))
    (setf (slot-value new-dir-watcher 'cl-fs-watcher:error-cb)
          (lambda (ev)
            (lodds.event:push-event :directory-error
                                    (format nil "Directory error on ~a~%~a"
                                            folder-path
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
                            folder-path)))

(defun share-folder (folder-path)
  "share a given folder, adds a watcher to handle updates.
  Check with FOLDER-SHAREABLE-P if folder can be shared first."
  ;; check if a folder like the given one exists already
  (when (folder-shareable-p folder-path)
    (let ((folder (lodds.core:get-absolute-path folder-path)))
      (bt:make-thread
       (lambda ()
         (start-dir-watcher folder))
       :name (format nil "Sharing Directory ~a"
                     folder)))))

(defun unshare-folder (folder-path)
  "unshare the given folder"
  (let ((watcher (lodds:get-watcher)))
    (let ((rem-watcher (find (lodds.core:get-absolute-path folder-path)
                             (dir-watchers watcher)
                             :key #'cl-fs-watcher:dir
                             :test #'equal)))
      (if rem-watcher
          (stop-dir-watcher rem-watcher)
          (error "TODO: could not find watcher to unshare with given ~
                 folder-path")))))

(defun folder-busy-p (folder)
  (let ((watcher (find (lodds.core:get-absolute-path folder)
                       (dir-watchers (lodds:get-watcher))
                       :key #'cl-fs-watcher:dir
                       :test #'equal)))
    (when watcher
      (cl-fs-watcher:busy-p watcher))))
