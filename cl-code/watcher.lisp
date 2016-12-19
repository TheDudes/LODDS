;;;; watcher.lisp

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
    (with-slots (lodds.subsystem:name
                 lodds.subsystem:alive-p
                 dir-watchers
                 list-of-changes) object
      (format stream "~a :alive-p ~a :watchers ~a last-change: ~a"
              lodds.subsystem:name
              lodds.subsystem:alive-p
              (length dir-watchers)
              (caar list-of-changes)))))

(defun get-file-stats (pathname)
  (values
   (lodds.core:generate-checksum pathname)
   (with-open-file (stream pathname
                           :direction :input
                           :if-does-not-exist nil)
     (if stream
         (file-length stream)
         0))))

(defmethod lodds.subsystem:start ((subsys watcher))
  (error "You cannot start the Watcher subsystem like that! use SHARE and UNSHARE to start/stop"))

(defmethod lodds.subsystem:stop ((subsys watcher))
  (mapcar
   (lambda (dir-watcher)
     (stop-dir-watcher dir-watcher nil))
   (dir-watchers subsys)))

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
  (let ((ft-hash (file-table-hash dir-watcher)))
    (setf (gethash pathname (file-table-name dir-watcher))
          (list checksum size))
    (let ((val (gethash checksum ft-hash)))
      (setf (gethash checksum ft-hash)
            (if val
                (cons pathname val)
                (list pathname))))))

(defun remove-file (dir-watcher pathname)
  "removes a file from the dir-watcher, will be called bei
   HOOK if a file was removed or has changed"
  (let* ((ft-name (file-table-name dir-watcher))
         (ft-hash (file-table-hash dir-watcher))
         (checksum (car (gethash pathname ft-name))))
    (funcall (change-hook dir-watcher)
             (destructuring-bind (checksum size)
                 (gethash pathname ft-name)
               (list (lodds.core:get-timestamp)
                     :del
                     checksum
                     size
                     (subseq pathname (length (root-dir-path dir-watcher))))))
    (let ((new-val (remove pathname (gethash checksum ft-hash)
                           :test #'string=)))
      (if new-val
          (setf (gethash checksum ft-hash)
                new-val)
          (remhash checksum ft-hash)))
    (remhash pathname ft-name)))

(defun update-file (dir-watcher pathname)
  "checks if the given file under PATHNAME changed (if the checksum is
   different to the current), and if so calls REMOVE-FILE and ADD-FILE"
  (multiple-value-bind (new-checksum new-size)
      (get-file-stats pathname)
    (unless (string= new-checksum
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
    (:file-changed (update-file dir-watcher pathname))))

(defmethod initialize-instance ((w dir-watcher) &rest initargs)
  (declare (ignorable initargs))
  (call-next-method)

  (multiple-value-bind (path name)
      (lodds.core:split-directory (cl-fs-watcher:dir w))
    (setf (slot-value w 'root-dir-name) name
          (slot-value w 'root-dir-path) path))

  ;; wait until dir-watcher is alive and added all initial handles
  (loop :while (not (cl-fs-watcher:alive-p w))
        :do (sleep 0.01))

  ;; now add all initialy tracked files
  (loop :for file :in (cl-fs-watcher:get-all-tracked-files w)
        :do (add-file w file))

  (setf (cl-fs-watcher:hook w)
        #'hook))

(defun get-all-tracked-file-infos (dir-watcher)
  "returns info about all tracked files."
  (loop :for filename :being :the :hash-keys :of (file-table-name dir-watcher)
        :using (hash-value info)
        :collect (append info (list (subseq filename (length (root-dir-path dir-watcher)))))))

(defun stop-dir-watcher (dir-watcher &optional (run-change-hook-p t))
  "stops a dir-watcher and its event loop and removes the dir-watcher
  from the dir-watchers list, also checks if it was the last
  dis-watcher, if so deletes list-of-changes and sets alive-p to nil"
  (cl-fs-watcher:stop-watcher dir-watcher)
  (when run-change-hook-p
    (loop :for info :in (get-all-tracked-file-infos dir-watcher)
          :do (funcall (change-hook dir-watcher)
                       (apply #'list
                              (lodds.core:get-timestamp)
                              :del
                              info))))
  (let ((watcher (lodds:get-subsystem :watcher)))
    (setf (dir-watchers watcher)
          (remove dir-watcher (dir-watchers watcher)))
    (unless (dir-watchers watcher)
      (bt:with-lock-held ((list-of-changes-lock watcher))
        (setf (list-of-changes watcher) nil
              (started-tracking watcher) 0))
      (setf (lodds.subsystem:alive-p watcher) nil)
      (lodds.event:push-event (lodds.subsystem:name watcher)
                              (list "stopped!")))))

(defun get-file-info (checksum)
  "returns a list with information about the requested file. if file
  with requested checksum is not found nil will be returned"
  (car
   (loop :for dir-watcher :in (dir-watchers (lodds:get-subsystem :watcher))
         :when (gethash checksum
                        (file-table-hash dir-watcher))
         :return it)))

(defun get-shared-folders ()
  "returns a list of all currently shared folders."
  (mapcar #'cl-fs-watcher:dir (dir-watchers (lodds:get-subsystem :watcher))))

(defun share-folder (folder-path)
  "share a given folder, adds a watcher to handle updates."
  ;; check if a folder like the given one exists already
  (let* ((dir (car (directory folder-path)))
         (absolute-path (format nil "~a" dir))
         (watcher (lodds:get-subsystem :watcher)))
    (if (null dir)
        (error "TODO: some error on given directory :( (does it exist?)")
        (multiple-value-bind (path name) (lodds.core:split-directory absolute-path)
          (declare (ignore path))
          (loop :for shared-folder :in (get-shared-folders)
                :do (multiple-value-bind (p n) (lodds.core:split-directory shared-folder)
                      (declare (ignore p))
                      (when (string= name n)
                        (error "TODO: the given directory can not be shared since a directory with that name already exists :("))))))
    (when (find folder-path (get-shared-folders))
      (error "TODO: the folder you tried to share has the same name"))
    (let* ((hook (lambda (change)
                   (bt:with-lock-held ((list-of-changes-lock watcher))
                     (push change (list-of-changes watcher))
                     (setf (last-change watcher) (car change)))))
           (new-dir-watcher
             (make-instance 'dir-watcher
                            :change-hook hook
                            :dir folder-path
                            :recursive-p t)))
      (when (eql 0 (started-tracking watcher))
        (setf (started-tracking watcher)
              (lodds.core:get-timestamp)))
      (push new-dir-watcher
            (dir-watchers watcher))
      (setf (lodds.subsystem:alive-p watcher) t)
      (lodds.event:push-event (lodds.subsystem:name watcher)
                              (list :watching folder-path)))))

(defun unshare-folder (folder-path)
  "unshare the given folder"
  (let ((watcher (lodds:get-subsystem :watcher)))
    (when folder-path
      (let ((rem-watcher (find (format nil "~a" (car (directory folder-path)))
                               (dir-watchers watcher )
                               :key #'cl-fs-watcher:dir
                               :test #'string=)))
        (if rem-watcher
            (stop-dir-watcher rem-watcher)
            (error "TODO: could not find watcher to unshare with given folder-path"))))))
