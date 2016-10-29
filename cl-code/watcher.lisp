;;;; watcher.lisp

(in-package #:lodds.watcher)

(defun get-file-info (pathname)
  (values
   (lodds.core:sha-256 pathname)
   (with-open-file (stream pathname
                           :direction :input
                           :if-does-not-exist nil)
     (if stream
         (file-length stream)
         0))))

(stmx:transactional
 (defclass watcher (cl-fs-watcher:watcher)
   ((file-table-name :type hashtable
                     :initform (make-hash-table :test 'equal)
                     :reader file-table-name
                     :documentation "hashtable of tracked files, with their path as key.
                                    Value is a list of file checksum and its size.
                                    Note: this member is STMX:TRANSACTIONAL")
    (file-table-hash :type hashtable
                     :initform (make-hash-table :test 'equal)
                     :reader file-table-hash
                     :documentation "hashmap of tracked files, with their checksum as key.
                                    Value is a list of files with the given checksum.
                                    Note: this member is STMX:TRANSACTIONAL"))))

(defun add-file (watcher pathname &optional (checksum nil) (size nil))
  "adds a file to the given watcher, this functions is called by
   HOOK to update the watcher if a file was added or has changed"
  (unless (and checksum size)
    (multiple-value-bind (hash filesize) (get-file-info pathname)
      (setf checksum hash
            size filesize)))
  (stmx:atomic
   (let ((ft-hash (file-table-hash watcher)))
     (setf (gethash pathname (file-table-name watcher))
           (list checksum size))
     (let ((val (gethash checksum ft-hash)))
       (setf (gethash checksum ft-hash)
             (if val
                 (cons pathname val)
                 (list pathname)))))))

(defun remove-file (watcher pathname)
  "removes a file from the watcher, will be called bei
   HOOK if a file was removed or has changed"
  (stmx:atomic
   (let* ((ft-name (file-table-name watcher))
          (ft-hash (file-table-hash watcher))
          (checksum (car (gethash pathname ft-name))))
     (let ((new-val (remove pathname (gethash checksum ft-hash)
                            :test #'string=)))
       (if new-val
           (setf (gethash checksum ft-hash)
                 new-val)
           (remhash checksum ft-hash)))
     (remhash pathname ft-name))))

(defun update-file (watcher pathname)
  "checks if the given file under PATHNAME changed (if the checksum is
   different to the current), and if so calls REMOVE-FILE and ADD-FILE"
  (multiple-value-bind (new-checksum new-size)
      (get-file-info pathname)
    (unless (string= new-checksum
                     (car (gethash pathname (file-table-name watcher))))
      ;; in case checksum changed, we need to update
      (remove-file watcher pathname)
      (add-file watcher pathname new-checksum new-size))))

(defun hook (watcher pathname type)
  "will be called if some filesystem events occur inside the watched
   directory"
  (case type
    (:file-added (add-file watcher pathname))
    (:file-removed (remove-file watcher pathname))
    (:file-changed (update-file watcher pathname))))

(defmethod initialize-instance ((w watcher) &rest initargs)
  (declare (ignorable initargs))
  (call-next-method)

  ;; wait until watcher is alive and added all initial handles
  (loop
     :while (not (cl-fs-watcher:alive-p w))
     :do (sleep 0.01))

  ;; now add all initialy tracked files
  (loop
     :for file :in (cl-fs-watcher:get-all-tracked-files w)
     :do (add-file w file))

  ;; TODO: replace lambda with direct call
  (cl-fs-watcher:set-hook w (lambda (a b c)
                              (hook a b c))))
