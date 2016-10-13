;;;; watcher.lisp

(in-package #:lodds.watcher)

;;; "lodds.watcher" goes here. Hacks and glory await!

(defclass watcher ()
  ((directory "/what/iam/watching"
              :type string)
   (hooks :type list
          :accessor hooks
          :documentation "List of functions which will be called if a
                          file changed or was renamed/moved. Use
                          ADD-HOOK to add a callback. The callback
                          function will be called with the following
                          arguments:

                          watcher
                          filename
                          handl
                          renamed-p
                          changed-p")
   (file-added-hooks :type list
                     :reader file-added-hooks)
   (file-removed-hooks :type list
                       :reader file-removed-hooks)
   (file-changed-hooks :type list
                       :reader file-changed-hooks)
   (directory-added-hooks :type list
                          :reader directory-added-hooks)
   (directory-removed-hooks :type list
                            :reader directory-removed-hooks)
   (on-delete :tyle function)
   (directory-handles :type list
                      (("some/subdir" handle) ;; hash-table
                       ("some/other/subdir" handle)
                       ...))
   (recursive-p :type boolean
                :accessor recursive-p)))

(defgeneric callback (watcher filename handle rename-p changed-p))

(defmethod callback ((watcher watcher-bare)
                     (filename string)
                     (handle cl-async:fs-monitor)
                     renamed-p
                     changed-p)
  ;; TODO: check if recursive, add del directory handler, etc etc
  (let ((callbacks (hooks watcher)))
    (when callbacks
      (mapcar
       (lambda (hook)
         (funcall hook watcher filename handle renamed-p changed-p))
       callbacks))))

(defmethod callback ((watcher watcher)
                     (filename string)
                     (handle cl-async:fs-monitor)
                     renamed-p
                     changed-p)
  ;;handle-stuff-here
  (if (recursive-p watcher)))

(stmx:transactional
 (defclass watcher-lodds (watcher)
   (watcher-object-lodds
    ((file-table-name :type hashtable
                      (("name"     ("checksum" size ))))
     (file-table-hash :type hashtable
                      (("checksum" ("name" ...))))))))

(defmethod rem-file (name)
  (stmx:atomic
   (let ((data (gethash name file-table-name)))
     (remhash name file-table-name)
     (let ((new-list (delete name (gethash data.hash file-table-hash)))
           (if (null new-list)
               (remhash data.hash file-table-hash)
               (setf (gethash data.hash new-list))))))))

(defmethod get-file-infos (checksum &key (all nil))
  (if all
      (stmx:atomic
       (mapcar
        (lambda (name)
          (gethash name file-table-name))
        (gethash hash file-table-hash)))
      (stmx:atomic
       (gethash (car (gethash hash file-table-hash))
                file-table-name))))
