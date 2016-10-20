;;;; watcher.lisp

(in-package #:lodds.watcher)

;;; "lodds.watcher" goes here. Hacks and glory await!

(stmx:transactional
 (defclass watcher ()
   ((dir :initform (error "specify a directory!")
         :type pathname
         :initarg :dir
         :reader dir
         :transactional nil)
    (thread :type bt:thread
            :reader thread
            :transactional nil)
    (hooks :type list
           :reader hooks
           :initform '((:file-added        . nil)
                       (:file-added        . nil)
                       (:file-removed      . nil)
                       (:file-changed      . nil)
                       (:directory-added   . nil)
                       (:directory-removed . nil)
                       (:on-deleted        . nil)))
    (directory-handles :type hash-table
                       :reader directory-handles
                       :initform (make-hash-table :test 'equalp))
    (recursive-p :initarg :recursive-p
                 :initform nil
                 :reader recursive-p
                 :transactional nil))))

(defun get-event-type (filename renamed-p changed-p)
  (let ((file-exists-p (uiop:file-exists-p filename))
        (directory-exists-p (uiop:directory-exists-p filename)))
    (cond ((and renamed-p
                (not changed-p)
                file-exists-p
                (not directory-exists-p))
           :file-added)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                (not directory-exists-p))
           :file-removed)
          ((and (not renamed-p)
                changed-p
                file-exists-p
                (not directory-exists-p))
           :file-changed)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                directory-exists-p)
           :directory-added)
          ((and renamed-p
                (not changed-p)
                (not file-exists-p)
                (not directory-exists-p))
           :directory-removed))))

(defun add-dir (watcher dir)
  "adds the specified dir to watcher, this funciton has to be called
   from the watcher-thread!"
  (format t "adding key: ~a~%" dir)
  (stmx:atomic
   (let ((table (directory-handles watcher)))
     (multiple-value-bind (value present-p) (gethash dir table)
       (declare (ignore value))
       (when present-p
         (format t "ERROR: Key was already Present!!!!~%")))
     (setf (gethash dir table)
           (as:fs-watch dir
                        (lambda (h f e s)
                          (callback watcher h f e s)))))))

(defun add-directory-to-watch (watcher dir)
  "adds dir to watcher, can be safetly called by any thread, will
   interrupt watcher-thread."
  (if (eql (bt:current-thread)
           (thread watcher))
      (add-dir watcher dir)
      (bt:interrupt-thread (thread watcher)
                           #'add-dir watcher dir)))

(defun remove-directory-from-watch (watcher dir)
  "adds dir to watcher, can be safetly called by any thread, will
   interrupt watcher-thread."

  (let* ((table (directory-handles watcher))
         (handle (gethash dir table)))
    (as:fs-unwatch handle)
    (stmx:atomic
     (setf (gethash dir table) nil))))

(defun get-handle-path (handle)
  "gets the path of the given cl-async fs-handle."
  (let ((buffer (cffi:foreign-alloc :char
                                    :initial-element 0
                                    :count 2048))
        (size (cffi:foreign-alloc :uint
                                  :initial-element 0))
        (result nil))
    (uv:uv-fs-event-getpath (as::fs-monitor-c handle)
                            buffer
                            size)
    (setf result (cffi:foreign-string-to-lisp buffer))
    (cffi:foreign-free buffer)
    (cffi:foreign-free size)
    result))

(defun callback (watcher handle filename renamed-p changed-p)
  (format t "something happend!~%watcher: ~a~%filename: ~a~%handle: ~a~%renamed-p, changed-p: ~a, ~a~%~%"
          watcher filename handle renamed-p changed-p)
  (let ((event-type nil)
        (full-filename nil))
    (if (and (eql 0 (length filename))
             (equalp (dir watcher) (get-handle-path handle)))
        (setf event-type :on-deleted)
        (progn
          (setf full-filename (concatenate 'string
                                           (get-handle-path handle)
                                           filename))
          (setf event-type (get-event-type full-filename renamed-p changed-p))))
    (case event-type
      (:directory-added (when (recursive-p watcher)
                          (add-directory-to-watch watcher full-filename)))
      (:directory-removed (when (recursive-p watcher)
                            (remove-directory-from-watch watcher full-filename)))
      (:on-deleted
       (as:fs-unwatch handle)))
    (let ((fn (cdr (assoc event-type (hooks watcher)))))
      (when fn
        (funcall fn watcher full-filename)))))

(defun watcher-event-loop (watcher)
  "Watcher event loop, will be called by the watcher thread. adds the
   initial directories. This thread will get interrupted by
   add-directory-to-watch-dir if a new directory is added."
  (let ((initial-directories (list)))
    (if (recursive-p watcher)
        (uiop:collect-sub*directories (pathname (dir watcher))
                                      t
                                      t
                                      (lambda (dir) (push dir initial-directories)))
        (push (dir watcher) initial-directories))
    (as:with-event-loop (:catch-app-errors t)
      (loop
         :for dir :in initial-directories
         :do (add-dir watcher dir))))) ;; we can directly call add-dir here,
                               ;; since we are inside the event-loop
                               ;; thread

(defmethod initialize-instance ((w watcher) &rest initargs)
  ;; (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value w 'dir) (car (directory (getf initargs :dir))))
  ;; add hook to call callback with watcher and args
  (setf (slot-value w 'thread)
        (bt:make-thread (lambda () (watcher-event-loop w))
                        :name "directory-watcher")))

(defun add-hook (watcher event-type hook-fn)
  "WATCHER is the watcher object the HOOK-FN should be added to.

   EVENT-TYPE is one of the following:
   :file-added
   :file-removed
   :file-changed
   :directory-added
   :directory-removed
   :on-deleted

   If a directory is added and RECURSIVE-P is true, the directory will
   automatically be added to the watched list.

   HOOK-FN is the function which should be called if a file
   changed. The function will be called with the following 2 Arguments:

   filename: the path to the new file which was added. If watcher is
   RECURSIVE-P its the full path to the file."
  (unless (bt:thread-alive-p (thread watcher))
    (format t "TODO: add-hook was called while watcher was not running! fixmeee~%")
    (return-from add-hook))
  (stmx:atomic
   (setf (cdr (assoc event-type (slot-value watcher 'hooks)))
         hook-fn)))

(defun remove-hook (watcher event-type)
  (unless (bt:thread-alive-p (thread watcher))
    (format t "TODO: remove-hook was called while watcher was not running! fixmeee~%")
    (return-from remove-hook))
  (stmx:atomic
   (setf (cdr (assoc event-type (slot-value watcher 'hooks)))
         nil)))

(defun stop-watcher (watcher)
  (let ((table (directory-handles watcher)))
    (loop :for handle :being :the :hash-key :of table
       :do (as:fs-unwatch (gethash handle table)))
    (bt:join-thread (thread watcher))))

;; (stmx:transactional
;;  (defclass watcher-lodds (watcher)
;;    (watcher-object-lodds
;;     ((file-table-name :type hashtable
;;                       (("name"     ("checksum" size ))))
;;      (file-table-hash :type hashtable
;;                       (("checksum" ("name" ...))))))))

;; (defmethod rem-file (name)
;;   (stmx:atomic
;;    (let ((data (gethash name file-table-name)))
;;      (remhash name file-table-name)
;;      (let ((new-list (delete name (gethash data.hash file-table-hash)))
;;            (if (null new-list)
;;                (remhash data.hash file-table-hash)
;;                (setf (gethash data.hash new-list))))))))

;; (defmethod get-file-infos (checksum &key (all nil))
;;   (if all
;;       (stmx:atomic
;;        (mapcar
;;         (lambda (name)
;;           (gethash name file-table-name))
;;         (gethash hash file-table-hash)))
;;       (stmx:atomic
;;        (gethash (car (gethash hash file-table-hash))
;;                 file-table-name))))
