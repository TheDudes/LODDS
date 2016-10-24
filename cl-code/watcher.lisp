;;;; watcher.lisp

(in-package #:lodds.watcher)

;;; "lodds.watcher" goes here. Hacks and glory await!

(stmx:transactional
 (defclass watcher ()
   ((dir :initform (error "specify a directory!")
         :type string
         :initarg :dir
         :reader dir
         :transactional nil)
    (thread :type bt:thread
            :reader thread
            :transactional nil)
    (hook :type function
          :initarg :hook
          :reader hook
          :initform nil)
    (directory-handles :type hash-table
                       :reader directory-handles
                       :initform (make-hash-table :test 'equal))
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
           :directory-added))))

(defun add-dir (watcher dir)
  "adds the specified dir to watcher, this funciton has to be called
   from the watcher-thread!"
  (stmx:atomic
   (let ((table (directory-handles watcher)))
     (multiple-value-bind (value present-p) (gethash dir table)
       (declare (ignore value))
       (when present-p
         (format t "ERROR: Key was already Present!~%")))
     (setf (gethash dir table)
           (as:fs-watch dir
                        (lambda (h f e s)
                          (callback watcher h f e s)))))))

(defun add-directory-to-watch (watcher dir)
  "adds dir to watcher, can be safetly called by any thread, will
   interrupt watcher-thread."
  (when (pathnamep dir)
    (format t "ERROR: add-directory-to-watch: dir is pathnamep, this should not happen!~%")
    (setf dir (format nil "~a" dir)))
  (unless (char= #\/ (aref dir (- (length dir) 1)))
    (format t "ERROR: add-directory-to-watch: dir had no trailing /~%")
    (setf dir (concatenate 'string dir "/")))
  (if (eql (bt:current-thread)
           (thread watcher))
      (add-dir watcher dir)
      (bt:interrupt-thread (thread watcher)
                           #'add-dir watcher dir)))

(defun remove-directory-from-watch (watcher dir)
  "adds dir to watcher, can be safetly called by any thread, will
   interrupt watcher-thread."
  (when (pathnamep dir)
    (format t "ERROR: remove-directory-to-watch: this should not happen~%")
    (setf dir (format nil "~a" dir)))
  (let* ((table (directory-handles watcher))
         (handle (gethash dir table)))
    (as:fs-unwatch handle)
    (stmx:atomic
     (remhash dir table))))

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
  ;; (format t "something happend!~%watcher: ~a~%filename: ~a~%handle:
  ;; ~a~%renamed-p, changed-p: ~a, ~a~%~%"
  ;;         watcher filename handle renamed-p changed-p)
  (let ((event-type nil)
        (full-filename (concatenate 'string
                                    (get-handle-path handle)
                                    filename)))
    (if (eql 0 (length filename))
        (if (equalp (dir watcher)
                    full-filename)
            ;; main directory got deleted
            (setf event-type :on-deleted)
            ;; if it wasnt the main directory it means
            ;; a subdirectory was removed, so ignore it, it will be
            ;; handled by the event the handle above gets.
            (return-from callback))
        ;; some other event besides :on-deleted and :directory-removed
        (setf event-type (get-event-type full-filename renamed-p changed-p)))
    ;; lets check if a directory was removed, just add a trailing /
    ;; and see if its inside directory-handles
    (when (eql event-type :file-removed)
      (let ((dir-name (concatenate 'string full-filename "/")))
        (multiple-value-bind (value present-p)
            (gethash dir-name (directory-handles watcher))
          (declare (ignore value))
          (when present-p
            (setf event-type :directory-removed)))))
    ;; in case a directoy was added/removed add a trailing /
    (when (or (eql event-type :directory-added)
              (eql event-type :directory-removed))
      (setf full-filename (concatenate 'string full-filename "/")))
    ;; if watcher is recursive-p add/remove dir
    (when (recursive-p watcher)
      (case event-type
        (:directory-added
         (add-directory-to-watch watcher full-filename))
        (:directory-removed
         (remove-directory-from-watch watcher full-filename))))
    ;; if watcher directory got removed, remove its handle too, so
    ;; that the event loop can finish
    (when (eql event-type :on-deleted)
      (remove-directory-from-watch watcher full-filename))
    ;; lets check if hook is set, and if so call it
    (let ((fn (hook watcher)))
      (when fn
        (funcall fn watcher full-filename event-type)))))

(defun watcher-event-loop (watcher)
  "Watcher event loop, will be called by the watcher thread. adds the
   initial directories. This thread will get interrupted by
   add-directory-to-watch-dir if a new directory is added."
  (let ((initial-directories (list)))
    (if (recursive-p watcher)
        (uiop:collect-sub*directories (pathname (dir watcher))
                                      t
                                      t
                                      (lambda (dir) (push (format nil "~a" dir)
                                                          initial-directories)))
        (push (dir watcher) initial-directories))
    (as:with-event-loop (:catch-app-errors t)
      (loop
         :for dir :in initial-directories
         :do (add-dir watcher dir))))) ;; we can directly call add-dir here,
                               ;; since we are inside the event-loop
                               ;; thread

(defmethod initialize-instance ((w watcher) &rest initargs)
  (call-next-method)
  ;; get fullpath as string
  (let ((fullpath (car (directory (getf initargs :dir)))))
    (unless fullpath
      (error "TODO: ERROR: The given Directory does not exist (or is
              fishy). calling DIRECTORY on it returned NIL."))
    (setf (slot-value w 'dir) (format nil "~a" fullpath)))
  ;; add hook to call callback with watcher and args
  (setf (slot-value w 'thread)
        (bt:make-thread (lambda () (watcher-event-loop w))
                        :name "directory-watcher")))

(defun set-hook (watcher hook-fn)
  "WATCHER is the watcher object the HOOK-FN should be set to.
   If a hook was already set, it will be overwritten!

   hook-fn should be a function witch takes 3 arguments, it will be called with the
   watcher object, the pathname and the type of event.
   the event type is one of the following:
   :file-added
   :file-removed
   :file-changed
   :directory-added
   :directory-removed
   :on-deleted

   If a directory is added and RECURSIVE-P is true, the directory will
   automatically be added to the watched list."
  (unless (bt:thread-alive-p (thread watcher))
    (format t "TODO: set-hook was called while watcher was not running! fixmeee~%")
    (return-from set-hook))
  (stmx:atomic
   (setf (slot-value watcher 'hook)
         hook-fn)))

(defun stop-watcher (watcher)
  (let ((table (directory-handles watcher)))
    (loop :for handle :being :the :hash-key :of table
       :do (progn
             (as:fs-unwatch (gethash handle table))
             (stmx:atomic
              (remhash handle table))))
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
