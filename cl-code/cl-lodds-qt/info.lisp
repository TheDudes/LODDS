(in-package #:lodds-qt)
(in-readtable :qtools)

(defvar +info-info+ 0)
(defvar +info-progress+ 1)
(defvar +info-cancel+ 2)
(defvar +info-id+ 3)

(define-widget info (QTreeWidget)
  ((tracked-tasks :initform (make-hash-table :test #'equalp)
                  :type hashtable
                  :documentation "hashtable which has task-id as key
                  and a list out of the treewidgetitem and the
                  displayed progressbar.")
   (lock :initform (bt:make-recursive-lock "info-lock")
         :documentation "lock to synchronize
         failed/canceled/finished-tasks")
   (failed-tasks :initform (list)
                 :type list
                 :documentation "hashtable containing failed tasks")
   (canceled-tasks :initform (list)
                   :type list
                   :documentation "hashtable containing failed tasks")
   (finished-tasks :initform (list)
                   :type list
                   :documentation "hashtable containing failed
                   tasks")
   (old-tasks :initform (list)
              :type list
              :documentation "Tasks which will be removed on next
              tick")))

(define-subwidget (info timer) (q+:make-qtimer info))

(defmethod add-info ((info info) id max done info-text)
  (with-slots-bound (info info)
    (let* ((new-entry (q+:make-qtreewidgetitem info))
           (progress (q+:make-qprogressbar info))
           (cancel (q+:make-qpushbutton "Cancel" info)))
      (qdoto progress
             (q+:set-maximum max)
             (q+:set-value done))
      (q+:set-item-widget info
                          new-entry
                          +info-progress+
                          progress)
      (connect cancel "pressed()"
               (lambda ()
                 (lodds.task:cancel-task id)))
      (q+:set-item-widget info
                          new-entry
                          +info-cancel+
                          cancel)
      (qdoto new-entry
             (q+:set-text +info-info+ info-text)
             (q+:set-text +info-id+ id))
      (setf (gethash id tracked-tasks) (list new-entry
                                             progress)))))

(defmethod update-info ((info info) id max done info-text)
  (with-slots-bound (info info)
    (destructuring-bind (widget progress) (gethash id tracked-tasks)
      (q+:set-value progress done)
      (q+:set-text widget +info-info+ info-text))))

(defmethod remove-info ((info info) id)
  (with-slots-bound (info info)
    (let ((root (q+:invisible-root-item info)))
      (do-childs (element index root)
        (when (equalp (q+:text element +info-id+)
                      id)
          (finalize (q+:take-child root index))
          (remhash id tracked-tasks)
          (return-from remove-info))))))

(define-slot (info tick) ()
  (declare (connected timer (timeout)))
  (let ((tasks (lodds.task:get-task-progresses
                (lodds:get-subsystem :tasker))))
    ;; add all missing, and update all we already have
    (loop :for (id max done type info-text) :in tasks
          :do (if (gethash id tracked-tasks)
                  (update-info info id max done info-text)
                  (unless (eql type :task)
                    (add-info info id max done info-text))))
    ;; remove all old tasks
    (dolist (id old-tasks)
      (when (gethash id tracked-tasks)
        (remove-info info id)))
    (setf old-tasks (list))
    (let ((finished nil)
          (failed nil)
          (canceled nil))
      ;; remove tasks
      (bt:with-recursive-lock-held (lock)
        (setf finished finished-tasks
              failed failed-tasks
              canceled canceled-tasks)
        (setf finished-tasks (list)
              failed-tasks (list)
              canceled-tasks (list)))
      (flet ((update-color (id color status)
               (let ((entry (gethash id tracked-tasks)))
                 (when entry
                   (destructuring-bind (widget progress) entry
                     (declare (ignore progress))
                     (q+:remove-item-widget info widget +info-progress+)
                     (let ((label (q+:make-qlabel (format nil "<b><font color=\"~a\">~a</font></b>"
                                                          color
                                                          status)
                                                  info)))
                       (q+:set-alignment label (q+:qt.align-center))
                       (q+:set-item-widget info widget +info-progress+
                                           label)))
                   (push id old-tasks)))))
        (dolist (id finished)
          (let ((entry (gethash id tracked-tasks)))
            (when entry
              (destructuring-bind (widget progress) entry
                (declare (ignore widget))
                (setf (q+:value progress)
                      (q+:maximum progress)))
              (push id old-tasks))))
        (dolist (id failed)
          (update-color id "#FF0000" "FAILED"))
        (dolist (id canceled)
          (update-color id "#FF5C14" "CANCELED"))))))

(define-initializer (info setup-widget)
  (qdoto info
         (q+:set-object-name "Info")
         (q+:set-column-count 4)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Info" "Progress" "Stop" "ID"))
         (q+:hide-column +info-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t))
  (qdoto (q+:header info)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +info-info+ (q+:qheaderview.stretch))
         (q+:resize-section +info-progress+ 150)
         (q+:set-resize-mode +info-cancel+ (q+:qheaderview.resize-to-contents))))

(define-initializer (info setup-timer)
  ;; TODO: get/set timeout from settings
  (q+:start timer 100))

(define-initializer (info setup-callbacks)
  (lodds.event:add-callback :qt-info
                            (lambda (event)
                              (bt:with-recursive-lock-held ((slot-value info 'lock))
                                (push (second event) canceled-tasks)))
                            :task-canceled)
  (lodds.event:add-callback :qt-info
                            (lambda (event)
                              (bt:with-recursive-lock-held ((slot-value info 'lock))
                                (push (second event) failed-tasks)))
                            :task-failed)
  (lodds.event:add-callback :qt-info
                            (lambda (event)
                              (bt:with-recursive-lock-held ((slot-value info 'lock))
                                (push (second event) finished-tasks)))
                            :task-finished))

(define-finalizer (info cleanup-widget)
  (q+:clear info))

(define-finalizer (info cleanup-callbacks)
  (lodds.event:remove-callback :qt-info :task-canceled)
  (lodds.event:remove-callback :qt-info :task-failed)
  (lodds.event:remove-callback :qt-info :task-finished))
