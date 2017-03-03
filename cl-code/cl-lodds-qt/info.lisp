(in-package #:lodds-qt)
(in-readtable :qtools)

(defvar +info-info+ 0)
(defvar +info-progress+ 1)
(defvar +info-cancel+ 2)
(defvar +info-id+ 3)

(define-widget info (QTreeWidget)
  ((tracked-tasks :initform (make-hash-table :test #'equal)
                  :type hashtable
                  :documentation "hashtable which has task-id as key
                  and a list out of the treewidgetitem, the
                  displayed progressbar and max.")
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

(defun normalized-value (max done)
  (if (= 0 max)
      0
      (round (/ (* 100 done) max))))

(defmethod add-info ((info info) id max done info-text)
  (with-slots-bound (info info)
    (let* ((new-entry (q+:make-qtreewidgetitem info))
           (progress (q+:make-qprogressbar info))
           (cancel (q+:make-qpushbutton "Cancel" info)))
      (qdoto progress
             (q+:set-maximum 100)
             (q+:set-value (normalized-value max done)))
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
                                             progress
                                             max)))))

(defmethod update-info ((info info) id done info-text)
  (with-slots-bound (info info)
    (destructuring-bind (widget progress max) (gethash id tracked-tasks)
      (q+:set-value progress
                    (normalized-value max done))
      (qdoto widget
             (q+:set-text +info-info+ info-text)
             (q+:set-tool-tip +info-info+
                              (format nil
                                      "Total: ~a (~:d bytes)~%Transfered: ~a (~:d bytes)"
                                      (lodds.core:format-size max) max
                                      (lodds.core:format-size done) done))))))

(defmethod remove-info ((info info) id)
  (with-slots-bound (info info)
    (let ((root (q+:invisible-root-item info)))
      (do-childs (element index root)
        (when (equal (q+:text element +info-id+)
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
                  (update-info info id done info-text)
                  (when info-text
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
      (loop :for task :being :the :hash-key :of tracked-tasks
            :do (when (not (find task tasks
                                 :test #'string=
                                 :key #'car))
                  (push task finished)))
      (flet ((update-color (id color status)
               (let ((entry (gethash id tracked-tasks)))
                 (when entry
                   (destructuring-bind (widget progress max) entry
                     (declare (ignore progress max))
                     (let ((old-widget (q+:item-widget info widget +info-progress+)))
                       (q+:remove-item-widget info widget +info-progress+)
                       (finalize old-widget))
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
              (destructuring-bind (widget progress max) entry
                (declare (ignore widget max))
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
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 4)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Info" "Progress" "Stop" "ID"))
         (q+:hide-column +info-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t))
  (qdoto (q+:header info)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +info-info+ (q+:qheaderview.stretch))
         (q+:resize-section +info-progress+ 150)
         (q+:set-resize-mode +info-cancel+ (q+:qheaderview.resize-to-contents))))

(define-initializer (info setup-timer)
  (q+:start timer (lodds.config:get-value :info-update-interval)))

(defmethod set-refresh-timeout ((info info) new-timeout)
  (with-slots-bound (info info)
    (q+:set-interval timer new-timeout)))

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
                            :task-failed))

(define-finalizer (info cleanup-widget)
  (q+:clear info))

(define-finalizer (info cleanup-callbacks)
  (lodds.event:remove-callback :qt-info :task-canceled)
  (lodds.event:remove-callback :qt-info :task-failed))
