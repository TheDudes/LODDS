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
                  displayed progressbar.")))

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
    (destructuring-bind (widget progress) (gethash id tracked-tasks)
      (declare (ignore widget))
      (let ((root (q+:invisible-root-item info)))
        (do-childs (element index root)
          (when (equalp (q+:text element +info-id+)
                        id)
            (finalize (q+:take-child root index))
            ;; (finalize progress)
            (remhash id tracked-tasks)
            (return-from remove-info)))))))

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
    ;; check all which have been removed
    (loop :for id :being :the :hash-keys :of tracked-tasks
          :using (hash-value (widget progress))
          :do (unless (find id tasks :key #'car :test #'equalp)
                (if (eql (q+:value progress)
                         (q+:maximum progress))
                    (remove-info info id)
                    (setf (q+:value progress)
                          (q+:maximum progress)))))))

(define-initializer (info setup-widget)
  (qdoto info
         (q+:set-column-count 4)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Info" "Progress" "Stop" "ID"))
         (q+:hide-column +info-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t)))

(define-initializer (info setup-timer)
  ;; TODO: get/set timeout from settings
  (q+:start timer 100))

(define-finalizer (info cleanup-widget)
  (q+:clear info))
