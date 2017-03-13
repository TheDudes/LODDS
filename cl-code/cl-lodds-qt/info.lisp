(in-package #:lodds-qt)
(in-readtable :qtools)

(defvar +info-info+ 0)
(defvar +info-speed+ 1)
(defvar +info-progress+ 2)
(defvar +info-cancel+ 3)
(defvar +info-id+ 4)

(defclass tracked-task ()
  ((widget :initarg :widget
           :documentation "The QTreeWidgetItem which displays the
           task info")
   (progressbar :initarg :progressbar
                :documentation "The QProgressBar inside the
                QTreeWidgetItem")
   (max :initarg :max
        :documentation "Total load of the task")
   (last-load :initarg :last-load
              :documentation "The load from the last tick")
   (started-tracking :initform (lodds.core:get-timestamp)
                     :documentation "Time we started tracking. used to
                     calculate the average Transfer speed. I know
                     seconds might be a bit vague, but since its ever
                     only usefull on long running tasks (> 1 min) the
                     error wont matter.")))

(define-widget info (QTreeWidget)
  ((tracked-tasks :initform (make-hash-table :test #'equal)
                  :type hashtable
                  :documentation "hashtable which has task-id as key
                  and tracked-task as value.")
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
           (cancel (q+:make-qpushbutton info)))
      (set-icon cancel "x.png" "Cancel")
      (qdoto progress
             (q+:set-maximum 100)
             (q+:set-value (normalized-value max done)))
      (q+:set-item-widget info
                          new-entry
                          +info-progress+
                          progress)
      (connect cancel "pressed()"
               (lambda ()
                 (lodds.task:cancel-task id nil)))
      (q+:set-item-widget info
                          new-entry
                          +info-cancel+
                          cancel)
      (qdoto new-entry
             (q+:set-text-alignment +info-speed+
                                    (qt:enum-or (q+:qt.align-center)
                                                (q+:qt.align-right)))
             (q+:set-text +info-info+ info-text)
             (q+:set-text +info-id+ id))
      (setf (gethash id tracked-tasks)
            (make-instance 'tracked-task
                           :last-load done
                           :max max
                           :progressbar progress
                           :widget new-entry)))))

(defmethod update-info ((info info) id new-max done info-text time-vanished)
  (with-slots-bound (info info)
    (with-slots (widget progressbar max last-load started-tracking)
        (gethash id tracked-tasks)
      ;; we might have pulled the info before the max-load was set on
      ;; task, check if this is the case and update max
      (unless (eql new-max max)
        (setf max new-max))
      (q+:set-value progressbar
                    (normalized-value max done))
      (qdoto widget
             (q+:set-text +info-info+ info-text)
             (q+:set-text +info-speed+
                          (format nil "~a/s"
                                  (lodds.core:format-size
                                   (round
                                    (* (/ 1000 time-vanished)
                                       (- done last-load))))))
             (q+:set-status-tip +info-info+
                                (format nil
                                        "Total: ~a (~:d bytes) | Transfered: ~a (~:d bytes) | Average Speed: ~a"
                                        (lodds.core:format-size max) max
                                        (lodds.core:format-size done) done
                                        (let ((total-time-vanished (- (lodds.core:get-timestamp)
                                                                      started-tracking)))
                                          (format nil "~a/s"
                                                  (lodds.core:format-size
                                                   (round
                                                    (/ done
                                                       (if (eql 0 total-time-vanished)
                                                           1
                                                           total-time-vanished))))))))
             (q+:set-tool-tip +info-info+
                              (format nil
                                      "Total: ~a (~:d bytes)~%Transfered: ~a (~:d bytes)"
                                      (lodds.core:format-size max) max
                                      (lodds.core:format-size done) done)))
      (setf last-load done))))

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
                (lodds:get-subsystem :tasker)))
        (time-vanished (lodds.config:get-value :info-update-interval)))
    ;; add all missing, and update all we already have
    (loop :for (id max done type info-text) :in tasks
          :do (if (gethash id tracked-tasks)
                  (update-info info id max done info-text time-vanished)
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
                   (with-slots (widget progressbar max last-load) entry
                     (declare (ignore progressbar max last-load))
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
          (update-color id "#1ED760" "FINISHED"))
        (dolist (id failed)
          (update-color id "#FF0000" "FAILED"))
        (dolist (id canceled)
          (update-color id "#FF5C14" "CANCELED"))))))

(define-initializer (info setup-widget)
  (qdoto info
         (q+:set-mouse-tracking t)
         (q+:set-object-name "Info")
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 5)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Info" "Speed" "Progress" "Stop" "ID"))
         (q+:hide-column +info-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t))
  (qdoto (q+:header info)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +info-info+ (q+:qheaderview.stretch))
         (q+:resize-section +info-speed+ 82)
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
                                (push (car event) canceled-tasks)))
                            :task-canceled)
  (lodds.event:add-callback :qt-info
                            (lambda (event)
                              (bt:with-recursive-lock-held ((slot-value info 'lock))
                                (push (car event) failed-tasks)))
                            :task-failed))

(define-finalizer (info cleanup-widget)
  (q+:clear info))

(define-finalizer (info cleanup-callbacks)
  (lodds.event:remove-callback :qt-info :task-canceled)
  (lodds.event:remove-callback :qt-info :task-failed))
