(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter *ignored-log-events*
  (list :listener
        :advertiser))

(define-widget info-log (QSplitter)
  ())

(define-subwidget (info-log info-log-list)
    (make-instance 'text-stream
                   :html-mode t
                   :maximum-block-count (lodds.config:get-value :log-message-max)
                   :font (get-font (lodds.config:get-value :log-font))))

(define-subwidget (info-log info-log-settings) (q+:make-qscrollarea info-log)
  (let* ((container (q+:make-qgroupbox "Log Settings" info-log))
         (layout (q+:make-qvboxlayout container)))
    (dolist (event +events+)
      (let ((checkbox (q+:make-qcheckbox (cl-strings:title-case (string event))
                                         info-log)))
        (q+:set-check-state checkbox
                            (if (find event *ignored-log-events*)
                                (q+:qt.unchecked)
                                (q+:qt.checked)))
        (connect checkbox "stateChanged(int)"
                 (lambda (new-state)
                   (case new-state
                     (0 (push event *ignored-log-events*))
                     (2 (setf *ignored-log-events*
                              (remove event *ignored-log-events*))))))
        (q+:add-widget layout checkbox)))
    (q+:set-widget info-log-settings container)
    (q+:set-maximum-width info-log-settings 250)))

(define-signal (info-log add-log-msg) (string string))

(define-slot (info-log add-log-msg) ((event string)
                                     (msg string))
  (declare (connected info-log (add-log-msg string string)))
  (let ((ts (lodds.core:format-timestamp)))
    (apply #'format info-log-list
           (if (html-mode info-log-list)
               (list "[~a] <color ~a>~a</color> ~a~%"
                     ts
                     (format nil
                             "style=\"background-color: ~a\""
                             (or (lodds.config:get-log-event-color event)
                                 (lodds.config:get-value :log-default-color)))
                     event
                     (escape-html msg))
               (list "[~a] ~a ~a~%"
                     ts
                     event
                     msg)))))

(defun format-log-message (event-type event-msg)
  (case event-type
    (:send-permission
     (if (> (length event-msg) 1)
         (destructuring-bind (task-id filename timeout user size &rest ignored)
             event-msg
           (declare (ignore ignored))
           (format nil "~{~a~^ ~}"
                   (list task-id filename timeout user size)))
         (format nil "~a" (car event-msg))))
    (:folder-download-error
     (destructuring-bind (task-id folder error-file &rest ignored)
         event-msg
       (declare (ignore ignored))
       (format nil "~{~a~^ ~}"
               (list task-id folder error-file))))
    (:list-update
     (destructuring-bind (name type ts changes)
         event-msg
       (let ((adds 0)
             (dels 0))
         (loop :for (type . rest) :in changes
               :if (eql type :add)
               :do (incf adds)
               :else
               :do (incf dels))
         (format nil "~a ~a ~a adds: ~a dels: ~a"
                 name type ts adds dels))))
    (:user-updated
     (destructuring-bind (name load last-change)
         event-msg
       (format nil "~a ~a ~a"
               name
               (lodds.core:format-size load)
               last-change)))
    (:config-changed
     (if (not event-msg)
         "replaced settings"
         (destructuring-bind (key old-val new-val) event-msg
           (format nil "Settings changed: ~a: ~a -> ~a"
                   key old-val new-val))))
    (t (format nil "~{~a~^ ~}" event-msg))))

(defun cb-log-messages (info-log event)
  (let ((event-type (first event))
        (event-msg (cdr event)))
    (unless (find event-type *ignored-log-events*)
      (signal! info-log (add-log-msg string string)
               (format nil "~a" event-type)
               (format-log-message event-type event-msg)))))

(defmethod update-log-message-max ((info-log info-log) new-max)
  (with-slots-bound (info-log info-log)
    (setf (maximum-block-count info-log-list) new-max)))

(defmethod update-font ((info-log info-log) new-font)
  (with-slots-bound (info-log info-log)
    (setf (font info-log-list)
          (get-font new-font))))

(defmethod update-html-mode ((info-log info-log) html-mode)
  (with-slots-bound (info-log info-log)
    (setf (html-mode info-log-list) html-mode)))

(define-initializer (info-log setup-widget)
    (qdoto info-log
           (q+:add-widget info-log-settings)
           (q+:add-widget info-log-list)))

(define-initializer (info-log setup-callbacks)
  (lodds.event:add-callback :qt-log
                            (lambda (event)
                              (cb-log-messages info-log event))))

(define-finalizer (info-log cleanup-callbacks)
  (lodds.event:remove-callback :qt-log))
