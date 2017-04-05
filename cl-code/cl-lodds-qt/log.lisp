(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter *ignored-log-events*
  (list :listener
        :advertiser))

(define-widget info-log (QSplitter)
  ())

(define-subwidget (info-log info-log-list)
    (q+:make-qplaintextedit info-log)
  (qdoto info-log-list
         (q+:set-maximum-block-count (lodds.config:get-value :log-message-max))
         (q+:set-center-on-scroll t)
         (q+:set-read-only t)))

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

(define-signal (info-log add-log-msg) (string string string))

(define-slot (info-log add-log-msg) ((event string)
                                     (color string)
                                     (msg string))
  (declare (connected info-log (add-log-msg string
                                            string
                                            string)))
  (q+:append-html info-log-list
                  (format nil "<tt>[~a] <color ~a>~a</color> ~a</tt>"
                          (generate-timestamp)
                          (if (eql 0 (length color))
                              ""
                              (format nil
                                      "style=\"background-color: ~a\""
                                      color))
                          event
                          (escape-html msg))))

(defun format-log-message (event-type event-msg)
  (case event-type
    (:send-permission
     (if (> (length event-msg) 1)
         (destructuring-bind (task-id filename timeout user size &rest nil)
             event-msg
           (format nil "~{~a~^ ~}"
                   (list task-id filename timeout user size)))
         (format nil "~a" (car event-msg))))
    (:folder-download-error
     (destructuring-bind (task-id folder error-file &rest nil)
         event-msg
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
    (:client-updated
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
      (let ((color (if (not (lodds.config:get-value :show-log-type-color))
                       ""
                       (case event-type
                         ((:task-finished
                           :shared-directory
                           :config-changed
                           :send-permission
                           :info) "#1ED760")
                         ((:task-failed
                           :directory-error
                           :folder-download-error
                           :error) "#FF0000")
                         ((:task-canceled
                           :unshared-directory) "#FF5C14")
                         (t "")))))
        (signal! info-log
                 (add-log-msg string string string)
                 (format nil "~a" event-type)
                 color
                 (format-log-message event-type event-msg))))))

(defmethod update-log-message-max ((info-log info-log) new-max)
  (with-slots-bound (info-log info-log)
    (q+:set-maximum-block-count info-log-list new-max)))

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
