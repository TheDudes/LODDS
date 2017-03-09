(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter *ignored-log-events*
  (list :listener
        :advertiser
        :task-finished))

;; info-log-list columns
(defvar +log-time+ 0)
(defvar +log-event+ 1)
(defvar +log-message+ 2)
(defvar +log-count+ 3)

(define-widget info-log (QSplitter)
  ((log-message-max :initform (lodds.config:get-value :log-message-max))))

(define-subwidget (info-log info-log-list) (q+:make-qtreewidget info-log)
  (qdoto info-log-list
         (q+:set-object-name "Log")
         (q+:set-column-count 4)
         (q+:set-header-labels (list "Time" "Event" "Message" "")))

  (qdoto (q+:header info-log-list)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +log-time+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +log-event+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +log-message+ (q+:qheaderview.stretch))
         (q+:set-resize-mode +log-count+ (q+:qheaderview.resize-to-contents))))

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
    (q+:set-widget info-log-settings container)))

(define-signal (info-log add-log-msg) (string string string string))

(define-slot (info-log add-log-msg) ((event string)
                                     (color string)
                                     (msg string)
                                     (long-msg string))
  (declare (connected info-log (add-log-msg string
                                            string
                                            string
                                            string)))
  (let* ((items (q+:top-level-item-count info-log-list))
         (last-item (if (> items 0)
                        (q+:top-level-item info-log-list (- items 1))
                        nil)))
    (if (and last-item
             (string= (q+:text last-item +log-event+)
                      event)
             (string= (q+:text last-item +log-message+)
                      msg))
        (let* ((current (q+:text last-item +log-count+))
               (next (if (string= current "")
                         2
                         (+ 1 (parse-integer current)))))
          ;; TODO: this might be very unperformant
          (with-finalizing* ((color (q+:make-qcolor (format nil "#~2,'0X0000"
                                                            (if (> next 255)
                                                                255
                                                                next))))
                             (brush (q+:make-qbrush color)))
            (qdoto last-item
                   (q+:set-text +log-time+ (generate-timestamp))
                   (q+:set-text +log-count+ (prin1-to-string next))
                   (q+:set-foreground +log-count+ brush))))
        (let ((new-entry (qdoto (q+:make-qtreewidgetitem info-log-list)
                                (q+:set-tool-tip +log-message+
                                                 long-msg)
                                (q+:set-text +log-time+ (generate-timestamp))
                                (q+:set-text +log-event+ event)
                                (q+:set-text +log-message+ msg)
                                (q+:set-text +log-count+ "")
                                (q+:set-text-alignment +log-count+ (q+:qt.align-right)))))
          (unless (eql 0 (length color))
            (with-finalizing* ((qcolor (q+:make-qcolor color))
                               (qbrush (q+:make-qbrush qcolor)))
              (q+:set-background new-entry +log-event+ qbrush)))
          (let* ((scrollbar (q+:vertical-scroll-bar info-log-list))
                 (position (q+:value scrollbar)))
            (loop :while (> (q+:top-level-item-count info-log-list) log-message-max)
                  :do (progn
                        (finalize (q+:take-top-level-item info-log-list 0))
                        (q+:set-value scrollbar (- position 1)))))
          (unless (q+:is-empty (q+:visible-region info-log))
            (let ((visual-rect (if last-item
                                   (q+:visual-item-rect info-log-list last-item)
                                   nil)))
              (when (and visual-rect
                         (< (- (q+:bottom visual-rect)
                               (q+:height (q+:viewport info-log-list)))
                            (q+:height visual-rect)))
                (q+:scroll-to-item info-log-list new-entry))))))))

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
                           :send-permission) "#1ED760")
                         ((:task-failed
                           :directory-error
                           :folder-download-error) "#FF0000")
                         (:task-canceled "#FF5C14")
                         (t "")))))
        (signal! info-log
                 (add-log-msg string string string string)
                 (format nil "~a" event-type)
                 color
                 (case event-type
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
                          (format nil "~a: ~a -> ~a"
                                  key old-val new-val))))
                   (t (format nil "~{~a~^ ~}" event-msg)))
                 (if (eql event-type :list-update)
                     (let* ((changes (fourth event-msg))
                            (len (length changes)))
                       (if (> len 50)
                           (format nil "...~{~%~{~a~^ ~}~}" (subseq changes (- len 50)))
                           (format nil "~{~{~a~^ ~}~^~%~}" changes)))
                     (format nil "~{~a~^ ~}" event-msg)))))))

(define-initializer (info-log setup-widget)
    (qdoto info-log
           (q+:add-widget info-log-settings)
           (q+:add-widget info-log-list)))

(define-initializer (info-log setup-callbacks)
  (lodds.event:add-callback :qt-info-log
                            (lambda (event)
                              (cb-log-messages info-log event))))

(define-finalizer (info-log cleanup-callbacks)
  (lodds.event:remove-callback :qt-info-log))
