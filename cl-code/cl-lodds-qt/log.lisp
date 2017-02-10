(in-package #:lodds-qt)
(in-readtable :qtools)

;; TODO: settings
(defparameter +log-message-maximum+ 1000)

(defparameter *ignored-log-events*
  (list :listener
        :advertiser))

;; info-log-list columns
(defvar +log-time+ 0)
(defvar +log-event+ 1)
(defvar +log-message+ 2)
(defvar +log-count+ 3)

(define-widget info-log (QSplitter) ())

(define-subwidget (info-log info-log-list) (q+:make-qtreewidget info-log)
  (qdoto info-log-list
         (q+:set-column-count 4)
         (q+:set-header-labels (list "Time" "Event" "Message" ""))
         (q+:set-alternating-row-colors t))

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
      (let ((checkbox (q+:make-qcheckbox (cl-strings:title-case (string (car event)))
                                         info-log)))
        (q+:set-check-state checkbox
                            (if (find (car event) *ignored-log-events*)
                                (q+:qt.unchecked)
                                (q+:qt.checked)))
        (connect checkbox "stateChanged(int)"
                 (lambda (new-state)
                   (case new-state
                     (0 (push (car event) *ignored-log-events*))
                     (2 (setf *ignored-log-events*
                              (remove (car event) *ignored-log-events*))))))
        (q+:add-widget layout checkbox)))
    (q+:set-widget info-log-settings container)))

(defun cb-log-messages (info-log event)
  (let ((event-type (first event))
        (event-msg (cdr event)))
    (unless (find event-type *ignored-log-events*)
      (signal! info-log
               (add-log-msg string string)
               (format nil "~a" event-type)
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
                 (t (format nil "~{~a~^ ~}" event-msg)))))))

(define-signal (info-log add-log-msg) (string string))

(define-slot (info-log add-log-msg) ((event string)
                                     (msg string))
  (declare (connected info-log (add-log-msg string
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
        (qdoto last-item
               (q+:set-text +log-time+ (generate-timestamp))
               (q+:set-text +log-count+
                            (prin1-to-string
                             (let ((current (q+:text last-item +log-count+)))
                               (if (string= current "")
                                   2
                                   (+ 1 (parse-integer current)))))))
        (let ((new-entry (qdoto (q+:make-qtreewidgetitem info-log-list)
                                (q+:set-text +log-time+ (generate-timestamp))
                                (q+:set-text +log-event+ event)
                                (q+:set-text +log-message+ msg)
                                (q+:set-text +log-count+ "")
                                (q+:set-text-alignment +log-count+ (q+:qt.align-right)))))
          (let* ((scrollbar (q+:vertical-scroll-bar info-log-list))
                 (position (q+:value scrollbar)))
            (loop :while (> (q+:top-level-item-count info-log-list) +log-message-maximum+)
                  :do (progn (finalize (q+:take-top-level-item info-log-list 0))
                             (q+:set-value scrollbar (- position 1)))))
          (let ((visual-rect (if last-item
                                 (q+:visual-item-rect info-log-list last-item)
                                 nil)))
            (when (and visual-rect
                       (< (- (q+:bottom visual-rect)
                             (q+:height (q+:viewport info-log-list)))
                          (q+:height visual-rect)))
              (q+:scroll-to-item info-log-list new-entry)))))))

(define-insitializer (info-log setup-widget)
    (qdoto info-log
           (q+:add-widget info-log-settings)
           (q+:add-widget info-log-list)))

(define-initializer (info-log setup-callbacks)
  (lodds.event:add-callback :qt-info-log
                            (lambda (event)
                              (cb-log-messages info-log event))))

(define-finalizer (info-log cleanup-callbacks)
  (lodds.event:remove-callback :qt-info-log))
