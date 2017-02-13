(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget dialog (QDialog)
  ((on-success :initform nil
               :initarg :on-success-fn
               :documentation "function which gets called when 'Ok'
               was clicked")
   (on-cancel :initform nil
              :initarg :on-cancel-fn
              :documentation "function which gets called when 'Cancel'
              was clicked")))

(define-subwidget (dialog message) (q+:make-qlabel dialog))

(define-subwidget (dialog ok) (q+:make-qpushbutton "Ok" dialog))

(define-subwidget (dialog cancel) (q+:make-qpushbutton "Cancel" dialog))

(defmethod finish-dialog ((dialog dialog) result)
  (with-slots-bound (dialog dialog)
    (let ((fn (case result
                (:success on-success)
                (:cancel on-cancel)
                (t nil))))
      (when fn
        (funcall fn))
      (finalize dialog))))

(define-slot (dialog ok-pressed) ()
  (declare (connected ok (pressed)))
  (finish-dialog dialog :success))

(define-slot (dialog cancel-pressed) ()
  (declare (connected cancel (pressed)))
  (finish-dialog dialog :cancel))

(define-subwidget (dialog button-area) (q+:make-qwidget dialog)
  (let ((layout (q+:make-qhboxlayout button-area)))
    (qdoto layout
           (q+:add-widget cancel)
           (q+:add-widget ok))))

(define-subwidget (dialog layout) (q+:make-qvboxlayout dialog)
  (q+:add-widget layout message))

(defmethod initialize-instance :after ((dialog dialog) &key
                                                         (title "Dialog")
                                                         (text "Text")
                                                         (widget nil))
  (with-slots-bound (dialog dialog)
    (q+:set-window-title dialog title)
    (q+:set-text message text)
    (when widget
      (q+:add-widget layout widget))
    (q+:add-widget layout button-area)))

(define-initializer (dialog setup-widget)
  (q+:set-attribute dialog (q+:qt.wa_delete-on-close))
  (q+:show dialog))
