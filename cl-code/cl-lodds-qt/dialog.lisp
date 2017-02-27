(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget dialog (QDialog)
  ((ok-button-text :initform "Ok"
                   :initarg :ok-text
                   :documentation "Text which will be displayed on the
                   right \"Ok\" Button which confirms the dialog")
   (cancel-button-text :initform "Cancel"
                       :initarg :cancel-text
                       :documentation "Text which will be displayed on
                       the left\"Cancel\" Button which closes the
                       dialog without confirming it")
   (on-success :initform nil
               :initarg :on-success-fn
               :documentation "function which gets called when 'Ok' was
               clicked. Will be called with the widget as arugment. If
               on-success is set and the callback returns nil, the
               dialog wont be closed. That can be used to verify user
               input on the widget and reject success, where the user
               has a change to corret it and press OK again.")
   (on-cancel :initform nil
              :initarg :on-cancel-fn
              :documentation "function which gets called when
              'Cancel'was clicked. Will be called with the widget as
              argument")
   (widget :initform nil
           :initarg :widget
           :documentation "Widget which will be displayed on the
           dialog")
   (finalize-widget-p :initform t
                      :initarg :finalize-widget-p
                      :documentation "If t the given widget will be
                      finalized when the dialog closes")))

(define-subwidget (dialog message) (q+:make-qlabel dialog))

(define-subwidget (dialog ok) (q+:make-qpushbutton "Ok" dialog))

(define-subwidget (dialog cancel) (q+:make-qpushbutton "Cancel" dialog))

(defmethod finish-dialog ((dialog dialog) result)
  (with-slots-bound (dialog dialog)
    (case result
      (:success (if on-success
                    (when (funcall on-success widget)
                      (finalize dialog))
                    (finalize dialog)))
      (:cancel (progn
                 (when on-cancel
                   (funcall on-cancel widget))
                 (finalize dialog)))
      (t nil))))

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
                                                         (text "Text"))
  (with-slots-bound (dialog dialog)
    (q+:set-window-title dialog title)
    (q+:set-text message text)
    (q+:set-text ok ok-button-text)
    (q+:set-text cancel cancel-button-text)
    (when widget
      (q+:add-widget layout widget))
    (q+:add-widget layout button-area)))

(defmethod cancel ((dialog dialog))
  (finish-dialog dialog :cancel))

(define-initializer (dialog setup-widget)
  (q+:set-attribute dialog (q+:qt.wa_delete-on-close))
  (q+:show dialog))

(define-finalizer (dialog cleanup-widget)
  (when (and widget
             finalize-widget-p)
    (finalize widget)))
