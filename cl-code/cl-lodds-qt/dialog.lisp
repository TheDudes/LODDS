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
   (message-text :initform nil
                 :initarg :text
                 :documentation "Displayed Detail text. Can be
                 missing, in that case no label will be added")
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
                      finalized when the dialog closes")
   (title :initform "Dialog"
          :initarg :title)
   (width :initform nil
          :initarg :width)
   (height :initform nil
           :initarg :height)))

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

(define-override (dialog close-event) (ev)
  (declare (ignore ev))
  (finish-dialog dialog :cancel))

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
  (when message-text
    (q+:set-text message message-text)
    (q+:add-widget layout message)))

(defmethod add-text ((dialog dialog) text)
  (with-slots (message-text layout message) dialog
    (unless message-text
      (q+:add-widget layout message))
    (setf message-text
          (format nil "~a~%~a"
                  (or message-text "")
                  text))
    (q+:set-text message message-text)))

(defmethod cancel ((dialog dialog))
  (finish-dialog dialog :cancel))

(define-initializer (dialog setup-widget)
  (q+:set-text ok ok-button-text)
  (q+:set-text cancel cancel-button-text)
  (when widget
    (q+:add-widget layout widget))
  (q+:add-widget layout button-area)
  (when (or width height)
    (q+:resize dialog
               (or width
                   (q+:width dialog))
               (or height
                   (q+:height dialog))))
  (when *main-window*
    (q+:set-window-icon dialog (q+:window-icon *main-window*)))
  (qdoto dialog
         (q+:set-attribute (q+:qt.wa_delete-on-close))
         (q+:set-window-title title)
         (q+:show)))

(define-finalizer (dialog cleanup-widget)
  (when (and widget finalize-widget-p)
    (finalize widget)))
