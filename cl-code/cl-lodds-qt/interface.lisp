(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget interface (QWidget)
  ((blocked :initform nil
            :documentation "used to block switching when user presses
            refresh (since combobox will autoselect a item when items
            are added to it)")))

(define-subwidget (interface label) (q+:make-qlabel "Interface:" interface)
  (setf (q+:size-policy label) (values (q+:qsizepolicy.minimum)
                                       (q+:qsizepolicy.fixed))))

(defun set-current-interface (selector)
  (let ((current-interface (lodds:interface lodds:*server*)))
    (if current-interface
        (let ((current-interface-index (q+:find-text selector
                                                     current-interface)))
          (when (>= current-interface-index 0)
            (q+:set-current-index selector current-interface-index)))
        (q+:set-current-index selector -1))))

(define-subwidget (interface selector) (q+:make-qcombobox interface)
  (q+:add-items selector (lodds.core:get-interfaces))
  (setf (q+:size-policy selector) (values (q+:qsizepolicy.expanding)
                                          (q+:qsizepolicy.fixed)))
  (set-current-interface selector))

(define-subwidget (interface refresh) (q+:make-qpushbutton "Reload" interface)
  (setf (q+:size-policy refresh) (values (q+:qsizepolicy.minimum)
                                         (q+:qsizepolicy.fixed))))

(define-subwidget (interface layout) (q+:make-qhboxlayout interface)
  (qdoto layout
         (q+:add-widget label)
         (q+:add-widget selector)
         (q+:add-widget refresh)))

(define-slot (interface interface-selected) ((selected-interface string))
  (declare (connected selector (current-index-changed string)))
  (unless (or (eql 0 (length selected-interface))
              blocked)
    (lodds:switch-interface selected-interface)))

(defmethod refresh-list :around ((interface interface))
  (with-slots-bound (interface interface)
    (setf blocked t)
    (call-next-method)
    (set-current-interface selector)
    (setf blocked nil)))

(defmethod refresh-list ((interface interface))
  (with-slots-bound (interface interface)
    (q+:clear selector)
    (q+:add-items selector (lodds.core:get-interfaces))
    (q+:set-current-index selector -1)))

(define-slot (interface refresh) ()
  (declare (connected refresh (pressed)))
  (refresh-list interface))
