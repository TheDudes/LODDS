(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget graph (QWidget)
  ((elements :initarg :elements)
   (points :initform nil)
   (position :initform 0)
   (alpha :initform 255
          :initarg :alpha)
   (color :initform (q+:make-qcolor "#000000")
          :initarg :color)
   (brush :initform nil)))

(define-initializer (graph setup-points)
  (setf points (make-array elements
                           :initial-element 0)))

(defmethod add-value ((graph graph) new-value)
  (with-slots-bound (graph graph)
    (setf (aref points position) new-value)
    (setf position (mod (+ 1 position) elements))))

(defmethod get-values ((graph graph))
  (with-slots-bound (graph graph)
    (loop :repeat elements
          :for pos = (mod (+ (or pos (- position 1)) 1) elements)
          :collect (aref points pos))))

(defmethod max-value ((graph graph))
  (loop :for x :across (slot-value graph 'points)
        :maximize x))

(define-override (graph paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((path (q+:make-qpainterpath))
                    (painter (q+:make-qpainter graph)))
    (let* ((height (- (q+:height graph) 1))
           (width (q+:width graph))
           (max-value (let ((max (max-value graph)))
                        (if (or (not max) (eql 0 max)) 1 max)))
           (height-pp (/ height max-value))
           (width-pp (/ width elements))
           (i 0))
      (q+:move-to path 0 height)
      (loop :for value :in (get-values graph)
            :for x = (round (* i width-pp))
            :for y = (round (- height (* value height-pp)))
            :do (progn (q+:line-to path x y)
                       (incf i)))
      (q+:line-to path width height)
      (q+:fill-path painter path brush))))

(define-initializer (graph setup-widget)
  (q+:set-alpha color alpha)
  (setf brush (q+:make-qbrush color)))

(define-finalizer (graph cleanup-color)
  (finalize color))
