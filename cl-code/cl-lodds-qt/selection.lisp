#|

Selection is a Widget which displays group of radio buttons where the
user can select one solution.

|#

(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget selection (QGroupBox)
  ((title :initarg :title)
   (solutions :initarg :solutions)
   (buttons :initform (list)
            :type list
            :documentation "List out of displayed radio buttons with
            their solution keyword")))

(define-subwidget (selection layout)
    (q+:make-qvboxlayout selection)
  (loop :for (name info) :in solutions
        :do (let ((button (q+:make-qradiobutton info selection)))
              (q+:add-widget layout button)
              (push (list button name) buttons)))
  (destructuring-bind (button solution) (car (last buttons))
    (declare (ignore solution))
    (q+:set-checked button t))
  (q+:add-stretch layout 1))

(define-initializer (selection setup-title)
  (q+:set-title selection title))

(defmethod get-selected-solution ((selection selection))
  (with-slots-bound (selection selection)
    (loop :for (button solution) :in buttons
          :if (q+:is-checked button)
          :do (return-from get-selected-solution solution))))
