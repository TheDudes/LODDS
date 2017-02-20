(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget directory-error (QGroupBox)
  ((title :initarg :title)
   (solutions :initarg :solutions)
   (buttons :initform (list)
            :type list
            :documentation "List out of displayed radio buttons with
            their solution keyword")))

(define-subwidget (directory-error layout)
    (q+:make-qvboxlayout directory-error)
  (loop :for (name info) :in solutions
        :do (let ((button (q+:make-qradiobutton info directory-error)))
              (q+:add-widget layout button)
              (push (list button name) buttons)))
  (destructuring-bind (button solution) (car (last buttons))
    (declare (ignore solution))
    (q+:set-checked button t))
  (q+:add-stretch layout 1))

(define-initializer (directory-error setup-title)
  (q+:set-title directory-error title))

(defmethod get-selected-solution ((directory-error directory-error))
  (with-slots-bound (directory-error directory-error)
    (loop :for (button solution) :in buttons
          :if (q+:is-checked button)
          :do (return-from get-selected-solution solution))))
