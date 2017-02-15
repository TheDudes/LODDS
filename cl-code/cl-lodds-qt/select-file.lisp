(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget selected (QWidget)
  ((default-folder :initarg :default-folder
     ;; TODO: get default folder from settings
                   :initform "/tmp/"
                   :type string)
   (default-filename :initarg :default-filename
                     :type string)
   (timeout :initarg :timeout
            :initform (error "Specify timeout")
            :type integer)
   (time-vanished :initform 1
                  :type integer)
   (on-timeout :initarg :on-timeout
               :initform nil
               :type function)))

(define-subwidget (selected selected-folder) (q+:make-qlineedit selected)
  (let* ((completer (q+:make-qcompleter selected))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer selected-folder completer)))

(define-subwidget (selected selected-filename) (q+:make-qlineedit selected))

(define-subwidget (selected selected-timer) (q+:make-qtimer selected))

(define-subwidget (selected selected-time-left) (q+:make-qlabel selected)
  (q+:set-text selected-time-left (prin1-to-string (- timeout 1))))

(define-subwidget (selected layout) (q+:make-qgridlayout selected)
  (qdoto layout
         (q+:add-widget (q+:make-qlabel "Time left to accept:" selected) 0 0 1 2)
         (q+:add-widget selected-time-left 0 2 1 2)

         (q+:add-widget (q+:make-qlabel "Save file to:" selected) 1 0 1 4)

         (q+:add-widget selected-folder 2 0 1 3)
         (q+:add-widget selected-filename 2 3 1 2)))

(define-slot (selected tick) ()
  (declare (connected selected-timer (timeout)))
  (incf time-vanished)
  (if (>= time-vanished timeout)
      (if on-timeout
          (funcall on-timeout)
          (q+:set-text selected-time-left "done"))
      (q+:set-text selected-time-left (prin1-to-string (- timeout time-vanished)))))

(defmethod get-full-filename ((selected selected))
  (with-slots-bound (selected selected)
    (let ((directory (q+:text selected-folder))
          (filename (q+:text selected-filename)))
      (if (and (> (length directory) 0)
               (uiop:directory-exists-p directory)
               (> (length filename) 0))
          (concatenate 'string
                       (if (char= #\/ (char directory (- (length directory) 1)))
                           directory
                           (concatenate 'string directory "/"))
                       filename)
          nil))))

(define-initializer (selected setup-widget)
  (q+:set-text selected-folder default-folder)
  (q+:set-text selected-filename default-filename)
  (q+:start selected-timer 1000))
