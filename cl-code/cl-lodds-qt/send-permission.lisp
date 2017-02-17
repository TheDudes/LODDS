(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget send-permission (QWidget)
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

(define-subwidget (send-permission folder)
    (q+:make-qlineedit send-permission )
  (let* ((completer (q+:make-qcompleter send-permission ))
         (dir-model (q+:make-qdirmodel completer)))
    (q+:set-filter dir-model (q+:qdir.dirs))
    (q+:set-model completer dir-model)
    (q+:set-completer folder completer)))

(define-subwidget (send-permission filename)
    (q+:make-qlineedit send-permission))

(define-subwidget (send-permission timer)
    (q+:make-qtimer send-permission))

(define-subwidget (send-permission time-left)
    (q+:make-qlabel send-permission)
  (q+:set-text time-left (prin1-to-string (- timeout 1))))

(define-subwidget (send-permission layout)
    (q+:make-qgridlayout send-permission)
  (qdoto layout
         (q+:add-widget (q+:make-qlabel "Time left to accept:" send-permission) 0 0 1 2)
         (q+:add-widget time-left 0 2 1 2)

         (q+:add-widget (q+:make-qlabel "Save file to:" send-permission) 1 0 1 4)

         (q+:add-widget folder 2 0 1 3)
         (q+:add-widget filename 2 3 1 2)))

(define-slot (send-permission tick) ()
  (declare (connected timer (timeout)))
  (incf time-vanished)
  (if (>= time-vanished timeout)
      (if on-timeout
          (funcall on-timeout)
          (q+:set-text time-left "done"))
      (q+:set-text time-left (prin1-to-string (- timeout time-vanished)))))

(defmethod get-full-filename ((send-permission send-permission))
  (with-slots-bound (send-permission send-permission)
    (let ((directory (q+:text folder))
          (filename-choosen (q+:text filename)))
      (if (and (> (length directory) 0)
               (uiop:directory-exists-p directory)
               (> (length filename-choosen) 0))
          (concatenate 'string
                       (if (char= #\/ (char directory (- (length directory) 1)))
                           directory
                           (concatenate 'string directory "/"))
                       filename-choosen)
          nil))))

(define-initializer (send-permission setup-widget)
  (q+:set-text folder default-folder)
  (q+:set-text filename default-filename)
  (q+:start timer 1000))

(defun open-send-permission-dialog (task)
  (with-slots ((size lodds.task::size)
               (timeout lodds.task::timeout)
               (filename lodds.task::filename)
               (socket lodds.task::socket)) task
    (let* ((user (lodds:get-user-by-ip
                  (usocket:get-peer-address socket)))
           (widget (make-instance 'send-permission
                                  :timeout timeout
                                  :default-filename filename)))
      (let ((dialog (make-instance
                     'dialog
                     :title (format nil "User ~{~a~^or~} asked for send permission"
                                    user)
                     :text (concatenate 'string
                                        "If you want to accept the Send Permission, "
                                        "select a folder and a filename and click OK")
                     :widget widget
                     :on-success-fn
                     (lambda (widget)
                       (let ((full-filename (get-full-filename widget)))
                         (if filename
                             (progn
                               (setf filename full-filename)
                               (lodds.task:submit-task task))
                             (progn
                               (make-instance 'dialog
                                              :title "Error - Invalid Input"
                                              :text "The given input was invalid")
                               (lodds.task:finish-task task)))))
                     :on-cancel-fn
                     (lambda (widget)
                       (declare (ignore widget))
                       (lodds.task:finish-task task)))))
        (setf (slot-value widget 'on-timeout)
              (lambda ()
                (cancel dialog)))))))
