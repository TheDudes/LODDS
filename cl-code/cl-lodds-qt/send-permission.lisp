(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget send-permission (QWidget)
  ((default-folder :initarg :default-folder
                   :initform (lodds.config:get-value :upload-folder)
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
               :type function)
   (task :initarg :task
         :documentation "Send Permission task, used to check on tick
         if task was canceled, if so call on-timeout.")))

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
    (q+:make-qprogressbar send-permission)
  (qdoto time-left
         (q+:set-maximum timeout)
         (q+:set-format "%v seconds left to accept")))

(define-subwidget (send-permission layout)
    (q+:make-qgridlayout send-permission)
  (qdoto layout
         (q+:add-widget time-left 0 0 1 5)

         (q+:add-widget (q+:make-qlabel "Save file to:" send-permission) 1 0 1 4)

         (q+:add-widget folder 2 0 1 3)
         (q+:add-widget filename 2 3 1 2)))

(define-slot (send-permission tick) ()
  (declare (connected timer (timeout)))
  (incf time-vanished)
  (if (or (>= time-vanished timeout)
          (slot-value task 'lodds.task::canceled-p))
      (when on-timeout
        (funcall on-timeout))
      (q+:set-value time-left (- timeout time-vanished))))

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
  (q+:start timer 1000)
  (q+:set-value time-left timeout))

(defun open-send-permission-dialog (task)
  (with-slots ((size lodds.task::size)
               (timeout lodds.task::timeout)
               (filename lodds.task::filename)
               (socket lodds.task::socket)) task
    (let* ((user (lodds:get-user-by-ip
                  (usocket:get-peer-address socket)))
           (widget (make-instance 'send-permission
                                  :timeout timeout
                                  :default-filename filename
                                  :task task)))
      (let ((dialog (make-instance
                     'dialog
                     :title
                     (format nil "User ~{~a~^or~} wants to send you a File (Size: ~a)"
                             user
                             (lodds.core:format-size size))
                     :text
                     (concatenate 'string
                                  "If you want to accept the File, "
                                  "select a folder and a filename and click OK")
                     :widget widget
                     :ok-text "Accept"
                     :cancel-text "Deny"
                     :on-success-fn
                     (lambda (widget)
                       (let ((full-filename (get-full-filename widget)))
                         (if full-filename
                             (progn
                               (setf filename full-filename)
                               (lodds.task:submit-task task)
                               t)
                             (progn
                               (make-instance 'dialog
                                              :title "Error - Invalid Input"
                                              :text "The given input was invalid")
                               nil))))
                     :on-cancel-fn
                     (lambda (widget)
                       (declare (ignore widget))
                       (lodds.task:cancel-task task)
                       (lodds.task:submit-task task)))))
        (setf (slot-value widget 'on-timeout)
              (lambda ()
                (cancel dialog)))))))
