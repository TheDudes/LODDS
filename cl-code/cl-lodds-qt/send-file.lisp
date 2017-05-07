(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget send-file (QWidget)
  ((users-selected :initarg :users-selected
                   :initform (list)
                   :type list
                   :documentation "All users the user has checked.")
   (checkboxes :initform nil
               :type list
               :documentation "List of all checkboxes inside the
               userlist")))

(define-subwidget (send-file file)
    (q+:make-qlineedit send-file)
  (let* ((completer (q+:make-qcompleter file))
         (file-model (q+:make-qdirmodel completer)))
    (q+:set-filter file-model (qt:enum-or (q+:qdir.all-dirs)
                                          (q+:qdir.no-dot-and-dot-dot)
                                          (q+:qdir.files)))
    (q+:set-model completer file-model)
    (q+:set-completer file completer))
  (q+:set-tool-tip file
                   (format nil
                           "Selected file you want to send~%~
                           to the selected user(s)."))
  (setf (q+:size-policy file)
        (values (q+:qsizepolicy.expanding)
                (q+:qsizepolicy.fixed))))

(define-subwidget (send-file button)
    (q+:make-qpushbutton "Select" send-file)
  (setf (q+:size-policy button)
        (values (q+:qsizepolicy.minimum)
                (q+:qsizepolicy.fixed))))

(define-slot (send-file select-file) ()
  (declare (connected button (pressed)))
  (let ((file-choosen (select-file)))
    (when file-choosen
      (q+:set-text file file-choosen))))

(define-subwidget (send-file file-layout)
    (q+:make-qhboxlayout)
  (qdoto file-layout
         (q+:add-widget file)
         (q+:add-widget button)))

(define-subwidget (send-file timeout)
    (q+:make-qspinbox send-file)
  (qdoto timeout
         (q+:set-tool-tip (format nil
                                  "Timeout in seconds the choosen~%~
                                  user(s) have to accept the file.~%~
                                  If they do not accept withing the~%~
                                  the given timeout, the transfer~%~
                                  will be aborted."))
         (q+:set-suffix " seconds")
         (q+:set-minimum 10)
         (q+:set-maximum 3600)
         (q+:set-value 30)))

(define-subwidget (send-file users)
    (q+:make-qscrollarea send-file)
  (q+:hide users)
  (let* ((widget (q+:make-qwidget send-file))
         (layout (q+:make-qvboxlayout widget)))
    (q+:set-widget users widget)
    (q+:set-widget-resizable users t)
    (loop :for user :in (lodds:get-user-list)
          :do (let ((checkbox (q+:make-qcheckbox user send-file)))
                (push checkbox checkboxes)
                (q+:add-widget layout checkbox)
                (when (find user users-selected :test #'equal)
                  (q+:set-checked checkbox t))
                (connect checkbox "toggled(bool)"
                         (lambda (checked)
                           (let ((user (q+:text checkbox)))
                             (if checked
                                 (push user users-selected)
                                 (setf users-selected
                                       (remove user users-selected
                                               :test #'equal))))))))))

(define-subwidget (send-file sub-layout)
    (q+:make-qwidget send-file)
  (let ((layout (q+:make-qformlayout sub-layout)))
    (qdoto layout
           (q+:add-row "File:" file-layout)
           (q+:add-row "Timeout:" timeout))))

(define-subwidget (send-file select-all)
    (q+:make-qpushbutton "Select All" send-file)
  (q+:hide select-all)
  (connect select-all "pressed()"
           (let ((was-pressed nil))
             (lambda ()
               (if was-pressed
                   (progn
                     (setf was-pressed nil)
                     (q+:set-text select-all "Select All")
                     (dolist (checkbox checkboxes)
                       (q+:set-checked checkbox nil)))
                   (progn
                     (setf was-pressed t)
                     (q+:set-text select-all "Unselect All")
                     (dolist (checkbox checkboxes)
                       (q+:set-checked checkbox t))))))))

(define-subwidget (send-file show-users)
    (q+:make-qpushbutton "User Selection" send-file)
  (connect show-users "toggled(bool)"
           (lambda (checked)
             (if checked
                 (progn
                   (q+:show users)
                   (q+:show select-all))
                 (progn
                   (q+:hide users)
                   (q+:hide select-all)
                   (q+:adjust-size send-file)
                   (when (qobject-alive-p (q+:parent send-file))
                     (q+:adjust-size (q+:parent send-file)))))))
  (qdoto show-users
         (q+:set-tool-tip (format nil
                                  "Click to select multiple users."))
         (q+:set-checkable t)
         (q+:set-minimum-width 400)
         (q+:set-checked nil)))

(define-subwidget (send-file layout)
    (q+:make-qvboxlayout send-file)
  (qdoto layout
         (q+:add-widget sub-layout)
         (q+:add-widget show-users)
         (q+:add-widget users)
         (q+:add-widget select-all)))

(defmethod initialize-instance :after ((send-file send-file) &key selected-file)
  (with-slots-bound (send-file send-file)
    (when selected-file
      (q+:set-text file
                   selected-file))))

(defun open-send-file-dialog (&optional user file)
  (make-instance 'dialog
                 :title "Send file"
                 :text "Select a file and a timeout"
                 :widget (make-instance 'send-file
                                        :users-selected (if user
                                                            (list user)
                                                            nil)
                                        :selected-file file)
                 :ok-text "Send"
                 :on-success-fn
                 (lambda (widget)
                   (with-slots-bound (widget send-file)
                     (let ((file-choosen (q+:text file))
                           (users (slot-value widget 'users-selected)))
                       (if (lodds.core:file-exists file-choosen)
                           (if users
                               (loop :for selected-user :in (slot-value widget 'users-selected)
                                     :do (lodds:send-file (pathname
                                                           (lodds.core:escape-wildcards file-choosen))
                                                          selected-user
                                                          (q+:value timeout))
                                     :finally (return t))
                               (progn
                                 (make-instance 'dialog
                                                :title "Error - No users selected"
                                                :text "Please select at least one user")
                                 nil))
                           (progn
                             (make-instance 'dialog
                                            :title "Error - File does not exist"
                                            :text "Selected file does not exist")
                             nil)))))))
