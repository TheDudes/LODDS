(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget send-file (QWidget)
  ((users-selected :initarg :users-selected
                   :initform (list)
                   :type list
                   :documentation "All users the user has checked.")))

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
                           "Selected File you want to send~%~
                           a User."))
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
  (let ((file-choosen (q+:qfiledialog-get-open-file-name)))
    (when (> (length file-choosen)
             0)
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
                                  "Timeout in Seconds the choosen~%~
                                  User(s) have to accept the File.~%~
                                  If they do not accept withing the~%~
                                  the given timeout, the transfer~%~
                                  will be aborted."))
         (q+:set-suffix " seconds")
         (q+:set-minimum 10)
         (q+:set-maximum 3600)
         (q+:set-value 30)))

(define-subwidget (send-file users)
    (q+:make-qtreewidget send-file)
  (qdoto users
         (q+:set-column-count 2)
         (q+:set-header-labels (list "Name" "Send File"))
         (q+:set-alternating-row-colors t)
         (q+:hide))
  (qdoto (q+:header users)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode 0 (q+:qheaderview.stretch))
         (q+:set-resize-mode 1 (q+:qheaderview.resize-to-contents)))
  (loop :for user :in (lodds:get-user-list)
        :do (let ((new-entry (q+:make-qtreewidgetitem users))
                  (checkbox (q+:make-qcheckbox send-file)))
              (q+:set-item-widget users
                                  new-entry
                                  1
                                  checkbox)
              (when (find user users-selected :test #'equal)
                (q+:set-checked checkbox t))
              (q+:set-text new-entry 0 user)
              (connect checkbox "toggled(bool)"
                       (lambda (checked)
                         (let ((user (q+:text new-entry 0)))
                           (if checked
                               (push user users-selected)
                               (setf users-selected
                                     (remove user users-selected
                                             :test #'equal)))))))))

(define-subwidget (send-file sub-layout)
    (q+:make-qwidget send-file)
  (let ((layout (q+:make-qformlayout sub-layout)))
    (qdoto layout
           (q+:add-row "File:" file-layout)
           (q+:add-row "Timeout:" timeout))))

(define-subwidget (send-file show-users)
    (q+:make-qpushbutton "User Selection" send-file)
  (connect show-users "toggled(bool)"
           (lambda (checked)
             (if checked
                 (q+:show users)
                 (q+:hide users))))
  (qdoto show-users
         (q+:set-tool-tip (format nil
                                  "Click to select multiple Users."))
         (q+:set-checkable t)
         (q+:set-minimum-width 400)
         (q+:set-checked nil)))

(define-subwidget (send-file layout)
    (q+:make-qvboxlayout send-file)
  (qdoto layout
         (q+:add-widget sub-layout)
         (q+:add-widget show-users)
         (q+:add-widget users)))

(defmethod initialize-instance :after ((send-file send-file) &key selected-file)
  (when selected-file
    (with-slots-bound (send-file send-file)
      (q+:set-text file
                   selected-file))))

(defun open-send-file-dialog (&optional user file)
  (make-instance 'dialog
                 :title "Send File"
                 :text "Select a File and a Timeout"
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
                       (if (cl-fs-watcher:escaped-file-exists-p file-choosen)
                           (if users
                               (loop :for selected-user :in (slot-value widget 'users-selected)
                                     :do (lodds:send-file-user file-choosen
                                                               selected-user
                                                               (q+:value timeout))
                                     :finally (return t))
                               (progn
                                 (make-instance 'dialog
                                                :title "Error - No Users selected"
                                                :text "Please Select at least one User")
                                 nil))
                           (progn
                             (make-instance 'dialog
                                            :title "Error - File does not exist"
                                            :text "Selected File does not exist")
                             nil)))))))
