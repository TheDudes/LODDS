(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget send-file (QWidget)
  ())

(define-subwidget (send-file file)
    (q+:make-qlineedit send-file)
  (let* ((completer (q+:make-qcompleter file))
         (file-model (q+:make-qdirmodel completer)))
    (q+:set-filter file-model (qt:enum-or (q+:qdir.all-dirs)
                                          (q+:qdir.no-dot-and-dot-dot)
                                          (q+:qdir.files)))
    (q+:set-model completer file-model)
    (q+:set-completer file completer))
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

(define-subwidget (send-file sub-layout)
    (q+:make-qhboxlayout)
  (qdoto sub-layout
         (q+:add-widget file)
         (q+:add-widget button)))

(define-subwidget (send-file timeout)
    (q+:make-qspinbox send-file)
  (qdoto timeout
         (q+:set-suffix " seconds")
         (q+:set-minimum 10)
         (q+:set-maximum 3600)
         (q+:set-value 30)))

(define-subwidget (send-file layout)
    (q+:make-qformlayout send-file)
  (qdoto layout
         (q+:add-row "File:" sub-layout)
         (q+:add-row "Timeout:" timeout)))

(defun open-send-file-dialog (user ip port)
  (make-instance 'dialog
                 :title "Send File"
                 :text (format nil "Select a File and a Timeout to send to ~a" user)
                 :widget (make-instance 'send-file)
                 :on-success-fn
                 (lambda (widget)
                   (with-slots-bound (widget send-file)
                     (let ((file-choosen (q+:text file)))
                       (if (uiop:file-exists-p file-choosen)
                           (progn
                             (lodds:send-file file-choosen
                                              ip
                                              port
                                              (q+:value timeout))
                             t)
                           (progn
                             (make-instance 'dialog
                                            :title "Error - File does not exist"
                                            :text "Selected File does not exist")
                             nil)))))))
