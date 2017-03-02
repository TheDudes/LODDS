(in-package #:lodds-qt)
(in-readtable :qtools)

(defgeneric get-value (widget))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass setting ()
    ((key :initarg :key
          :documentation "Settings key to look up value, description
          etc. see lodds.config")
     (widget :documentation "The widget itself, will be set on widget
           init. is used to set tooltip on initialize-instance"))))

(defmethod initialize-instance :after ((setting setting) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (widget key) setting
    (q+:set-tool-tip widget
                     (lodds.config:get-description key))))

;; Selection Widget

(define-widget selection-setting (QWidget setting)
  ())

(define-subwidget (selection-setting selector)
    (q+:make-qcombobox selection-setting)
  (with-slots (key widget) selection-setting
    (setf (q+:size-policy selector) (values (q+:qsizepolicy.expanding)
                                            (q+:qsizepolicy.fixed)))
    (q+:add-items selector (lodds.config:get-selection-options key))
    (setf widget selector)
    (let ((value (lodds.config:get-value key)))
      (if value
          (let ((current-index (q+:find-text selector
                                             value)))
            (when (>= current-index 0)
              (q+:set-current-index selector current-index)))
          (q+:set-current-index selector -1)))))

(define-subwidget (selection-setting refresh)
    (q+:make-qpushbutton "Reload" selection-setting)
  (setf (q+:size-policy refresh) (values (q+:qsizepolicy.minimum)
                                         (q+:qsizepolicy.fixed))))

(define-slot (selection-setting refresh) ()
  (declare (connected refresh (pressed)))
  (q+:clear selector)
  (q+:add-items selector
                (lodds.config:get-selection-options
                 (slot-value selection-setting 'key))))

(define-subwidget (selection-setting layout)
    (q+:make-qhboxlayout selection-setting)
  (qdoto layout
         (q+:add-widget selector)
         (q+:add-widget refresh)))

(defmethod get-value ((selection-setting selection-setting))
  (q+:current-text (slot-value selection-setting 'selector)))

;; Boolean Widget

(define-widget boolean-setting (QCheckBox setting)
  ())

(define-initializer (boolean-setting setup-widget)
  (with-slots (key widget) boolean-setting
    (setf widget boolean-setting)
    (q+:set-checked boolean-setting (lodds.config:get-value key))))

(defmethod get-value ((boolean-setting boolean-setting))
  (q+:is-checked boolean-setting))

;; Integer Widget

(define-widget integer-setting (QSpinBox setting)
  ())

(define-initializer (integer-setting setup-widget)
  (with-slots (key widget) integer-setting
    (q+:set-range integer-setting
                  (lodds.config:get-integer-min key)
                  (lodds.config:get-integer-max key))
    (setf widget integer-setting)
    (q+:set-value integer-setting (lodds.config:get-value key))))

(defmethod get-value ((integer-setting integer-setting))
  (q+:value integer-setting))

;; String Widget

(define-widget string-setting (QLineEdit setting)
  ())

(define-initializer (string-setting setup-widget)
  (with-slots (key widget) string-setting
    (setf widget string-setting)
    (q+:set-text string-setting (lodds.config:get-value key))))

(defmethod get-value ((string-setting string-setting))
  (q+:text string-setting))

;; List Widget

(define-widget list-setting (QLineEdit setting)
  ())

(define-initializer (list-setting setup-widget)
  (with-slots (key widget) list-setting
    (setf widget list-setting)
    (q+:set-text list-setting (format nil "~{~a~^,~}"
                                       (lodds.config:get-value key)))))

(defmethod get-value ((list-setting list-setting))
  (let ((line (q+:text list-setting)))
    (mapcar (lambda (str)
              (string-trim '(#\Space) str))
            (unless (eql 0 (length (string-trim '(#\Space) line)))
              (cl-strings:split line #\,)))))

;; Folder Widget

(define-widget folder-setting (QWidget setting)
  ())

(define-subwidget (folder-setting folder)
    (q+:make-qlineedit folder-setting)
  (with-slots (key) folder-setting
    (let* ((completer (q+:make-qcompleter folder))
           (dir-model (q+:make-qdirmodel completer)))
      (q+:set-filter dir-model (q+:qdir.dirs))
      (q+:set-model completer dir-model)
      (q+:set-completer folder completer)
      (q+:set-minimum-width folder 150))
    (setf (q+:size-policy folder) (values (q+:qsizepolicy.expanding)
                                          (q+:qsizepolicy.fixed)))))

(define-subwidget (folder-setting open)
    (q+:make-qpushbutton "Open" folder-setting)
  (setf (q+:size-policy open) (values (q+:qsizepolicy.minimum)
                                      (q+:qsizepolicy.fixed))))

(define-slot (folder-setting open) ()
  (declare (connected open (pressed)))
  (let ((dir (q+:qfiledialog-get-existing-directory)))
    (when (> (length dir)
             0)
      (q+:set-text folder dir))))

(define-subwidget (folder-setting layout)
    (q+:make-qhboxlayout folder-setting)
  (qdoto layout
         (q+:add-widget folder)
         (q+:add-widget open)))

(define-initializer (folder-setting setup-widget)
  (with-slots (key widget) folder-setting
    (setf widget folder)
    (q+:set-text folder (lodds.config:get-value key))))

(defmethod get-value ((folder-setting folder-setting))
  (q+:text (slot-value folder-setting 'folder)))

;; Functions

(defun make-setting (key)
  (case (lodds.config:get-type key)
    (:boolean   (make-instance 'boolean-setting   :key key))
    (:list      (make-instance 'list-setting      :key key))
    (:string    (make-instance 'string-setting    :key key))
    (:integer   (make-instance 'integer-setting   :key key))
    (:folder    (make-instance 'folder-setting    :key key))
    (:selection (make-instance 'selection-setting :key key))
    (t (error "Type ~a of key ~a not recognised"
              (lodds.config:get-type key)
              key))))

(defun make-setting-dialog ()
  (let* ((scroll (q+:make-qscrollarea))
         (widget (q+:make-qwidget))
         (layout (q+:make-qformlayout widget))
         (settings (list)))
    (loop :for key :in (lodds.config:get-all-keys)
          :do (let ((label (q+:make-qlabel
                            (format nil "~a:"
                                    (string-downcase (string key)))))
                    (setting (make-setting key)))
                (q+:set-tool-tip label
                                 (lodds.config:get-description key))
                (q+:add-row layout
                            label
                            setting)
                (push setting settings)))
    (qdoto scroll
           (q+:set-widget widget)
           (q+:set-widget-resizable t))
    (make-instance 'dialog
                   :title "Settings"
                   :widget scroll
                   :width 500
                   :height 400
                   :on-success-fn
                   (lambda (widget)
                     (declare (ignore widget))
                     (loop :for setting :in settings
                           :do
                           (let* ((key (slot-value setting 'key))
                                  (err (lodds.config:update-entry key
                                                                  (get-value setting))))
                             (when err
                               (make-instance 'dialog
                                              :title "ERROR - Wrong Setting"
                                              :text (format nil "Error on setting key: ~a~%~a"
                                                            key
                                                            err))
                               (return nil)))
                           :finally (return t))))))
