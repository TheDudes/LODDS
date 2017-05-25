(in-package #:lodds-qt)
(in-readtable :qtools)

(defgeneric get-value (widget))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass setting ()
    ((config :initarg :config
             :documentation "The Current configuration")
     (key :initarg :key
          :documentation "Settings key to look up value, description
          etc. see lodds.config")
     (widget :documentation "The widget itself, will be set on widget
           init. is used to set tooltip on initialize-instance"))))

(defmethod initialize-instance :after ((setting setting) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (widget key config) setting
    (q+:set-tool-tip widget
                     (lodds.config:get-description key config))))

;; Selection Widget

(define-widget selection-setting (QWidget setting)
  ())

(define-subwidget (selection-setting selector)
    (q+:make-qcombobox selection-setting)
  (with-slots (key widget config) selection-setting
    (setf (q+:size-policy selector) (values (q+:qsizepolicy.expanding)
                                            (q+:qsizepolicy.fixed)))
    (q+:set-focus-policy selector (q+:qt.strong-focus))
    (q+:add-items selector (lodds.config:get-selection-options key config))
    (setf widget selector)
    (let ((value (lodds.config:get-value key config)))
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
  (with-slots (config key) selection-setting
    (q+:clear selector)
    (q+:add-items selector
                  (lodds.config:get-selection-options
                   key
                   config))))

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
  (with-slots (key widget config) boolean-setting
    (setf widget boolean-setting)
    (q+:set-checked boolean-setting (lodds.config:get-value key config))))

(defmethod get-value ((boolean-setting boolean-setting))
  (q+:is-checked boolean-setting))

;; Integer Widget

(define-widget integer-setting (QSpinBox setting)
  ())

(define-override (integer-setting wheel-event) (ev)
  (unless (q+:has-focus integer-setting)
    (q+:ignore ev)))

(define-initializer (integer-setting setup-widget)
  (with-slots (key widget config) integer-setting
    (qdoto integer-setting
           (q+:set-focus-policy (q+:qt.strong-focus))
           (q+:set-range (lodds.config:get-integer-min key config)
                         (lodds.config:get-integer-max key config))
           (q+:set-value  (lodds.config:get-value key config)))
    (setf widget integer-setting)))

(defmethod get-value ((integer-setting integer-setting))
  (q+:value integer-setting))

;; String Widget

(define-widget string-setting (QLineEdit setting)
  ())

(define-initializer (string-setting setup-widget)
  (with-slots (key widget config) string-setting
    (setf widget string-setting)
    (q+:set-text string-setting
                 (lodds.config:get-value key config))))

(defmethod get-value ((string-setting string-setting))
  (q+:text string-setting))

;; List Widget

(define-widget list-setting (QLineEdit setting)
  ())

(define-initializer (list-setting setup-widget)
  (with-slots (key widget config) list-setting
    (setf widget list-setting)
    (q+:set-text list-setting (format nil "~{~a~^,~}"
                                      (lodds.config:get-value key config)))))

(define-override (list-setting key-press-event) (ev)
  (with-slots (key config) list-setting
    (if (enum-equal (q+:key ev) (q+:qt.key_tab))
        (let* ((text (q+:text list-setting))
               (already-inserted (mapcar (lambda (str)
                                           (string-trim '(#\Space) str))
                                         (cl-strings:split text #\,)))
               (name-substring (car (last already-inserted)))
               (user (find name-substring
                           (lodds.config:get-suggestions
                            key
                            config)
                           :test (lambda (swap me)
                                   (cl-strings:starts-with me swap)))))
          (when user
            (setf (car (last already-inserted))
                  user)
            (q+:set-text list-setting
                         (format nil "~{~a~^,~}"
                                 already-inserted))))
        (stop-overriding))))

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
  (let ((dir (select-directory)))
    (when dir
      (q+:set-text folder dir))))

(define-subwidget (folder-setting layout)
    (q+:make-qhboxlayout folder-setting)
  (qdoto layout
         (q+:add-widget folder)
         (q+:add-widget open)))

(define-initializer (folder-setting setup-widget)
  (with-slots (key widget config) folder-setting
    (setf widget folder)
    (q+:set-text folder
                 (uiop:native-namestring
                  (lodds.config:get-value key config)))))

(defmethod get-value ((folder-setting folder-setting))
  (lodds.core:ensure-directory-pathname
   (q+:text (slot-value folder-setting 'folder))))

;; Color widget

(define-widget color-setting (QWidget setting)
  ())

(define-subwidget (color-setting color)
    (q+:make-qlineedit color-setting)
  (q+:set-alignment color (q+:qt.align-center))
  (setf (q+:size-policy color)
        (values (q+:qsizepolicy.expanding)
                (q+:qsizepolicy.fixed))))

(define-subwidget (color-setting open)
    (q+:make-qpushbutton "..." color-setting)
  (setf (q+:size-policy open)
        (values (q+:qsizepolicy.minimum)
                (q+:qsizepolicy.fixed))))

(define-slot (color-setting open) ()
  (declare (connected open (pressed)))
  (let ((new-color (select-color)))
    (when new-color
      (q+:set-text color new-color))))

(define-slot (color-setting text-changed) ((text string))
  (declare (connected color (text-changed string)))
  (let ((pos (q+:cursor-position color)))
    (q+:set-text color text)
    (q+:set-cursor-position color pos))
  (with-finalizing ((qcolor (q+:make-qcolor
                             (if (cl-ppcre:scan lodds.config:*color-scanner*
                                                text)
                                 text
                                 "#ffffff"))))
    (let ((palette (q+:palette color)))
      (q+:set-color palette
                    (q+:qpalette.base)
                    qcolor)
      (q+:set-palette color palette))))

(define-subwidget (color-setting layout)
    (q+:make-qhboxlayout color-setting)
  (qdoto layout
         (q+:add-widget color)
         (q+:add-widget open)))

(define-initializer (color-setting setup-widget)
  (with-slots (key widget config) color-setting
    (setf widget color)
    (q+:set-text color
                 (lodds.config:get-value key config))))

(defmethod get-value ((color-setting color-setting))
  (q+:text (slot-value color-setting 'color)))

;; Font widget

(define-widget font-setting (QWidget setting)
  ())

(define-subwidget (font-setting font)
    (q+:make-qlineedit font-setting)
  (q+:set-alignment font (q+:qt.align-center))
  (setf (q+:size-policy font)
        (values (q+:qsizepolicy.expanding)
                (q+:qsizepolicy.fixed))))

(define-subwidget (font-setting open)
    (q+:make-qpushbutton "..." font-setting)
  (setf (q+:size-policy open)
        (values (q+:qsizepolicy.minimum)
                (q+:qsizepolicy.fixed))))

(define-slot (font-setting open) ()
  (declare (connected open (pressed)))
  (let ((new-font (select-font)))
    (when new-font
      (q+:set-text font (concatenate 'string "_"
                                     (q+:to-string new-font)))
      (finalize new-font))))

(define-subwidget (font-setting layout)
    (q+:make-qhboxlayout font-setting)
  (qdoto layout
         (q+:add-widget font)
         (q+:add-widget open)))

(define-initializer (font-setting setup-widget)
  (with-slots (key widget config) font-setting
    (setf widget font)
    (q+:set-text font
                 (lodds.config:get-value key config))))

(defmethod get-value ((font-setting font-setting))
  (q+:text (slot-value font-setting 'font)))

;; Functions

(defun make-setting (key config)
  (make-instance (case (lodds.config:get-type key config)
                   (:boolean   'boolean-setting)
                   (:list      'list-setting)
                   (:string    'string-setting)
                   (:integer   'integer-setting)
                   (:folder    'folder-setting)
                   (:selection 'selection-setting)
                   (:color     'color-setting)
                   (:font      'font-setting)
                   (t (error "Type ~a of key ~a not recognised"
                             (lodds.config:get-type key config)
                             key)))
                 :key key
                 :config config))

(define-widget settings-widget (QScrollArea)
  ((config :initform nil
           :initarg :config)
   (update-inplace-p :initform t
                     :initarg :update-inplace-p)
   (settings :initform (list))))

(define-subwidget (settings-widget scrollarea)
    (q+:make-qscrollarea settings-widget))

(define-subwidget (settings-widget save)
    (q+:make-qpushbutton "Save to File" settings-widget)
  (q+:set-tool-tip save
                   (format nil
                           "Lodds will load the following files on startup:~%~
                           ~{~a~%~}~
                           These files will be loaded from top to bottom. The~%~
                           ones loaded later override settings from the~%~
                           earlier ones."
                           (lodds.config:load-path))))

(define-subwidget (settings-widget load-file)
    (q+:make-qpushbutton "Load from File" settings-widget))

(define-subwidget (settings-widget load-default)
    (q+:make-qpushbutton "Load Default" settings-widget))

(define-subwidget (settings-widget layout)
    (q+:make-qvboxlayout settings-widget)
  (let* ((button-widget (q+:make-qwidget settings-widget))
         (button-layout (q+:make-qhboxlayout button-widget)))
    (qdoto button-layout
           (q+:add-widget save)
           (q+:add-widget load-file)
           (q+:add-widget load-default))
    (qdoto layout
           (q+:add-widget button-widget)
           (q+:add-widget scrollarea))))

(defmethod generate-config-widget ((settings-widget settings-widget))
  (with-slots-bound (settings-widget settings-widget)
    (let* ((widget (q+:make-qwidget))
           (layout (q+:make-qformlayout widget)))
      (dolist (key (lodds.config:get-all-keys config) widget)
        (let ((label (q+:make-qlabel
                      (format nil "~a:"
                              (cl-strings:title-case
                               (string-downcase (string key))))))
              (setting (make-setting key config)))
          (q+:set-tool-tip label
                           (lodds.config:get-description key config))
          (q+:add-row layout
                      label
                      setting)
          (push setting settings))))))

(define-initializer (settings-widget setup-widget)
  (q+:set-widget-resizable scrollarea t)
  (q+:set-widget scrollarea
                 (generate-config-widget settings-widget)))

(define-slot (settings-widget save-pressed) ()
  (declare (connected save (pressed)))
  (let ((update-inplace-backup update-inplace-p))
    (setf update-inplace-p nil)
    (let ((setting (update-setting settings-widget)))
      (setf update-inplace-p update-inplace-backup)
      (when setting
        (let ((file (select-save-file t)))
          (when file
            (lodds.config:save-to-file file
                                       setting)))))))

(defmethod generate-new-settings ((settings-widget settings-widget))
  (with-slots-bound (settings-widget settings-widget)
    (setf update-inplace-p nil)
    (finalize (q+:take-widget scrollarea))
    (setf settings (list))
    (q+:set-widget scrollarea
                   (generate-config-widget settings-widget))))

(define-slot (settings-widget load-default-pressed) ()
  (declare (connected load-default (pressed)))
  (setf config (lodds.config:generate-default-config))
  (generate-new-settings settings-widget))

(define-slot (settings-widget load-file-pressed) ()
  (declare (connected load-file (pressed)))
  (let ((file (select-file t)))
    (when file
      (multiple-value-bind (new-config error)
          (lodds.config:load-from-file file config)
        (if error
            (show-dialog settings-widget :critical
                         (format nil "Error reading config file ~a"
                                 (uiop:native-namestring file))
                         error)
            (progn
              (setf config new-config)
              (generate-new-settings settings-widget)))))))

(defmethod update-setting ((settings-widget settings-widget))
  (with-slots (settings config update-inplace-p) settings-widget
    (let ((new-config (if update-inplace-p
                          config
                          (lodds.config:generate-default-config))))
      (loop :for setting :in settings
            :do
            (let* ((key (slot-value setting 'key))
                   (err (lodds.config:update-entry key
                                                   (get-value setting)
                                                   update-inplace-p
                                                   new-config)))
              (when err
                (make-instance 'dialog
                               :title "ERROR - Wrong setting"
                               :text (format nil "Error on setting key: ~a~%~a"
                                             key
                                             err))
                (return nil)))
            :finally (return new-config)))))

(defmethod validate-config ((settings-widget settings-widget))
  (with-slots (config update-inplace-p) settings-widget
    (let ((successfull (update-setting settings-widget)))
      (when (and successfull
                 (not update-inplace-p))
        (lodds:update-config config))
      successfull)))

(defun make-setting-dialog ()
  (make-instance 'dialog
                 :title "Settings"
                 :widget (make-instance 'settings-widget
                                        :config
                                        (slot-value lodds:*server*
                                                    'lodds:settings))
                 :width 600
                 :height 500
                 :on-success-fn
                 (lambda (settings)
                   (validate-config settings))))
