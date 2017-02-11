(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget main-window (QMainWindow) ())

(define-menu (main-window File)
  (:item ("Run" (ctrl r))
         (if (lodds:interface lodds:*server*)
             (progn
               (lodds.subsystem:start (lodds:get-subsystem :event-queue))
               (lodds.subsystem:start (lodds:get-subsystem :tasker))
               (lodds.subsystem:start (lodds:get-subsystem :listener))
               (lodds.subsystem:start (lodds:get-subsystem :advertiser))
               (lodds.subsystem:start (lodds:get-subsystem :handler)))
             (qdoto (q+:make-qmessagebox main-window)
                    (q+:set-window-title "Error - Could not start LODDS")
                    (q+:set-text "Interface not set!")
                    (q+:set-detailed-text
                     (concatenate 'string
                                  "Please select a Interface (there should be a"
                                  " 'Settings' Section somewhere with a empty"
                                  " combobox, just click it and select the"
                                  " prefered interface.)"))
                    (q+:open))))
  (:item ("Stop" (ctrl s))
         (lodds.subsystem:stop (lodds:get-subsystem :tasker))
         (lodds.subsystem:stop (lodds:get-subsystem :listener))
         (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
         (lodds.subsystem:stop (lodds:get-subsystem :handler))
         (lodds.subsystem:stop (lodds:get-subsystem :watcher)))
  (:separator)
  (:item "Reload Stylesheet"
         (q+:set-style-sheet main-window *style-sheet*))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-subwidget (main-window interfaces) (q+:make-qcombobox main-window)
  (q+:add-items interfaces (lodds:get-interfaces))
  (let ((current-interface (lodds:interface lodds:*server*)))
    (if current-interface
        (let ((current-interface-index (q+:find-text interfaces
                                                     current-interface)))
          (when (> current-interface-index 0)
            (q+:set-current-index interfaces current-interface-index)))
        (q+:set-current-index interfaces -1))))

(define-subwidget (main-window info-log-widget) (make-instance 'info-log))
(define-subwidget (main-window user-list-widget) (make-instance 'user-list))
(define-subwidget (main-window shared-widget) (make-instance 'shared))
(define-subwidget (main-window download-widget) (make-instance 'download))
(define-subwidget (main-window shares-widget) (make-instance 'shares)
  (connect shares-widget "itemClicked(QTreeWidgetItem *, int)"
           (lambda (selected-item column)
             (declare (ignore column))
             (setf *selected-file* (get-selected-file selected-item))
             (signal! download-widget (update-download)))))

(define-initializer (main-window setup-widget)
  (qdoto main-window
         (q+:set-window-title "LODDS")
         (q+:set-window-icon (q+:make-qicon "./res/lodds.png"))
         (q+:resize 800 450)
         (q+:set-style-sheet *style-sheet*)))

(define-initializer (main-window setup-docks)
  (let ((settings-dock (q+:make-qdockwidget "Settings" main-window))
        (log-dock (q+:make-qdockwidget "Log" main-window))
        (user-list-dock (q+:make-qdockwidget "User List" main-window))
        (directories-shared-dock (q+:make-qdockwidget "Directories Shared" main-window))
        (download-dock (q+:make-qdockwidget "Download File" main-window)))

    ;; download-file dock
    (q+:set-widget download-dock download-widget)
    (q+:add-dock-widget main-window (q+:qt.left-dock-widget-area) download-dock)

    ;; settings-dock
    (let* ((dock-content (q+:make-qwidget))
           (dock-layout (q+:make-qformlayout dock-content)))
      (q+:set-maximum-height dock-content 48)
      (q+:add-row dock-layout "Interface:" interfaces)
      (q+:set-widget settings-dock dock-content))

    ;; user-dock
    (q+:set-widget user-list-dock user-list-widget)

    ;; directories-shared-dock
    (q+:set-widget directories-shared-dock shared-widget)

    ;; log-dock
    (q+:set-widget log-dock info-log-widget)

    ;; add view-toggle to View MenuBar Item
    (qdoto (q+:add-menu (q+:menu-bar main-window) "View")
           (q+:add-action (q+:toggle-view-action log-dock))
           (q+:add-action (q+:toggle-view-action settings-dock))
           (q+:add-action (q+:toggle-view-action user-list-dock))
           (q+:add-action (q+:toggle-view-action directories-shared-dock))
           (q+:add-action (q+:toggle-view-action download-dock)))

    ;; add widgets to main-window
    (qdoto main-window
           (q+:add-dock-widget (q+:qt.right-dock-widget-area) settings-dock)
           (q+:add-dock-widget (q+:qt.left-dock-widget-area) user-list-dock)
           (q+:add-dock-widget (q+:qt.right-dock-widget-area) directories-shared-dock)
           (q+:add-dock-widget (q+:qt.bottom-dock-widget-area) log-dock)
           (q+:set-central-widget shares-widget))))

(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window fix-menubar-order) ())

(define-slot (main-window interfaces) ((selected-item string))
  (declare (connected interfaces (current-index-changed string)))
  (lodds:switch-interface selected-item))

(define-slot (main-window fix-menubar-order) ()
  (declare (connected main-window (fix-menubar-order)))
  (let* ((menu-bar (q+:menu-bar main-window)))
    (with-finalizing ((menu (q+:make-qmenu)))
      (let ((order (list (cons "File" nil)
                         (cons "View" nil))))
        (loop :for child :in (find-children menu-bar menu)
              :collect (let ((entry (find (q+:title child) order
                                          :test (lambda (a b)
                                                  (string= a (car b))))))
                         (when entry
                           (setf (cdr entry) child))))
        (q+:clear menu-bar)
        (loop :for (childname . child) :in order
              :do (q+:add-menu menu-bar child))))))

(define-slot (main-window reload-stylesheet) ()
  (declare (connected main-window (reload-stylesheet)))
  (q+:set-style-sheet main-window *style-sheet*))

(defun debug-ignore (&rest args)
  (format t "ERROR:---------------------------------------~%")
  (format t "~a~%" args)
  (format t "---------------------------------------------~%")
  (abort))

(defparameter *main-window* nil
  "Contains the Main-window, usefull to debug/inspect gui widgets.")

(defun main (&optional (lodds-server (make-instance 'lodds:lodds-server) server-given-p))
  ;; so iam calling tmt:with-body-in-main-thread here myself and set
  ;; :main-thread to nil on with-main-window. This way lodds-server
  ;; can be dynamically bound to lodds::*server* with
  ;; lodds:with-server and is available on the main thread.
  (tmt:with-body-in-main-thread ()
    (lodds:with-server lodds-server
      (with-main-window (window (make-instance 'main-window)
                         :main-thread nil
                         :on-error #'debug-ignore)
        (setf *main-window* window)
        (signal! window (fix-menubar-order)))
      (unless server-given-p
        (lodds:shutdown)))))
