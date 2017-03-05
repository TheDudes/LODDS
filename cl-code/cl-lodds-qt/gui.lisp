(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ((directory-error-dialog :initform nil
                           :documentation "If a directory-error-dialog
                            is displayed this slot is set to it. This
                            way messages can be added to the dialog,
                            instead of opening multiple dialogs")))

(defun start-lodds ()
  (lodds.subsystem:start (lodds:get-subsystem :event-queue))
  (lodds.subsystem:start (lodds:get-subsystem :tasker))
  (lodds.subsystem:start (lodds:get-subsystem :listener))
  (lodds.subsystem:start (lodds:get-subsystem :handler))
  (lodds.subsystem:start (lodds:get-subsystem :advertiser)))

(defun stop-lodds ()
  (lodds.subsystem:stop (lodds:get-subsystem :tasker))
  (lodds.subsystem:stop (lodds:get-subsystem :listener))
  (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
  (lodds.subsystem:stop (lodds:get-subsystem :handler))
  (lodds.subsystem:stop (lodds:get-subsystem :watcher)))

(defun run ()
  (if (lodds.config:get-value :interface)
      (start-lodds)
      (make-instance
       'dialog
       :title "Error - Interface not set!"
       :text "Please select a Interface first."
       :widget (make-setting :interface
                             (slot-value lodds:*server*
                                         'lodds:settings))
       :on-success-fn
       (lambda (widget)
         (let ((selection (get-value widget)))
           (if selection
               (let ((err
                       (lodds.config:update-entry :interface
                                                  selection)))
                 (if err
                     (progn
                       (make-instance 'dialog
                                      :title "Error - Could not set interface"
                                      :text (format nil "Could not set interface: ~a"
                                                    err))
                       nil)
                     (when (lodds.config:get-value :interface)
                       (start-lodds)
                       t)))
               t))))))

;; Copied system-about and About Menu Entry from Lionchat.
;; modified it to my needs, but all the credit goes to Shinmera
;; see: https://github.com/shirakumo/lionchat
(defun system-about ()
  (let ((system (asdf:find-system :cl-lodds-qt)))
    (format nil "~a<br />~
                License: ~a<br />~
                <br />~
                Homepage: <a href=\"~a~:*\">~a</a><br />~
                Author: ~a<br />~
                Version: ~a<br />
                <br />
                LODDS (Local Open Distributed Data Sharing) is a
                protocol for Filesharing and the base for this
                Program. This Program is a Qt Gui Client which
                implements the LODDS Protocol and can be used to
                share and exchange files with others inside a local
                network."
            (asdf:system-description system)
            (asdf:system-license system)
            (asdf:system-homepage system)
            (asdf:system-author system)
            (asdf:component-version system))))

(define-menu (main-window Help)
  (:item "&About"
         (with-finalizing ((about (q+:make-qmessagebox)))
           (qdoto about
                  (q+:set-window-title "About Lodds")
                  (q+:set-icon-pixmap (q+:pixmap (q+:window-icon main-window) 64 64))
                  (q+:set-text (system-about)))
           (q+:exec about))))

(define-menu (main-window Lodds)
  (:item ("&Run" (ctrl r))
         (run))
  (:item ("&Stop" (ctrl s))
         (stop-lodds))
  (:separator)
  (:item "&Reload Stylesheet"
         (q+:set-style-sheet main-window *style-sheet*))
  (:separator)
  (:item ("&Settings" (ctrl c))
         (make-setting-dialog))
  (:separator)
  (:item ("&Quit" (ctrl q))
         (progn
           (q+:close main-window)
           (q+:qcoreapplication-quit))))

(define-subwidget (main-window tray-icon)
    (q+:make-qsystemtrayicon main-window)
  (q+:set-tool-tip tray-icon
                   (format nil "LODDS - ~a"
                           (lodds.config:get-value :name))))

(define-slot (main-window tray-activated) ((reason "QSystemTrayIcon::ActivationReason"))
  (declare (connected tray-icon (activated "QSystemTrayIcon::ActivationReason")))
  (when (or (qt:enum-equal reason (q+:qsystemtrayicon.trigger))
            (qt:enum-equal reason (q+:qsystemtrayicon.double-click)))
    (if (q+:is-visible main-window)
        (q+:hide main-window)
        (q+:show main-window))))

(define-override (main-window close-event) (ev)
  (q+:hide main-window)
  (q+:ignore ev))

(define-subwidget (main-window view-menu) (q+:add-menu (q+:menu-bar main-window)
                                                       "View"))

(define-subwidget (main-window shares-widget) (make-instance 'shares))

(define-subwidget (main-window log-dock)
    (make-instance 'dock :title "Log"
                         :widget (make-instance 'info-log)
                         :main-window main-window
                         :side :bottom
                         :menu view-menu))

(define-subwidget (main-window user-dock)
    (make-instance 'dock :title "User List"
                         :widget (make-instance 'user-list)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-subwidget (main-window info-dock)
    (make-instance 'dock :title "Info"
                         :widget (make-instance 'info)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-subwidget (main-window shared-dock)
    (make-instance 'dock :title "Directories Shared"
                         :widget (make-instance 'shared)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-initializer (main-window setup-widget)
  (qdoto main-window
         (q+:set-window-title (format nil "LODDS - ~a" (lodds.config:get-value :name)))
         (q+:set-window-icon (q+:make-qicon "./res/lodds.png"))
         (q+:resize 800 450)
         (q+:set-style-sheet *style-sheet*)
         (q+:set-central-widget shares-widget)))

(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window config-changed) ())
(define-signal (main-window received-send-permission) (string))
(define-signal (main-window folder-download-error) (string))
(define-signal (main-window directory-error) (string))

(define-slot (main-window config-changed) ()
  (declare (connected main-window (config-changed)))
  (let ((title (format nil "LODDS - ~a"
                       (lodds.config:get-value :name))))
    (q+:set-window-title main-window title)
    (q+:set-tool-tip tray-icon title))
  (set-refresh-timeout (slot-value info-dock 'widget)
                       (lodds.config:get-value :info-update-interval))
  (set-directory-busy-check-timeout (slot-value shared-dock 'widget)
                                    (lodds.config:get-value :directory-busy-check))
  (setf (slot-value (slot-value log-dock 'widget)
                    'log-message-max)
        (lodds.config:get-value :log-message-max)))

(define-slot (main-window received-send-permission) ((task-id string))
  (declare (connected main-window (received-send-permission string)))
  (let ((task (lodds.task:remove-task-from-hold task-id)))
    (when task
      (open-send-permission-dialog task))))

(define-slot (main-window folder-download-error) ((task-id string))
  (declare (connected main-window (folder-download-error string)))
  (let ((task (lodds.task:get-task-by-id task-id)))
    (with-slots ((items lodds.task::items)
                 (items-done lodds.task::items-done)
                 (remote-path lodds.task::remote-path)
                 (canceled-p lodds.task::canceled-p)) task
      (destructuring-bind (file checksum size) (car items-done)
        (declare (ignore checksum))
        (let ((options (list (list :skip
                                   "skip current file")
                             (list :abort
                                   "abort directory download")
                             (list :retry
                                   "retry loading file"))))
          (flet ((on-close (widget)
                   (case (get-selected-solution widget)
                     (:skip (lodds.task:submit-task task))
                     (:abort (progn
                               (setf canceled-p t)
                               (lodds.task:submit-task task)))
                     (:retry (progn
                               (setf items
                                     (append (list (pop items-done))
                                             items))
                               (lodds.task:submit-task task))))
                   t))
            (make-instance 'dialog
                           :title "Error - File from Directory Download failed"
                           :text (format nil "File ~a (~a) which is part of directory download (~a) failed"
                                         file
                                         (lodds.core:format-size size)
                                         remote-path)
                           :widget (make-instance 'selection
                                                  :title "Solutions:"
                                                  :solutions options)
                           :on-success-fn #'on-close
                           :on-cancel-fn #'on-close)))))))

(define-slot (main-window directory-error) ((error-message string))
  (declare (connected main-window (directory-error string)))
  (flet ((remove-dialog (widget)
           (declare (ignore widget))
           (setf directory-error-dialog nil)
           t))
    (if directory-error-dialog
        (add-text directory-error-dialog error-message)
        (setf directory-error-dialog
              (make-instance
               'dialog
               :title "Error - Directory Watcher threw uncaught error"
               :text error-message
               :on-cancel-fn #'remove-dialog
               :on-success-fn #'remove-dialog)))))

(define-initializer (main-window setup-callbacks)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (declare (ignore event))
                              (signal! main-window (config-changed)))
                            :config-changed)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (signal! main-window (received-send-permission
                                                    string)
                                       (second event)))
                            :send-permission)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (signal! main-window (folder-download-error
                                                    string)
                                       (second event)))
                            :folder-download-error)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (signal! main-window (directory-error
                                                    string)
                                       (second event)))
                            :directory-error))

(define-finalizer (main-window cleanup-callbacks)
  (lodds.event:remove-callback :qt-main :config-changed)
  (lodds.event:remove-callback :qt-main :send-permission)
  (lodds.event:remove-callback :qt-main :folder-download-error)
  (lodds.event:remove-callback :qt-main :directory-error))

(defmethod fix-menubar-order ((main-window main-window))
  (with-slots-bound (main-window main-window)
    (let* ((menu-bar (q+:menu-bar main-window)))
      (with-finalizing ((menu (q+:make-qmenu)))
        (let ((order (list (cons "Lodds" nil)
                           (cons "View" nil)
                           (cons "Help" nil))))
          (loop :for child :in (find-children menu-bar menu)
                :collect (let ((entry (find (q+:title child) order
                                            :test (lambda (a b)
                                                    (string= a (car b))))))
                           (when entry
                             (setf (cdr entry) child))))
          (q+:clear menu-bar)
          (loop :for (childname . child) :in order
                :do (q+:add-menu menu-bar child)))))))

(defmethod setup-tray-icon ((main-window main-window))
  (with-slots-bound (main-window main-window)
    (q+:set-icon tray-icon (q+:window-icon main-window))
    (with-finalizing ((menu (q+:make-qmenu)))
      (q+:set-context-menu
       tray-icon
       (loop :for menu
             :in (find-children (q+:menu-bar main-window)
                                menu)
             :if (string= (q+:title menu) "Lodds")
             :do (return menu))))
    (q+:show tray-icon)))

(define-slot (main-window reload-stylesheet) ()
  (declare (connected main-window (reload-stylesheet)))
  (q+:set-style-sheet main-window *style-sheet*))

(defun on-error (&rest args)
  (format t "ERROR:---------------------------------------~%")
  (format t "~a~%" args)
  (format t "---------------------------------------------~%")
  (apply #'qui:invoke-gui-debugger args))

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
                         :on-error #'on-error)
        (q+:qapplication-set-quit-on-last-window-closed nil)
        (setf *main-window* window)
        (fix-menubar-order window)
        (setup-tray-icon window))
      (unless server-given-p
        (lodds:shutdown)))))
