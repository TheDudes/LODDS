#|

This file contains the menus, the main-window and related
functionality (for example system tray and status-info)

|#

(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ((directory-error-dialog :initform nil
                           :documentation "If a directory-error-dialog
                            is displayed this slot is set to it. This
                            way messages can be added to the dialog,
                            instead of opening multiple dialogs")
   (tray-info-blocked :initform nil
                      :documentation "Flag to disable the
                      tray-info. If the User Clicks the info message
                      this slot will be set to t.")
   (send-permission-dialogs :initform (list)
                            :documentation "alist of Send Permissions
                            which have been received. If the Main
                            Window is hidden only a Tray Message will
                            be displayed and the dialog will be hidden
                            added to this slot. If the User clicks the
                            message all dialogs will be shown.")
   (folder-error-dialogs :initform (list)
                         :documentation "alist of Folder Error Dialogs
                         which occured. If the Main Window is hidden
                         only a Tray Message will be displayed and the
                         dialog will be hidden and added to this
                         slot. If the User clicks the message all
                         dialogs will be shown.")
   (last-tray-message :initform nil
                      :documentation "Keyword describing the last
                      displayed tray message. Can be one of :info,
                      :send-permission or :folder-error")
   (callback-buffer :initform (make-hash-table :test 'equal)
                    :documentation "Buffer which is filled by the
                    event thread (when callbacks are called) to
                    transfer values over to the qt thread")))

(defun run ()
  (if (lodds.config:get-value :interface)
      (lodds:start)
      (make-instance
       'dialog
       :title "Error - Interface not set!"
       :text "Please select a interface first."
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
                       (lodds:start)
                       t)))
               t))))))

;; Copied system-about and About Menu Entry from Lionchat.
;; modified it to my needs, but all the credit goes to Shinmera
;; see: https://github.com/shirakumo/lionchat
(defun system-about ()
  (let ((system (asdf:find-system :cl-lodds-qt)))
    (format nil "<h2>About ~a</h2>
                <h4>~a</h4>
                License: ~a<br />
                Homepage: <a href=\"~a~:*\">~a</a><br />
                Author: ~a<br />
                Version: ~a<br />
                <br />
                LODDS (Local Open Distributed Data Sharing) is a
                protocol for filesharing and the base for this
                program. This program is a Qt gui client which
                implements the LODDS Protocol and can be used to
                share and exchange files with others inside a local
                network.<br />
                <br />
                Some Icons are made by
                <a href=\"https://icons8.com/free-ios-7-icons-in-vector/\">
                  icons8
                </a> (downloaded from
                <a href=\"https://www.iconfinder.com/iconsets/ios-7-icons\">
                  here
                </a>) and are licensed under the
                <a href=\"https://creativecommons.org/licenses/by-nd/3.0/\">
                  Creative Commons Attribution-No Derivative Works 3.0 Unported
                </a> License"
            :cl-lodds-qt
            (asdf:system-description system)
            (asdf:system-license system)
            (asdf:system-homepage system)
            (asdf:system-author system)
            (asdf:component-version system))))

(defun used-libraries ()
  (format nil "~{~a~^<br /><br />~}"
          (loop :for name
                :in (sort
                     (remove-duplicates
                      (append (asdf:system-depends-on
                               (asdf:find-system :cl-lodds-qt))
                              (asdf:system-depends-on
                               (asdf:find-system :cl-lodds))))
                     #'string<)
                :collect
                (let ((system (asdf:find-system name)))
                  (format nil
                          "<h1>~a </h1><a href=\"~a~:*\">~a</a>
                          <h4>~a</h4>
                          Licence: ~a<br />
                          Author: ~a"
                          name
                          (or (asdf:system-homepage system) "")
                          (or (asdf:system-description system) "(not specified)")
                          (or (asdf:system-license system) "(not specified)")
                          (or (asdf:system-author system) "(not specified)"))))))

(defun restart-lodds (main-window)
  (lodds:stop)
  (let* ((timer (qdoto (q+:make-qtimer)
                       (q+:start 100))))
    (connect timer "timeout()"
             (let ((time 0))
               (lambda ()
                 (incf time 1)
                 (q+:show-message
                  (q+:status-bar main-window)
                  (format nil "Restarting lodds ~c"
                          (aref #(#\/ #\- #\\ #\|)
                                (mod time 4))))
                 (when (> time 50)
                   (finalize timer)
                   (q+:clear-message (q+:status-bar main-window))
                   (lodds:start)))))))

(define-menu (main-window Help)
  (:item "&Intro"
         (make-instance 'dialog
                        :widget
                        (qdoto (q+:make-qtextbrowser)
                               (q+:set-read-only t)
                               (q+:set-open-external-links t)
                               (q+:append *intro*)
                               (q+:move-cursor (q+:qtextcursor.start)))
                        :width 400
                        :height 600
                        :title "Intro"))
  (:item "&About"
         (finalize
          (qdoto (q+:make-qmessagebox)
                 (q+:set-window-title "About Lodds")
                 (q+:set-icon-pixmap (q+:pixmap (q+:window-icon main-window) 64 64))
                 (q+:set-text (system-about))
                 (q+:exec))))
  (:item "About&Qt"
         (finalize (q+:qmessagebox-about-qt main-window
                                            "About Qt")))
  (:item "&Used Libraries"
         (make-instance 'dialog
                        :widget
                        (qdoto (q+:make-qtextbrowser)
                               (q+:set-read-only t)
                               (q+:set-open-external-links t)
                               (q+:append (used-libraries))
                               (q+:move-cursor (q+:qtextcursor.start)))
                        :width 500
                        :height 500
                        :title "Used Libraries")))

(define-menu (main-window Lodds)
  (:item ("&Run" (ctrl r))
         (run))
  (:item ("&Stop" (ctrl s))
         (lodds:stop))
  (:item ("&Restart" (ctrl e))
         (restart-lodds main-window))
  (:separator)
  (:item "&Reload Stylesheet"
         (signal! main-window (reload-stylesheet)))
  (:separator)
  (:item ("Share &Directory" (ctrl o))
         (let ((dir (q+:qfiledialog-get-existing-directory)))
           (when (> (length dir) 0)
             (let* ((dock (slot-value main-window 'shared-dock))
                    (shared (slot-value dock 'widget))
                    (shared-directories (slot-value shared 'shared-directories)))
               (share-directories shared-directories
                                  (list dir))))))
  (:separator)
  (:item ("&Settings" (ctrl c))
         (make-setting-dialog))
  (:separator)
  (:item ("&Quit" (ctrl q))
         (signal! main-window (shutdown))))

(define-subwidget (main-window tray-icon)
    (q+:make-qsystemtrayicon main-window)
  (q+:set-tool-tip tray-icon
                   (format nil "LODDS - ~a"
                           (lodds.config:get-value :name))))

(defmethod show-pending-send-requests ((main-window main-window))
  (with-slots-bound (main-window main-window)
    (loop :for (path . dialog)
          :in send-permission-dialogs
          :do (when dialog
                (q+:show dialog)))
    (setf send-permission-dialogs (list))))

(defmethod show-pending-folder-errors ((main-window main-window))
  (with-slots-bound (main-window main-window)
    (loop :for (task-id . dialog)
          :in folder-error-dialogs
          :do (q+:show dialog))
    (setf folder-error-dialogs (list))))

(define-slot (main-window tray-activated) ((reason "QSystemTrayIcon::ActivationReason"))
  (declare (connected tray-icon (activated "QSystemTrayIcon::ActivationReason")))
  (when (or (qt:enum-equal reason (q+:qsystemtrayicon.trigger))
            (qt:enum-equal reason (q+:qsystemtrayicon.double-click)))
    (if (q+:is-visible main-window)
        (q+:hide main-window)
        (progn
          (q+:show main-window)
          (show-pending-send-requests main-window)
          (show-pending-folder-errors main-window)))))

(define-override (main-window close-event) (ev)
  (if (lodds.config:get-value :minimize-to-tray)
      (progn
        (q+:hide main-window)
        (q+:ignore ev)
        (unless tray-info-blocked
          (setf last-tray-message :info)
          (when (q+:qsystemtrayicon-supports-messages)
            (q+:show-message tray-icon
                             "Lodds Minimized"
                             (format nil
                                     "Lodds is still running in the~%~
                                     background. This behaviour can~%~
                                     be changed in the settings. Click~%~
                                     to not show this message again.")))))
      (q+:qcoreapplication-quit)))

(define-slot (main-window tray-message-clicked) ()
  (declare (connected tray-icon (message-clicked)))
  (case last-tray-message
    (:send-permission (show-pending-send-requests main-window))
    (:folder-error (show-pending-folder-errors main-window))
    (:info (setf tray-info-blocked t))))

(define-subwidget (main-window view-menu) (q+:add-menu (q+:menu-bar main-window)
                                                       "View"))

(define-subwidget (main-window shares-widget)
    (make-instance 'shares
                   :main-window main-window))

(define-subwidget (main-window log-dock)
    (make-instance 'dock :title "Log"
                         :widget (make-instance 'info-log)
                         :main-window main-window
                         :side nil
                         :menu view-menu))

(define-subwidget (main-window user-dock)
    (make-instance 'dock :title "User List"
                         :widget (make-instance 'user-list)
                         :main-window main-window
                         :side :left
                         :width 300
                         :menu view-menu))

(define-subwidget (main-window info-dock)
    (make-instance 'dock :title "Info"
                         :widget (make-instance 'info)
                         :main-window main-window
                         :side :bottom
                         :height 150
                         :menu view-menu))

(define-subwidget (main-window shared-dock)
    (make-instance 'dock :title "Directories Shared"
                         :widget (make-instance 'shared)
                         :main-window main-window
                         :side :right
                         :width 300
                         :menu view-menu))

(define-subwidget (main-window status-timer)
    (q+:make-qtimer main-window)
  (q+:start status-timer
            (lodds.config:get-value :status-update-interval)))

(define-subwidget (main-window status-label)
    (q+:make-qlabel main-window)
  (q+:set-tool-tip status-label
                   (format nil
                           "Describes Lodds current status.~%~
                           Total Load: Sum of all Loads accross the Network~%~
                           Load: How much load the client currently has~%~
                           Tasks: How many tasks are currently running~%~
                           Shared Folders: Amount of currently shared folders~%~
                           Users: Amount of User on the Network~%~
                           Network Files: Amount of Files in the network (non Unique)~%~
                           Network Size: Size of all shared Files")))

(define-slot (main-window tick) ()
  (declare (connected status-timer (timeout)))
  (let ((status (append (mapcar
                         (lambda (status)
                           (format nil "~a: ~a" (car status) (cdr status)))
                         (lodds:get-status t))
                        (list
                         (format nil "Network Size: ~a"
                                 (lodds.core:format-size
                                  (get-total-shares-size shares-widget)))))))
    (q+:set-text status-label
                 (format nil "~{~a~^ | ~}" status))
    (q+:set-tool-tip tray-icon
                     (format nil "~a~%~{~a~^~%~}"
                             (q+:window-title main-window)
                             (append
                              status
                              (append
                               (let ((requests 0))
                                 (loop :for (path . dialog) :in send-permission-dialogs
                                       :when dialog
                                       :do (incf requests))
                                 (when (> requests 0)
                                   (list
                                    (format nil "Send Requests: ~a" requests))))
                               (let ((folder-errors (length folder-error-dialogs)))
                                 (unless (eql 0 folder-errors)
                                   (list
                                    (format nil "Folder Errors: ~a" folder-errors))))))))))

(define-initializer (main-window setup-widget)
  (let ((lodds-icon (format nil "~a~a" (lodds.config:get-value :resources-folder)
                            "lodds.png")))
    (if (lodds.core:file-exists lodds-icon)
        (q+:set-window-icon main-window
                            (q+:make-qicon lodds-icon))
        (let ((text (format nil "Could not find lodds icon ~a"
                            lodds-icon)))
          (format t "~a~%" text)
          (lodds.event:push-event :info text))))
  (q+:add-permanent-widget (q+:status-bar main-window)
                           status-label)
  (qdoto main-window
         (q+:set-window-title (format nil "LODDS - ~a" (lodds.config:get-value :name)))
         (q+:set-central-widget shares-widget)
         (q+:resize 1280 720))
  (signal! main-window (reload-stylesheet)))

(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window config-changed) ())
(define-signal (main-window received-send-permission) (string))
(define-signal (main-window folder-download-error) (string))
(define-signal (main-window directory-error) (string))
(define-signal (main-window shutdown) ())

(define-slot (main-window shutdown) ()
  (declare (connected main-window (shutdown)))
  (q+:close main-window)
  (q+:qcoreapplication-quit))

(define-slot (main-window config-changed) ()
  (declare (connected main-window (config-changed)))
  (let ((title (format nil "LODDS - ~a"
                       (lodds.config:get-value :name))))
    (q+:set-window-title main-window title)
    (q+:set-tool-tip tray-icon title))
  (set-refresh-timeout (slot-value info-dock 'widget)
                       (lodds.config:get-value :info-update-interval))
  (q+:set-interval status-timer
                   (lodds.config:get-value :status-update-interval))
  (set-directory-busy-check-timeout (slot-value shared-dock 'widget)
                                    (lodds.config:get-value :directory-busy-check))
  (update-user-colors (slot-value user-dock 'widget))
  (setf (slot-value (slot-value log-dock 'widget)
                    'log-message-max)
        (lodds.config:get-value :log-message-max)))

(define-slot (main-window received-send-permission) ((task-id string))
  (declare (connected main-window (received-send-permission string)))
  (apply #'open-send-permission-dialog
         main-window
         task-id
         (gethash task-id callback-buffer))
  (remhash task-id callback-buffer))

(define-slot (main-window folder-download-error) ((task-id string))
  (declare (connected main-window (folder-download-error string)))
  (destructuring-bind (folder error-file retry-fn skip-fn abort-fn)
      (gethash task-id callback-buffer)
    (remhash task-id callback-buffer)
    (let ((options (list (list :skip
                               "skip current file")
                         (list :abort
                               "abort directory download")
                         (list :retry
                               "retry loading file"))))
      (flet ((on-close (widget)
               (funcall
                (case (get-selected-solution widget)
                  (:skip skip-fn)
                  (:abort abort-fn)
                  (:retry retry-fn)))
               t))
        (let* ((dialog (make-instance
                        'dialog
                        :title "Error - File from directory download failed"
                        :text (format nil
                                      "File ~a which is part of directory download (~a) failed"
                                      error-file
                                      folder)
                        :widget (make-instance 'selection
                                               :title "Solutions:"
                                               :solutions options)
                        :on-success-fn #'on-close
                        :on-cancel-fn #'on-close))
               (list-entry (cons task-id dialog)))
          (when (and (q+:is-hidden main-window)
                     (q+:qsystemtrayicon-supports-messages))
            (q+:hide dialog)
            (push list-entry folder-error-dialogs)
            (setf last-tray-message :folder-error)
            (q+:show-message tray-icon
                             "Error downloading folder"
                             (format nil
                                     "There was a error downloading file~%~
                                         ~a~%~
                                         of Folder~%~
                                         ~a~%~
                                         Click message or tray icon to fix."
                                     error-file
                                     folder))))))))

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
               :title "Error - Directory watcher threw uncaught error"
               :text error-message
               :on-cancel-fn #'remove-dialog
               :on-success-fn #'remove-dialog)))))

(define-initializer (main-window setup-callbacks)
  (lodds.event:add-callback :qt-main
                            (lambda (&rest args)
                              (declare (ignore args))
                              (signal! main-window (config-changed)))
                            :config-changed)
  (lodds.event:add-callback :qt-main
                            (lambda (task-id &rest args)
                              (setf (gethash task-id callback-buffer)
                                    args)
                              (signal! main-window
                                       (received-send-permission string)
                                       task-id))
                            :send-permission)
  (lodds.event:add-callback :qt-main
                            (lambda (task-id &rest args)
                              (setf (gethash task-id callback-buffer)
                                    args)
                              (signal! main-window
                                       (folder-download-error string)
                                       task-id))
                            :folder-download-error)
  (lodds.event:add-callback :qt-main
                            (lambda (error-msg)
                              (signal! main-window
                                       (directory-error string)
                                       error-msg))
                            :directory-error)
  (lodds.event:add-callback :qt-main
                            (lambda (&rest args)
                              (declare (ignore args))
                              (signal! main-window
                                       (shutdown)))
                            :shutdown))

(define-finalizer (main-window cleanup-callbacks)
  (lodds.event:remove-callback :qt-main :config-changed)
  (lodds.event:remove-callback :qt-main :send-permission)
  (lodds.event:remove-callback :qt-main :folder-download-error)
  (lodds.event:remove-callback :qt-main :directory-error)
  (lodds.event:remove-callback :qt-main :shutdown))

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
  (let ((style-sheet (format nil "~a~a"
                             (lodds.config:get-value :resources-folder)
                             "style-sheet.qss")))
    (if (lodds.core:file-exists style-sheet)
        (q+:set-style-sheet main-window
                            (uiop:read-file-string
                             (lodds.core:escape-wildcards style-sheet)))
        (let ((text (format nil "Could not find stylesheet ~a"
                            style-sheet)))
          (q+:set-style-sheet main-window
                              "")
          (lodds.event:push-event :info text)
          (format t "~a~%" text)))))

(defun on-error (&rest args)
  (format t "ERROR:---------------------------------------~%")
  (format t "~a~%" args)
  (format t "---------------------------------------------~%")
  (apply #'qui:invoke-gui-debugger args))

(defparameter *main-window* nil
  "Contains the main-window, usefull to debug/inspect gui widgets.")

(defun main (&optional (lodds-server (make-instance 'lodds:lodds-server) server-given-p))
  ;; so iam calling tmt:with-body-in-main-thread here myself and set
  ;; :main-thread to nil on with-main-window. This way lodds-server
  ;; can be dynamically bound to lodds::*server* with
  ;; lodds:with-server and is available on the main thread.
  (tmt:with-body-in-main-thread ()
    (lodds.core:with-server lodds-server
      (with-main-window (window (make-instance 'main-window)
                         :main-thread nil
                         :on-error #'on-error)
        (q+:qapplication-set-quit-on-last-window-closed nil)
        (setf *main-window* window)
        (fix-menubar-order window)
        (setup-tray-icon window)
        (unless server-given-p
          (lodds:start)))
      (unless server-given-p
        (lodds:shutdown)))))
