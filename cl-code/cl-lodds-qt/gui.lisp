(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget main-window (QMainWindow) ())

(define-menu (main-window File)
  (:item ("Run" (ctrl r))
         (flet ((run () (progn
                          (lodds.subsystem:start (lodds:get-subsystem :event-queue))
                          (lodds.subsystem:start (lodds:get-subsystem :tasker))
                          (lodds.subsystem:start (lodds:get-subsystem :listener))
                          (lodds.subsystem:start (lodds:get-subsystem :advertiser))
                          (lodds.subsystem:start (lodds:get-subsystem :handler)))))
           (if (lodds:interface lodds:*server*)
               (run)
               (make-instance 'dialog
                              :title "Error - Interface not set!"
                              :text "Please select a Interface first."
                              :widget (make-instance 'interface)
                              :on-success-fn (lambda (widget)
                                               (declare (ignore widget))
                                               (when (lodds:interface lodds:*server*)
                                                 (run))
                                               t)))))
  (:item ("Stop" (ctrl s))
         (progn
           (lodds.subsystem:stop (lodds:get-subsystem :tasker))
           (lodds.subsystem:stop (lodds:get-subsystem :listener))
           (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
           (lodds.subsystem:stop (lodds:get-subsystem :handler))
           (lodds.subsystem:stop (lodds:get-subsystem :watcher))))
  (:separator)
  (:item "Reload Stylesheet"
         (q+:set-style-sheet main-window *style-sheet*))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-subwidget (main-window view-menu) (q+:add-menu (q+:menu-bar main-window)
                                                       "View"))

(define-subwidget (main-window shares-widget) (make-instance 'shares))

(define-subwidget (main-window log-dock)
    (make-instance 'dock :title "Log"
                         :widget (make-instance 'info-log)
                         :main-window main-window
                         :side :bottom
                         :menu view-menu))

(define-subwidget (main-window download-dock)
    (make-instance 'dock :title "Download"
                         :widget (make-instance 'download)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-subwidget (main-window user-dock)
    (make-instance 'dock :title "User List"
                         :widget (make-instance 'user-list)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-subwidget (main-window shared-dock)
    (make-instance 'dock :title "Directories Shared"
                         :widget (make-instance 'shared)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-subwidget (main-window info-dock)
    (make-instance 'dock :title "Info"
                         :widget (make-instance 'info)
                         :main-window main-window
                         :side :right
                         :menu view-menu))

(define-initializer (main-window setup-widget)
  (connect shares-widget
           "itemClicked(QTreeWidgetItem *, int)"
           (lambda (selected-item column)
             (declare (ignore column))
             (apply #'update-download
                    (slot-value download-dock 'widget)
                    (get-selected-file shares-widget selected-item))))
  (qdoto main-window
         (q+:set-window-title (format nil "LODDS - ~a" (lodds:name lodds:*server*)))
         (q+:set-window-icon (q+:make-qicon "./res/lodds.png"))
         (q+:resize 800 450)
         (q+:set-style-sheet *style-sheet*)
         (q+:set-central-widget shares-widget)))

(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window fix-menubar-order) ())
(define-signal (main-window change-title) (string))
(define-signal (main-window received-send-permission) (string))
(define-signal (main-window directory-error) (string))

(define-slot (main-window received-send-permission) ((task-id string))
  (declare (connected main-window (received-send-permission string)))
  (let ((task (lodds.task:remove-task-from-hold task-id)))
    (when task
      (open-send-permission-dialog task))))

(define-slot (main-window directory-error) ((task-id string))
  (declare (connected main-window (directory-error string)))
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
                           :widget (make-instance 'directory-error
                                                  :title "Solutions:"
                                                  :solutions options)
                           :on-success-fn #'on-close
                           :on-cancel-fn #'on-close)))))))

(define-initializer (main-window setup-callbacks)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (declare (ignore event))
                              (signal! main-window
                                       (change-title string)
                                       (format nil "LODDS - ~a"
                                               (lodds:name lodds:*server*))))
                            :name-changed)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (signal! main-window (received-send-permission
                                                    string)
                                       (second event)))
                            :send-permission)
  (lodds.event:add-callback :qt-main
                            (lambda (event)
                              (format t "iam here on catching signals~%")
                              (signal! main-window (directory-error
                                                    string)
                                       (second event)))
                            :directory-error))

(define-finalizer (main-window cleanup-callbacks)
  (lodds.event:remove-callback :qt-main :name-changed)
  (lodds.event:remove-callback :qt-main :send-permission)
  (lodds.event:remove-callback :qt-main :directory-error))

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

(define-slot (main-window change-title) ((new-title string))
  (declare (connected main-window (change-title string)))
  (q+:set-window-title main-window
                       new-title))

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
        (setf *main-window* window)
        (signal! window (fix-menubar-order)))
      (unless server-given-p
        (lodds:shutdown)))))
