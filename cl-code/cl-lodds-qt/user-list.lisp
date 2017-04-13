(in-package #:lodds-qt)
(in-readtable :qtools)

;; user-list columns
(defvar +user-list-name+ 0)
(defvar +user-list-load+ 1)
(defvar +user-list-send-file+ 2)
(defvar +user-list-full-name+ 3)

(define-widget user-list (QTreeWidget)
  ((users :initform (make-hash-table :test 'equal)
          :accessor users)))

(define-signal (user-list add-user) (string string int))
(define-signal (user-list remove-user) (string))
(define-signal (user-list update-user) (string string int))

(defun gen-tool-tip (user)
  (let ((user-info (lodds:get-user-info user)))
    (when user-info
      (let ((all-files (hash-table-count (lodds:user-file-table-name user-info)))
            (unique-files (hash-table-count (lodds:user-file-table-hash user-info)))
            (last-change (lodds:user-last-change user-info)))
        (lodds.core:split-user-identifier (name ip port) user
          (format nil "Ip: ~a~%Port: ~a~%Last Change: ~a~%Shared: ~a"
                  ip
                  port
                  (if (eql 0 last-change)
                      "-"
                      (lodds.core:format-timestamp last-change))
                  (format nil "~:d Files (~:d unique)"
                          (or all-files 0)
                          (or unique-files 0))))))))

(defmethod set-user-color ((user-list user-list) user widget)
  (with-slots-bound (user-list user-list)
    (with-finalizing* ((qcolor-trusted (q+:make-qcolor 0 255 0 22))
                       (qcolor-blocked (q+:make-qcolor 255 0 0 22))
                       (qcolor-default (q+:make-qcolor 0 0 0 0))
                       (qbrush (q+:make-qbrush
                                (cond ((lodds:user-is-blocked user)
                                       qcolor-blocked)
                                      ((lodds:user-is-trusted user)
                                       qcolor-trusted)
                                      (t
                                       qcolor-default)))))
      (q+:set-background widget
                         +user-list-name+
                         qbrush))))

(define-slot (user-list add-user) ((user string)
                                   (load string)
                                   (last-change int))
  (declare (connected user-list (add-user string
                                          string
                                          int)))
  (let* ((new-entry (q+:make-qtreewidgetitem user-list))
         (send-file-button (q+:make-qpushbutton user-list)))
    (set-icon send-file-button "send-file.png" "Send File")
    (connect send-file-button "pressed()"
             (lambda ()
               (open-send-file-dialog user)))
    (q+:set-item-widget user-list
                        new-entry
                        +user-list-send-file+
                        send-file-button)
    (set-user-color user-list user new-entry)
    (lodds.core:split-user-identifier (name ip port) user
      (q+:set-tool-tip send-file-button
                       (format nil
                               "Click to select and send a file to user ~a"
                               name))
      (qdoto new-entry
             (q+:set-text +user-list-name+ name)
             (q+:set-status-tip +user-list-name+
                                (format nil "User: ~a" user))
             (q+:set-tool-tip +user-list-name+
                              (or (gen-tool-tip user) ""))
             (q+:set-text +user-list-load+
                          (lodds.core:format-size (parse-integer load) ""))
             (q+:set-tool-tip +user-list-load+
                              (format nil "Load of the User, is used~%~
                                          to calculate the best user~%~
                                          on file downloads. The load~%~
                                          describes the size of~%~
                                          outstanding bytes the User~%~
                                          has to transfer."))
             (q+:set-text-alignment +user-list-load+
                                    (qt:enum-or (q+:qt.align-center)
                                                (q+:qt.align-right)))
             (q+:set-text +user-list-full-name+ user)))
    (setf (gethash user (users user-list)) new-entry)
    (q+:update-geometries user-list)))

(define-slot (user-list remove-user) ((user string))
  (declare (connected user-list (remove-user string)))
  (let ((widget (gethash user (users user-list))))
    (when widget
      (q+:take-top-level-item user-list
                              (q+:index-of-top-level-item user-list widget))
      (remhash user (users user-list)))))

(define-slot (user-list update-user) ((user string)
                                      (load string)
                                      (last-change int))
  (declare (connected user-list (update-user string
                                             string
                                             int)))
  (let ((entry (gethash user (users user-list))))
    (when entry
      (qdoto entry
             (q+:set-text +user-list-load+
                          (lodds.core:format-size (parse-integer load) ""))
             (q+:set-tool-tip +user-list-name+ (or (gen-tool-tip user) ""))))))

(define-slot (user-list prepare-menu) ((pos "const QPoint &"))
  (declare (connected user-list (custom-context-menu-requested "const QPoint &")))
  (let ((widget (q+:item-at user-list pos)))
    (when (qobject-alive-p widget)
      (with-finalizing ((global-pos (q+:map-to-global user-list pos))
                        (menu (q+:make-qmenu)))
        (let ((user (q+:text widget +user-list-full-name+)))
          (if (lodds:user-is-trusted user)
              (q+:add-action menu "Untrust")
              (q+:add-action menu "Trust"))
          (if (lodds:user-is-blocked user)
              (q+:add-action menu "Unblock")
              (q+:add-action menu "Block"))
          (let ((option (q+:exec menu global-pos)))
            (unless (null-qobject-p option)
              (lodds.core:str-case (q+:text option)
                ("Trust"   (lodds:trust-user   user))
                ("Untrust" (lodds:untrust-user user))
                ("Block"   (lodds:block-user   user))
                ("Unblock" (lodds:unblock-user user))))))))))

(define-initializer (user-list setup-widget)
  (qdoto user-list
         (q+:set-mouse-tracking t)
         (q+:set-object-name "UserList")
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 4)
         (q+:set-header-labels (list "User" "Load" "" ""))
         (q+:set-alternating-row-colors t)
         (q+:set-accept-drops t)
         (q+:set-context-menu-policy (q+:qt.custom-context-menu))
         (q+:hide-column +user-list-full-name+))

  (qdoto (q+:header user-list)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +user-list-name+ (q+:qheaderview.stretch))
         (q+:resize-section +user-list-load+ (lodds.config:get-value :size-column-width))
         (q+:set-resize-mode +user-list-send-file+ (q+:qheaderview.resize-to-contents))))

(define-override (user-list drag-enter-event) (ev)
  (when (q+:has-urls (q+:mime-data ev))
    (q+:accept-proposed-action ev)))

(define-override (user-list drag-move-event) (ev)
  (q+:accept-proposed-action ev))

(define-override (user-list drop-event) (ev)
  (dolist (filepath (format-dropped-links ev))
    (cond
      ((lodds.core:directory-exists filepath)
       (make-instance
        'dialog
        :title (format nil "Error - Cannot send directory (~a)"
                       filepath)
        :text "Its not possible to send a directory, select a file please."))
      ((lodds.core:file-exists filepath)
       (let ((item (q+:item-at user-list (q+:pos ev))))
         (open-send-file-dialog
          (when (qobject-alive-p item)
            (q+:text item +user-list-full-name+))
          filepath)))
      (t
       (make-instance
        'dialog
        :title "Error - Dont know what to do"
        :text "Whatever you dropped there is neither a dir nor a file.")))))

(defmethod update-user-colors ((user-list user-list))
  (maphash (lambda (user widget)
             (set-user-color user-list user widget))
           (slot-value user-list 'users)))

(define-initializer (user-list setup-callbacks)
  (lodds.event:add-callback :qt-user-list
                            (lambda (name load last-change)
                              (signal! user-list
                                       (add-user string string int)
                                       name
                                       (prin1-to-string load)
                                       last-change))
                            :user-added)
  (lodds.event:add-callback :qt-user-list
                            (lambda (user)
                              (signal! user-list
                                       (remove-user string)
                                       user))
                            :user-removed)
  (lodds.event:add-callback :qt-user-list
                            (lambda (name load last-change)
                              (signal! user-list
                                       (update-user string string int)
                                       name
                                       (prin1-to-string load)
                                       last-change))
                            :user-updated))

(define-initializer (user-list setup-add-users)
  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add user
              (signal! user-list
                       (add-user string string int)
                       user
                       (prin1-to-string (lodds:user-load user-info))
                       (lodds:user-last-change user-info)))))

(define-finalizer (user-list cleanup-callbacks)
  (lodds.event:remove-callback :qt-user-list :user-added)
  (lodds.event:remove-callback :qt-user-list :user-removed)
  (lodds.event:remove-callback :qt-user-list :user-updated))
