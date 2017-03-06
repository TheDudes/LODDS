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
  (let* ((client-info (lodds:get-user-info user))
         (all-files (hash-table-count (lodds:c-file-table-name client-info)))
         (unique-files (hash-table-count (lodds:c-file-table-hash client-info)))
         (last-change (lodds:c-last-change client-info)))
    (lodds.core:split-user-identifier (name ip port) user
      (format nil "Ip: ~a~%Port: ~a~%Last Change: ~a~%Shared: ~a"
              ip
              port
              (if (eql 0 last-change)
                  "-"
                  (generate-timestamp last-change))
              (format nil "~:d Files (~:d unique)"
                      (or all-files 0)
                      (or unique-files 0))))))

(define-slot (user-list add-user) ((user string)
                                   (load string)
                                   (last-change int))
  (declare (connected user-list (add-user string
                                          string
                                          int)))
  (let* ((new-entry (q+:make-qtreewidgetitem user-list))
         (send-file-button (q+:make-qpushbutton "Send File" user-list)))
    (connect send-file-button "pressed()"
             (lambda ()
               (open-send-file-dialog user)))
    (q+:set-item-widget user-list
                        new-entry
                        +user-list-send-file+
                        send-file-button)
    (lodds.core:split-user-identifier (name ip port) user
      (q+:set-tool-tip send-file-button
                       (format nil
                               "Click to Select and Send a file to User ~a"
                               name))
      (qdoto new-entry
             (q+:set-text +user-list-name+ name)
             (q+:set-tool-tip +user-list-name+
                              (gen-tool-tip user))
             (q+:set-text +user-list-load+ (lodds.core:format-size (parse-integer load)))
             (q+:set-text-alignment +user-list-load+ (q+:qt.align-right))
             (q+:set-text +user-list-full-name+ user)))
    (setf (gethash user (users user-list)) new-entry)))

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
                          (lodds.core:format-size (parse-integer load)))
             (q+:set-tool-tip +user-list-name+ (gen-tool-tip user))))))

(define-initializer (user-list setup-widget)
  (qdoto user-list
         (q+:set-object-name "UserList")
         (q+:set-focus-policy (q+:qt.no-focus))
         (q+:set-selection-mode 0)
         (q+:set-column-count 4)
         (q+:set-header-labels (list "User" "Load" "" ""))
         (q+:set-alternating-row-colors t)
         (q+:set-accept-drops t)
         (q+:hide-column +user-list-full-name+))

  (qdoto (q+:header user-list)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +user-list-name+ (q+:qheaderview.stretch))
         (q+:resize-section +user-list-load+ 60)
         (q+:set-resize-mode +user-list-send-file+ (q+:qheaderview.resize-to-contents))))

(define-override (user-list drag-enter-event) (ev)
  (when (q+:has-urls (q+:mime-data ev))
    (q+:accept-proposed-action ev)))

(define-override (user-list drag-move-event) (ev)
  (q+:accept-proposed-action ev))

(define-override (user-list drop-event) (ev)
  (let ((dropped-link (q+:const-data
                       (q+:data (q+:mime-data ev)
                                "text/uri-list"))))
    (loop :for link
          :in (cl-strings:split dropped-link
                                (format nil "~C~C"
                                        #\return #\linefeed))
          :do
          (when (cl-strings:starts-with link "file://")
            (let ((filepath (subseq link 7)))
              (cond
                ((cl-fs-watcher:escaped-directory-exists-p filepath)
                 (make-instance
                  'dialog
                  :title (format nil "Error - Cannot send Directory (~a)"
                                 filepath)
                  :text "Its not possible to send a directory, select a file please."))
                ((cl-fs-watcher:escaped-file-exists-p filepath)
                 (let ((item (q+:item-at user-list (q+:pos ev))))
                   (open-send-file-dialog
                    (when (qobject-alive-p item)
                      (q+:text item +user-list-full-name+))
                    filepath)))
                (t
                 (make-instance
                  'dialog
                  :title "Error - Dont know what to do"
                  :text "Whatever you dropped there is neither a dir nor a file."))))))))

(define-initializer (user-list setup-callbacks)
  (lodds.event:add-callback :qt-user-list
                            (lambda (event)
                              (destructuring-bind (name load last-change) (cdr event)
                                (signal! user-list
                                         (add-user string string int)
                                         name
                                         (prin1-to-string load)
                                         last-change)))
                            :client-added)
  (lodds.event:add-callback :qt-user-list
                            (lambda (event)
                              (signal! user-list
                                       (remove-user string)
                                       (second event)))
                            :client-removed)
  (lodds.event:add-callback :qt-user-list
                            (lambda (event)
                              (destructuring-bind (name load last-change) (cdr event)
                                (signal! user-list
                                         (update-user string string int)
                                         name
                                         (prin1-to-string load)
                                         last-change)))
                            :client-updated))

(define-initializer (user-list setup-add-users)
  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add user
              (signal! user-list
                       (add-user string string int)
                       user
                       (prin1-to-string (lodds:c-load user-info))
                       (lodds:c-last-change user-info)))))

(define-finalizer (user-list cleanup-callbacks)
  (lodds.event:remove-callback :qt-user-list :client-added)
  (lodds.event:remove-callback :qt-user-list :client-removed)
  (lodds.event:remove-callback :qt-user-list :client-updated))
