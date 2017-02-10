(in-package #:lodds-qt)
(in-readtable :qtools)

;; user-list columns
(defvar +user-list-name+ 0)
(defvar +user-list-ip+ 1)
(defvar +user-list-port+ 2)
(defvar +user-list-load+ 3)
(defvar +user-list-last-change+ 4)
(defvar +user-list-send-file+ 5)
(defvar +user-list-id+ 6)

(define-widget user-list (QTreeWidget) ())

(define-signal (user-list add-user) (string string string))
(define-signal (user-list remove-user) (string))
(define-signal (user-list update-user) (string string string))

(define-slot (user-list add-user) ((user string)
                                   (load string)
                                   (last-change string))
  (declare (connected user-list (add-user string
                                          string
                                          string)))
  (lodds.core:split-user-identifier (name ip port) user
    (let* ((entry-id (concatenate 'string "user:" user))
           (new-entry (q+:make-qtreewidgetitem user-list))
           (send-file-button (q+:make-qpushbutton "Send File" user-list)))
      (connect send-file-button "pressed()"
               (lambda ()
                 (format t "TODO: implement SendFile (entry-id: ~a)~%" entry-id)))
      (q+:set-item-widget user-list
                          new-entry
                          +user-list-send-file+
                          send-file-button)
      (qdoto new-entry
             (q+:set-text +user-list-id+ entry-id)
             (q+:set-text +user-list-name+ name)
             (q+:set-text +user-list-ip+ ip)
             (q+:set-text +user-list-port+ port)
             (q+:set-text +user-list-load+ (lodds.core:format-size (parse-integer load)))
             (q+:set-text +user-list-last-change+ last-change)
             (q+:set-text +user-list-id+ entry-id)
             (q+:set-text-alignment +user-list-load+ (q+:qt.align-right)))
      (setf (gethash entry-id *id-mapper*) new-entry))))

(define-slot (user-list remove-user) ((user string))
  (declare (connected user-list (remove-user string)))
  (lodds.core:split-user-identifier (name ip port) user
    (loop :for i :from 0 :below (q+:top-level-item-count user-list)
          :do (let ((child (q+:top-level-item user-list i)))
                (when (and (string= name (q+:text child +user-list-name+))
                           (string= ip (q+:text child +user-list-ip+))
                           (string= port (q+:text child +user-list-port+)))
                  (remhash (q+:text child +user-list-id+)
                           *id-mapper*)
                  (q+:take-top-level-item user-list i)
                  (return))))))

(define-slot (user-list update-user) ((user string)
                                      (load string)
                                      (last-change string))
  (declare (connected user-list (update-user string
                                             string
                                             string)))
  (let ((entry (gethash (concatenate 'string "user:" user)
                        *id-mapper*)))
    (when entry
      (qdoto entry
             (q+:set-text +user-list-load+
                          (lodds.core:format-size (parse-integer load)))
             (q+:set-text +user-list-last-change+
                          last-change)))))

(define-initializer (user-list setup)
  (qdoto user-list
         (q+:set-column-count 7)
         (q+:set-header-labels (list "User" "IP" "Port" "Load" "Last Change" "" ""))
         (q+:hide-column +user-list-id+)
         (q+:set-alternating-row-colors t))

  (qdoto (q+:header user-list)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +user-list-name+ (q+:qheaderview.stretch))
         (q+:set-resize-mode +user-list-ip+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +user-list-port+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +user-list-load+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +user-list-last-change+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +user-list-send-file+ (q+:qheaderview.resize-to-contents)))
  (lodds.event:add-callback :qt-user-list
                            (lambda (event)
                              (destructuring-bind (name load last-change) (cdr event)
                                (signal! user-list
                                         (add-user string string string)
                                         name
                                         (prin1-to-string load)
                                         (prin1-to-string last-change))))
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
                                         (update-user string string string)
                                         name
                                         (prin1-to-string load)
                                         (prin1-to-string last-change))))
                            :client-updated)

  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add user
              (signal! user-list
                       (add-user string string string)
                       user
                       (prin1-to-string (lodds:c-load user-info))
                       (prin1-to-string (lodds:c-last-change user-info))))))

(define-finalizer (user-list cleanup)
  (lodds.event:remove-callback :qt-user-list :client-added)
  (lodds.event:remove-callback :qt-user-list :client-removed)
  (lodds.event:remove-callback :qt-user-list :client-updated))
