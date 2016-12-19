(in-package #:lodds-qt)

(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter +log-message-maximum+ 1000)

(defparameter *ignored-log-events*
  (list :listener
        :advertiser))

;; list-of-shares columns
(defvar +los-name+ 0)
(defvar +los-size+ 1)
(defvar +los-checksum+ 2)
(defvar +los-id+ 3)

;; log columns
(defvar +log-time+ 0)
(defvar +log-event+ 1)
(defvar +log-message+ 2)
(defvar +log-count+ 3)

;; user-list columns
(defvar +user-list-name+ 0)
(defvar +user-list-ip+ 1)
(defvar +user-list-port+ 2)
(defvar +user-list-load+ 3)
(defvar +user-list-last-change+ 4)
(defvar +user-list-id+ 5)

;; directories-shared columns
(defvar +directories-shared-path+ 0)
(defvar +directories-shared-remove-button+ 1)

(defparameter *container*
  (make-hash-table :test 'equalp)
  "hash-table used to pass data between threads and ui Thread.")

(defparameter *container-lock*
  (bt:make-lock "container-lock")
  "hash-table lock to savetly store data in a secure way")

(defun container-put (data)
  "adds data to *container* in a save way and returns a id which can
  be used to retrieve the data again."
  (let ((id nil))
    (bt:with-lock-held (*container-lock*)
      (loop :for test = (format nil "~a" (random 1024))
            :while (gethash test *container*)
            :finally (setf id test))
      (setf (gethash id *container*)
            data))
    id))

(defun container-get (id &optional (delete-id-p t))
  "returns data corresponding to given id. When delete-id-p is t the
  id gets freed to be used again"
  (let ((data nil))
    (bt:with-lock-held (*container-lock*)
      (setf data (gethash id *container*))
      (when delete-id-p
        (remhash id *container*)))
    data))

(defun generate-timestamp ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            yr mon day hr min sec) ))

(defparameter *current-id* 0
  "each time a new widget gets added it will increment the *curren-id*
  and take the new value as its own id.")

(defparameter *id-mapper*
  (make-hash-table :test 'equalp)
  "hash-table mapping generated widget id's to Information. For
  example: Each element in list-of-shares has a hidden id column. If a
  item gets klicked you can get Information like the file owner or the
  filesize by looking up the id inside *id-mapper*.")

(defparameter *style-sheet*
  "QTreeView {
     alternate-background-color: #eeeeef;
     background-color: #ffffff;
   }

  QTreeView::branch:has-children:!has-siblings:closed,
  QTreeView::branch:closed:has-children:has-siblings {
    border-image: none;
    image: url(./res/folder-closed.png);
  }

  QTreeView::branch:open:has-children:!has-siblings,
  QTreeView::branch:open:has-children:has-siblings  {
    border-image: none;
    image: url(./res/folder-open.png);
  }")

(define-widget main-window (QMainWindow) ())

(define-menu (main-window File)
  (:item ("Run" (ctrl r))
         (lodds.subsystem:start (lodds:get-subsystem :event-queue))
         (lodds.subsystem:start (lodds:get-subsystem :tasker))
         (lodds.subsystem:start (lodds:get-subsystem :listener))
         (lodds.subsystem:start (lodds:get-subsystem :advertiser))
         (lodds.subsystem:start (lodds:get-subsystem :handler)))
  (:item ("Stop" (ctrl s))
         (lodds.subsystem:stop (lodds:get-subsystem :tasker))
         (lodds.subsystem:stop (lodds:get-subsystem :listener))
         (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
         (lodds.subsystem:stop (lodds:get-subsystem :handler)))
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

(define-subwidget (main-window log) (q+:make-qtreewidget main-window)
  (q+:set-column-count log 4)
  (q+:set-header-labels log (list "Time" "Event" "Message" ""))
  (q+:set-alternating-row-colors log t)

  (let ((header (q+:header log)))
    (q+:set-stretch-last-section header nil)
    (q+:set-resize-mode header +log-time+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +log-event+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +log-message+ (q+:qheaderview.stretch))
    (q+:set-resize-mode header +log-count+ (q+:qheaderview.resize-to-contents))))

(defun update-ignored-log (checked-or-unchecked event-type)
  (case checked-or-unchecked
    (0 (push event-type *ignored-log-events*))
    (2 (setf *ignored-log-events*
             (remove event-type *ignored-log-events*)))))

(defmacro gen-log-checkbox (event-type widget-name title slot-name)
  `(progn
     (define-subwidget (main-window ,widget-name) (q+:make-qcheckbox ,title main-window)
       (q+:set-check-state ,widget-name
                           (if (find ,event-type *ignored-log-events*)
                               (q+:qt.unchecked)
                               (q+:qt.checked))))
     (define-slot (main-window ,slot-name) ((new-state int))
       (declare (connected ,widget-name (state-changed int)))
       (update-ignored-log new-state ,event-type))))

(gen-log-checkbox :advertiser log-check-advertiser "Advertiser" log-check-advertiser-changed)
(gen-log-checkbox :listener log-check-listener "Listener" log-check-listener-changed)
(gen-log-checkbox :info log-check-info "Info" log-check-info-changed)
(gen-log-checkbox :client-added log-check-client-added "Client Added" log-check-client-added-changed)
(gen-log-checkbox :client-removed log-check-client-removed "Client Removed" log-check-client-removed-changed)
(gen-log-checkbox :client-updated log-check-client-updated "Client Updated" log-check-client-updated-changed)
(gen-log-checkbox :debug log-check-debug "Debug" log-check-debug-changed)
(gen-log-checkbox :watcher log-check-watcher "Watcher" log-check-watcher-changed)
(gen-log-checkbox :tasker log-check-watcher "Tasker" log-check-tasker-changed)
(gen-log-checkbox :handler log-check-handler "Handler" log-check-handler-changed)
(gen-log-checkbox :list-update log-check-list-update "List Update" log-check-list-update-changed)

(define-subwidget (main-window log-checkboxes-widget) (q+:make-qscrollarea main-window)
  (let ((container (q+:make-qgroupbox "Log Settings" main-window))
        (layout (q+:make-qvboxlayout main-window)))
    (q+:set-layout container layout)
    (q+:add-widget layout log-check-advertiser)
    (q+:add-widget layout log-check-listener)
    (q+:add-widget layout log-check-info)
    (q+:add-widget layout log-check-client-added)
    (q+:add-widget layout log-check-client-removed)
    (q+:add-widget layout log-check-client-updated)
    (q+:add-widget layout log-check-debug)
    (q+:add-widget layout log-check-watcher)
    (q+:add-widget layout log-check-watcher)
    (q+:add-widget layout log-check-handler)
    (q+:add-widget layout log-check-list-update)
    (q+:set-widget log-checkboxes-widget container)))

(define-subwidget (main-window download-file) (q+:make-qlineedit main-window))
(define-subwidget (main-window download-checksum) (q+:make-qlabel main-window))
(define-subwidget (main-window download-user-selection) (q+:make-qcombobox main-window)
  (q+:add-item download-user-selection "Any"))

(define-subwidget (main-window download) (q+:make-qwidget main-window)
  (let* ((layout (q+:make-qgridlayout main-window))
         (folder-edit (q+:make-qlineedit main-window))
         (folder-completer (q+:make-qcompleter main-window))
         (download-button (q+:make-qpushbutton "Download" main-window))
         (folder-dir-model (q+:make-qdirmodel folder-completer)))
    (q+:set-layout download layout)
    ;; first row - checksum
    (q+:add-widget layout (q+:make-qlabel "Checksum:" main-window) 0 0)
    (q+:add-widget layout download-checksum 0 1 1 -1)
    ;; second row - local file location
    (q+:add-widget layout (q+:make-qlabel "Download to:" main-window) 1 0)
    (q+:set-filter folder-dir-model (q+:qdir.dirs))
    (q+:set-model folder-completer folder-dir-model)
    (q+:set-completer folder-edit folder-completer)
    (q+:add-widget layout folder-edit 1 1 1 8)
    (q+:add-widget layout download-file 1 9 1 4)
    ;; third row - user selction and download button
    (q+:add-widget layout (q+:make-qlabel "User:" main-window) 2 0)
    (q+:add-widget layout download-user-selection 2 1 1 11)
    (q+:add-widget layout download-button 2 12 1 1)
    (connect download-button "pressed()"
             (lambda ()
               (let ((user (q+:current-text download-user-selection))
                     (directory (q+:text folder-edit))
                     (filename (q+:text download-file)))
                 (cond
                   ;; TODO: popup error
                   ((eql 0 (length directory))
                     (lodds.event:push-event :error (list "local directory not selected")))
                   ((eql 0 (length filename))
                     (lodds.event:push-event :error (list "no local filename given")))
                   ((not (uiop:directory-exists-p directory))
                     (lodds.event:push-event :error (list "local directory does not exist")))
                   (t (lodds:get-file (concatenate 'string
                                                   (if (char= #\/ (char directory (- (length directory) 1)))
                                                       directory
                                                       (concatenate 'string directory "/"))
                                                   filename)
                                      (q+:text download-checksum)
                                      (unless (string= user "Any")
                                        user)))))))))

(define-subwidget (main-window log-widget) (q+:make-qsplitter main-window)
  (q+:add-widget log-widget log-checkboxes-widget)
  (q+:add-widget log-widget log))

(define-subwidget (main-window user-list) (q+:make-qtreewidget main-window)
  (q+:set-column-count user-list 6)
  (q+:set-header-labels user-list (list "User" "IP" "Port" "Load" "Last Change" ""))
  (q+:hide-column user-list +user-list-id+)
  (q+:set-alternating-row-colors user-list t)

  (let ((header (q+:header user-list)))
    (q+:set-stretch-last-section header nil)
    (q+:set-resize-mode header +user-list-name+ (q+:qheaderview.stretch))
    (q+:set-resize-mode header +user-list-ip+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +user-list-port+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +user-list-load+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +user-list-last-change+ (q+:qheaderview.resize-to-contents))))

(define-subwidget (main-window directories-shared) (q+:make-qtreewidget main-window)
  (q+:set-column-count directories-shared 2)
  (q+:set-header-labels directories-shared (list "Path" "Remove"))
  (q+:set-alternating-row-colors directories-shared t)

  (let ((header (q+:header directories-shared)))
    (q+:set-stretch-last-section header nil)
    (q+:set-resize-mode header +directories-shared-path+ (q+:qheaderview.stretch))
    (q+:set-resize-mode header +directories-shared-remove-button+ (q+:qheaderview.resize-to-contents))))

(define-subwidget (main-window share-widget) (q+:make-qwidget main-window)
  (let ((layout (q+:make-qvboxlayout main-window))
        (share-directory-button (q+:make-qpushbutton "Share Directory" main-window)))
    (q+:set-layout share-widget layout)
    (connect share-directory-button "pressed()"
             (lambda ()
               (let ((dir (q+:qfiledialog-get-existing-directory)))
                 (when (> (length dir)
                          0)
                   (lodds.watcher:share-folder dir)))))
    (q+:add-widget layout share-directory-button)
    (q+:add-widget layout directories-shared)))

(define-subwidget (main-window list-of-shares) (q+:make-qtreewidget main-window)
  (connect list-of-shares "itemClicked(QTreeWidgetItem *, int)"
           (lambda (item column)
             (declare (ignore column))
             (let ((checksum (q+:text item +los-checksum+)))
               ;; TODO: needs to be removed if folder download works
               (unless (eql 0 (length checksum))
                 (q+:set-text download-file (q+:text item +los-name+))
                 (q+:set-text download-checksum checksum)
                 (loop :repeat (- (q+:count download-user-selection) 1)
                       :do (q+:remove-item download-user-selection 1))
                 (loop :for (user . rest) :in (lodds:get-file-info checksum)
                       :do (q+:add-item download-user-selection user))))))
  (q+:set-column-count list-of-shares 4)
  (q+:set-uniform-row-heights list-of-shares t)
  (q+:set-header-labels list-of-shares (list "Name" "Size" "Checksum" "ID"))
  (q+:hide-column list-of-shares +los-id+)
  (q+:set-alternating-row-colors list-of-shares t)
  (q+:set-animated list-of-shares t)
  (q+:set-items-expandable list-of-shares t)
  (q+:set-expands-on-double-click list-of-shares t)

  (let ((header (q+:header list-of-shares)))
    (q+:set-stretch-last-section header nil)
    (q+:set-resize-mode header +los-name+ (q+:qheaderview.stretch))
    (q+:set-resize-mode header +los-size+ (q+:qheaderview.resize-to-contents))
    (q+:set-resize-mode header +los-checksum+ (q+:qheaderview.resize-to-contents)))

  (setf (q+:window-title main-window) "LODDS")
  (q+:set-window-icon main-window (q+:make-qicon "./res/lodds.png"))
  (q+:resize main-window 800 450)
  (q+:set-style-sheet main-window *style-sheet*)

  (let ((settings-dock (q+:make-qdockwidget "Settings" main-window))
        (log-dock (q+:make-qdockwidget "Log" main-window))
        (user-list-dock (q+:make-qdockwidget "User List" main-window))
        (directories-shared-dock (q+:make-qdockwidget "Directories Shared" main-window))
        (download-dock (q+:make-qdockwidget "Download File" main-window)))

    ;; download-file dock
    (q+:set-widget download-dock download)
    (q+:add-dock-widget main-window (q+:qt.left-dock-widget-area) download-dock)

    ;; settings-dock
    (let* ((dock-content (q+:make-qwidget))
           (dock-layout (q+:make-qformlayout dock-content)))
      (q+:set-maximum-height dock-content 48)
      (q+:add-row dock-layout "Interface:" interfaces)
      (q+:set-widget settings-dock dock-content))
    (q+:add-dock-widget main-window (q+:qt.right-dock-widget-area) settings-dock)

    ;; user-dock
    (q+:set-widget user-list-dock user-list)
    (q+:add-dock-widget main-window (q+:qt.left-dock-widget-area) user-list-dock)

    ;; directories-shared-dock
    (q+:set-widget directories-shared-dock share-widget)
    (q+:add-dock-widget main-window (q+:qt.right-dock-widget-area) directories-shared-dock)

    ;; log-dock
    (q+:set-widget log-dock log-widget)
    (q+:add-dock-widget main-window (q+:qt.bottom-dock-widget-area) log-dock)

    ;; add view-toggle to View MenuBar Item
    (let* ((menu-bar (q+:menu-bar main-window))
           (menu (q+:add-menu menu-bar "View")))
      (q+:add-action menu (q+:toggle-view-action log-dock))
      (q+:add-action menu (q+:toggle-view-action settings-dock))
      (q+:add-action menu (q+:toggle-view-action user-list-dock))
      (q+:add-action menu (q+:toggle-view-action directories-shared-dock))
      (q+:add-action menu (q+:toggle-view-action download-dock))))

  (setf (q+:central-widget main-window) list-of-shares))

(define-signal (main-window update-entries) (string string))
(define-signal (main-window remove-entry) (string))
(define-signal (main-window dump-table) ())
(define-signal (main-window add-log-msg) (string string))
(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window fix-menubar-order) ())
(define-signal (main-window add-user) (string string string))
(define-signal (main-window remove-user) (string))
(define-signal (main-window update-user) (string string string))
(define-signal (main-window add-directory) (string))
(define-signal (main-window remove-directory) (string))

(define-slot (main-window interfaces) ((selected-item string))
  (declare (connected interfaces (current-index-changed string)))
  (lodds:switch-interface selected-item))

(defun add-node (path size checksum get-parent-fn &optional
                                                    (child-count 0)
                                                    (get-child-fn nil)
                                                    (root-p nil))
  "loop over childs with get-child-fn and check if current entry (car
  path) is already present, if so call add-node on that entry
  recursivly, if not add a new entry and then call add-node with the
  new entry. If add-node returns t, the entry was added, otherwise nil
  is returned. The return is used to check if size on parents needs to
  be increased."
  (loop :for i :from 0 :below child-count
        :do (let ((element (funcall get-child-fn i)))
              ;; check if node matches
              (when (string= (car path) (q+:text element +los-name+))
                (unless (cdr path)
                  ;; if there is no path left but we have a
                  ;; matching node, it means that the node
                  ;; we tried to add already exists -> we
                  ;; return nil to indicate an error
                  (return-from add-node nil))
                ;; if add-node was successfull, update size and
                ;; return t
                (when (add-node (cdr path) size checksum
                                (lambda () element)
                                (q+:child-count element)
                                (lambda (place) (q+:child element place)))
                  (q+:set-text element
                               +los-size+
                               (lodds.core:format-size
                                (let* ((id (q+:text element +los-id+))
                                       (old-size (gethash id *id-mapper*)))
                                  (setf (gethash id *id-mapper*)
                                        (+ old-size size)))))
                  (return-from add-node t))
                ;; if add-child-node was not successfull, just return
                ;; nil
                (return-from add-node nil))))
  ;; when we get down here it means there was no matching
  ;; node, so lets add a new one.
  (let ((new-entry (q+:make-qtreewidgetitem (funcall get-parent-fn))))
    (let ((entry-id (prin1-to-string (incf *current-id*))))
      (q+:set-text new-entry +los-id+ entry-id)
      (setf (gethash entry-id *id-mapper*) size))
    (q+:set-text new-entry +los-name+ (car path))
    (q+:set-text new-entry +los-size+ (lodds.core:format-size size))
    (q+:set-text-alignment new-entry +los-size+ (q+:qt.align-right))
    ;; only add checksums on files, not on folders
    (unless (cdr path)
      (q+:set-text new-entry +los-checksum+ checksum))
    ;; only if not root
    (unless root-p
      (q+:add-child (funcall get-parent-fn) new-entry))
    ;; if there is path left, add those under the new node. If there
    ;; is no path left it means we reached the last node, so return t
    ;; as we successfully added it. We dont have to update size here,
    ;; since its a new node and it will match the elements size.
    (if (cdr path)
        (add-node (cdr path) size checksum (lambda () new-entry))
        t)))

(defun cleanup-node (node)
  "calls finalize on node and all its children to cleanup memory."
  (loop :while (> (q+:child-count node) 0)
        :do (cleanup-node (q+:child node 0)))
  (remhash (q+:text node +los-id+) *id-mapper*)
  (finalize node))

(defun remove-node (path child-count get-child-fn remove-child-fn)
  "loops over children with get-child-fn, and if path matches
  remove-node is recursivly called on the matching node. Remove node
  will return a Size if a node was successfull removed. This is used to
  updated sizes on parents."
  (loop :for i :from 0 :below child-count
        :do (let ((element (funcall get-child-fn i)))
              ;; check if node matches
              (when (string= (car path) (q+:text element +los-name+))
                ;; check if we have a path left and need to call
                ;; remove-node recursivly
                (if (cdr path)
                    (let ((size (remove-node (cdr path)
                                             (q+:child-count element)
                                             (lambda (place)
                                               (q+:child element place))
                                             (lambda (place)
                                               (let* ((removed-item (q+:take-child element place))
                                                      (size (gethash (q+:text removed-item +los-id+)
                                                                     *id-mapper*)))
                                                 (cleanup-node removed-item)
                                                 size)))))
                      ;; remove-node will return the size of the
                      ;; removed element, if successfull.
                      (when size
                        ;; If element has no childs left (if the
                        ;; recursive call removed the last child)
                        ;; remove element too. If there are childs
                        ;; left, just update the size.
                        (if (eql 0 (q+:child-count element))
                            (funcall remove-child-fn i)
                            (q+:set-text element
                                         +los-size+
                                         (lodds.core:format-size
                                          (let* ((id (q+:text element +los-id+))
                                                 (old-size (gethash id *id-mapper*)))
                                            (setf (gethash id *id-mapper*)
                                                  (- old-size size))))))
                        ;; return size here, since recursive remove
                        ;; was successfull
                        (return-from remove-node size)))
                    ;; return size (will be returned by
                    ;; remove-child-fn) here, since remove was
                    ;; successfull
                    (return-from remove-node (funcall remove-child-fn i)))))
        ;; return nil (failed) if loop went through and we did not
        ;; find the specified node
        :finally (return nil)))

(define-slot (main-window update-entries) ((id string)
                                           (name string))
  (declare (connected main-window (update-entries string
                                                  string)))
  (q+:set-updates-enabled list-of-shares nil)
  (loop :for (type checksum size path) :in (container-get id)
        :do (let ((combined-path (concatenate 'string name path)))
              (if (eql type :add)
                  (add-node (cl-strings:split combined-path #\/)
                            size
                            checksum
                            (lambda () list-of-shares)
                            (q+:top-level-item-count list-of-shares)
                            (lambda (place) (q+:top-level-item list-of-shares place))
                            t)
                  (remove-node (cl-strings:split combined-path #\/)
                               (q+:top-level-item-count list-of-shares)
                               (lambda (place) (q+:top-level-item list-of-shares place))
                               (lambda (place) (cleanup-node (q+:take-top-level-item list-of-shares place)))))))
  (q+:set-updates-enabled list-of-shares t))

(define-slot (main-window remove-entry) ((path string))
  (declare (connected main-window (remove-entry string)))
  (remove-node (cl-strings:split path #\/)
               (q+:top-level-item-count list-of-shares)
               (lambda (place) (q+:top-level-item list-of-shares place))
               (lambda (place) (cleanup-node (q+:take-top-level-item list-of-shares place)))))

(define-slot (main-window add-log-msg) ((event string)
                                        (msg string))
  (declare (connected main-window (add-log-msg string
                                               string)))
  (let* ((items (q+:top-level-item-count log))
         (last-item (if (> items 0)
                        (q+:top-level-item log (- items 1))
                        nil)))
    (if (and last-item
             (string= (q+:text last-item +log-event+)
                      event)
             (string= (q+:text last-item +log-message+)
                      msg))

        (progn
          (q+:set-text last-item +log-time+ (generate-timestamp))
          (q+:set-text last-item
                       +log-count+
                       (prin1-to-string
                        (let ((current (q+:text last-item +log-count+)))
                          (if (string= current "")
                              2
                              (+ 1 (parse-integer current)))))))
        (let ((new-entry (q+:make-qtreewidgetitem log)))
          (q+:set-text new-entry +log-time+ (generate-timestamp))
          (q+:set-text new-entry +log-event+ event)
          (q+:set-text new-entry +log-message+ msg)
          (q+:set-text new-entry +log-count+ "")
          (q+:set-text-alignment new-entry +log-count+ (q+:qt.align-right))
          (let* ((scrollbar (q+:vertical-scroll-bar log))
                 (position (q+:value scrollbar)))
            (loop :while (> (q+:top-level-item-count log) +log-message-maximum+)
                  :do (progn (finalize (q+:take-top-level-item log 0))
                             (q+:set-value scrollbar (- position 1)))))
          (let ((visual-rect (if last-item
                                 (q+:visual-item-rect log last-item)
                                 nil)))
            (when (and visual-rect
                       (< (- (q+:bottom visual-rect)
                             (q+:height (q+:viewport log)))
                          (q+:height visual-rect)))
              (q+:scroll-to-item log new-entry)))))))

(defun dump-item (item &optional (depth 0))
  "dumps given item, and all its childs, if it has any. Just for
  debugging"
  (format t "ITEM: ~a~a~%"
          (make-string depth :initial-element #\ )
          (q+:text item +los-name+))
  (loop :for i :from 0 :below (q+:child-count item)
        :do (dump-item (q+:child item i)
                       (+ depth 1))))

(define-slot (main-window dump-table) ()
  (declare (connected main-window (dump-table)))
  (loop :for i :from 0 :below (q+:top-level-item-count list-of-shares)
        :do (dump-item (q+:top-level-item list-of-shares i))))

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

(define-slot (main-window add-user) ((user string)
                                     (load string)
                                     (last-change string))
  (declare (connected main-window (add-user string
                                            string
                                            string)))
  (let ((new-entry (q+:make-qtreewidgetitem user-list)))
    (let ((entry-id (concatenate 'string "user:" user)))
      (q+:set-text new-entry +los-id+ entry-id)
      (setf (gethash entry-id *id-mapper*)
            new-entry)
      (lodds.core:split-user-identifier (name ip port) user
        (q+:set-text new-entry +user-list-name+ name)
        (q+:set-text new-entry +user-list-ip+ ip)
        (q+:set-text new-entry +user-list-port+ port))
      (q+:set-text new-entry +user-list-load+ (lodds.core:format-size (parse-integer load)))
      (q+:set-text new-entry +user-list-last-change+ last-change)
      (q+:set-text new-entry +user-list-id+ entry-id)
      (q+:set-text-alignment new-entry +user-list-load+ (q+:qt.align-right)))))

(define-slot (main-window remove-user) ((user string))
  (declare (connected main-window (remove-user string)))
  (lodds.core:split-user-identifier (name ip port) user
    (loop :for i :from 0 :to (q+:top-level-item-count user-list)
          :do (let ((child (q+:top-level-item user-list i)))
                (when (and (string= name (q+:text child +user-list-name+))
                           (string= ip (q+:text child +user-list-ip+))
                           (string= port (q+:text child +user-list-port+)))
                  (remhash (q+:text child +user-list-id+)
                           *id-mapper*)
                  (q+:take-top-level-item user-list i)
                  (return))))))

(define-slot (main-window add-directory) ((path string))
  (declare (connected main-window (add-directory string)))
  (let* ((new-entry (q+:make-qtreewidgetitem directories-shared))
         (remove-button (q+:make-qpushbutton "Del" main-window)))
    (q+:set-text new-entry +directories-shared-path+ path)
    (connect remove-button "pressed()"
             (lambda ()
               (lodds.watcher:unshare-folder path)))
    (q+:set-item-widget directories-shared
                        new-entry
                        +directories-shared-remove-button+
                        remove-button)))

(define-slot (main-window remove-directory) ((path string))
  (declare (connected main-window (remove-directory string)))
  (loop :for i :from 0 :to (q+:top-level-item-count directories-shared)
        :do (let ((child (q+:top-level-item directories-shared i)))
              (when (string= path (q+:text child +directories-shared-path+))
                (q+:take-top-level-item directories-shared i)
                (return)))))

(define-slot (main-window update-user) ((user string)
                                        (load string)
                                        (last-change string))
  (declare (connected main-window (update-user string
                                               string
                                               string)))
  (let ((entry (gethash (concatenate 'string "user:" user)
                        *id-mapper*)))
    (when entry
      (q+:set-text entry
                   +user-list-load+
                   (lodds.core:format-size (parse-integer load)))
      (q+:set-text entry
                   +user-list-last-change+
                   last-change))))

(defun cb-list-update (main-window event)
  "callback which will be called on a :list-update event"
  (destructuring-bind (name type timestamp changes) event
    (declare (ignore timestamp))
    (when (eql type :all)
      (signal! main-window (remove-entry string) name))
    (signal! main-window (update-entries string string)
             (container-put changes)
             name)))

(defun cb-client-removed (main-window client-name)
  "callback which will get called if a client was removed"
  (signal! main-window (remove-entry string) client-name)
  (signal! main-window (remove-user string) client-name))

(defun cb-client-added (main-window data)
  "callback which will get called if a client was removed"
  (destructuring-bind (name load last-change) data
    (signal! main-window
             (add-user string string string)
             name
             (prin1-to-string load)
             (prin1-to-string last-change))))

(defun cb-client-updated (main-window data)
  "callback which will get called if a client was removed"
  (destructuring-bind (name load last-change) data
    (signal! main-window
             (update-user string string string)
             name
             (prin1-to-string load)
             (prin1-to-string last-change))))

(defun cb-log-messages (main-window event)
  (let ((event-type (first event))
        (event-msg (cdr event)))
    (unless (find event-type *ignored-log-events*)
      (signal! main-window
               (add-log-msg string string)
               (format nil "~a" event-type)
               (case event-type
                 (:list-update
                  (destructuring-bind (name type ts changes)
                      event-msg
                    (let ((adds 0)
                          (dels 0))
                      (loop :for (type . rest) :in changes
                            :if (eql type :add)
                            :do (incf adds)
                            :else
                            :do (incf dels))
                      (format nil "~a ~a ~a adds: ~a dels: ~a"
                              name type ts adds dels))))
                 (:client-updated
                  (destructuring-bind (name load last-change)
                      event-msg
                    (format nil "~a ~a ~a"
                            name
                            (lodds.core:format-size load)
                            last-change)))
                 (t (format nil "~{~a~^ ~}" event-msg)))))))

(defun init-gui (window)
  ;; add callbacks
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (cb-list-update window (cdr event)))
                            :event-type :list-update)
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (signal! window
                                       (add-directory string)
                                       (cadr event)))
                            :event-type :shared-directory)
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (signal! window
                                       (remove-directory string)
                                       (cadr event)))
                            :event-type :unshared-directory)
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (cb-client-removed window (second event)))
                            :event-type :client-removed)
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (cb-log-messages window event)))
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (cb-client-added window (cdr event)))
                            :event-type :client-added)
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (cb-client-updated window (cdr event)))
                            :event-type :client-updated)
  ;; reset id
  (setf *current-id* 0)
  ;; reorder menubar items
  (signal! window (fix-menubar-order))
  ;; add known users and their shared files
  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add user
              (signal! window
                       (add-user string string string)
                       user
                       (prin1-to-string (lodds:c-load user-info))
                       (prin1-to-string (lodds:c-last-change user-info)))
              ;; add all files from user
              (let ((changes nil))
                (maphash (lambda (filename file-info)
                           (destructuring-bind (checksum size) file-info
                             (push (list :add checksum size filename) changes)))
                         (lodds:c-file-table-name user-info))
                (signal! window (update-entries string string)
                         (container-put (reverse changes))
                         user))))
  (loop :for dir :in (lodds.watcher:get-shared-folders)
        :do (signal! window (add-directory string) dir)))

(defun cleanup ()
  ;; remove all attached callbacks
  (lodds.event:remove-callback :gui
                               :event-type :list-update)
  (lodds.event:remove-callback :gui
                               :event-type :client-removed)
  (lodds.event:remove-callback :gui
                               :event-type :shared-directory)
  (lodds.event:remove-callback :gui
                               :event-type :unshared-directory)
  (lodds.event:remove-callback :gui)
  (lodds.event:remove-callback :gui
                               :event-type :client-added)
  (lodds.event:remove-callback :gui
                               :event-type :client-updated))

(defun main ()
  (let ((lodds-server lodds:*server*))
    ;; TODO: thats not supposed to be in a CALL-IN-MAIN-THREAD
    (trivial-main-thread:call-in-main-thread
     (lambda ()
       (lodds:with-server lodds-server
         (with-main-window (window (make-instance 'main-window))
           (init-gui window))
         (cleanup))))))
