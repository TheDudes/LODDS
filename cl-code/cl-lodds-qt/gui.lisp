(in-package #:lodds-qt)
(in-readtable :qtools)

;; list-of-shares columns
(defvar +los-name+ 0)
(defvar +los-items+ 1)
(defvar +los-size+ 2)
(defvar +los-checksum+ 3)
(defvar +los-id+ 4)

(defmethod initialize-instance :after ((entry info) &rest initargs)
  (declare (ignorable initargs))
  (setf (gethash (info-id entry) *id-mapper*) entry))

(defclass shares-entry (info)
  ((shares-entry-name :accessor shares-entry-name
                      :initform (error "Please specify entry name")
                      :initarg :name
                      :type string
                      :documentation "Name of entry")
   (shares-entry-user :accessor shares-entry-user
                      :initform (error "Please specify a user")
                      :initarg :user
                      :type string
                      :documentation "Id which identifies a id-info
                      object, Use id to retrieve id-info objects from
                      *id-mapper* table.")
   (shares-entry-size :accessor shares-entry-size
                      :initform (error "Please specify a widget")
                      :initarg :size
                      :type bignum
                      :documentation "A qt widget corresponding to the
                      given id.")
   (shares-entry-path :accessor shares-entry-path
                      :initform (error "Please specify the entry path")
                      :initarg :path
                      :type string
                      :documentation "")))

(defmethod initialize-instance :after ((entry shares-entry) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((name shares-entry-name)
                   (size shares-entry-size)
                   (widget info-widget)
                   (id info-id)) entry
    (let ((font (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)))
      (setf (q+:style-hint font) (q+:qfont.type-writer))
      (qdoto widget
             (q+:set-font +los-name+ font)
             (q+:set-font +los-items+ font)
             (q+:set-font +los-size+ font)
             (q+:set-font +los-checksum+ font)
             (q+:set-text-alignment +los-size+ (q+:qt.align-right))
             (q+:set-text-alignment +los-items+ (q+:qt.align-right))
             (q+:set-text +los-name+ name)
             (q+:set-text +los-size+ (lodds.core:format-size size))
             (q+:set-text +los-id+ id)))))

(defclass shares-entry-dir (shares-entry)
  ((shares-entry-items :accessor shares-entry-items
                       :initform 0
                       :type bignum
                       :documentation "Amount of Children the
                       Directory has")))

(defmethod initialize-instance :after ((entry shares-entry-dir) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((items shares-entry-items)
                   (widget info-widget)) entry
    (qdoto widget
           (q+:set-text +los-items+ (prin1-to-string items))
           (q+:set-text +los-checksum+ ""))))

(defclass shares-entry-file (shares-entry)
  ((shares-entry-checksum :accessor shares-entry-checksum
                          :initform (error "Please specify a checksum")
                          :initarg :checksum
                          :type string
                          :documentation "File entries checksum")))

(defmethod initialize-instance :after ((entry shares-entry-file) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((checksum shares-entry-checksum)
                   (widget info-widget)) entry
    (qdoto widget
           (q+:set-text +los-items+ "")
           (q+:set-text +los-checksum+ checksum))))

(define-widget main-window (QMainWindow) ())

(define-initializer (main-window setup-widget)
  (qdoto main-window
         (q+:set-window-title "LODDS")
         (q+:set-window-icon (q+:make-qicon "./res/lodds.png"))
         (q+:resize 800 450)
         (q+:set-style-sheet *style-sheet*)))

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

(define-subwidget (main-window download-file) (q+:make-qlineedit main-window))
(define-subwidget (main-window download-checksum) (q+:make-qlabel main-window))
(define-subwidget (main-window download-user-selection) (q+:make-qcombobox main-window)
  (q+:add-item download-user-selection "Any"))

(define-subwidget (main-window download) (q+:make-qwidget main-window)
  (let* ((layout (q+:make-qgridlayout download))
         (folder-edit (q+:make-qlineedit main-window))
         (folder-completer (q+:make-qcompleter main-window))
         (download-button (q+:make-qpushbutton "Download" main-window))
         (folder-dir-model (q+:make-qdirmodel folder-completer)))
    (q+:set-filter folder-dir-model (q+:qdir.dirs))
    (q+:set-model folder-completer folder-dir-model)
    (q+:set-completer folder-edit folder-completer)

    (qdoto layout
           ;; first row - checksum
           (q+:add-widget (q+:make-qlabel "Checksum:" main-window) 0 0)
           (q+:add-widget download-checksum 0 1 1 -1)
           ;; second row - local file location
           (q+:add-widget (q+:make-qlabel "Download to:" main-window) 1 0)
           (q+:add-widget folder-edit 1 1 1 8)
           (q+:add-widget download-file 1 9 1 4)
           ;; third row - user selction and download button
           (q+:add-widget (q+:make-qlabel "User:" main-window) 2 0)
           (q+:add-widget download-user-selection 2 1 1 11)
           (q+:add-widget download-button 2 12 1 1))

    (connect download-button "pressed()"
             (lambda ()
               (let ((user (q+:current-text download-user-selection))
                     (directory (q+:text folder-edit))
                     (filename (q+:text download-file))
                     (checksum (q+:text download-checksum)))
                 ;; add / if missing
                 (when (> (length directory) 0)
                   (setf directory
                         (if (char= #\/ (char directory (- (length directory) 1)))
                             directory
                             (concatenate 'string directory "/"))))
                 (cond
                   ;; TODO: popup error
                   ((eql 0 (length directory))
                     (lodds.event:push-event :error (list "local directory not selected")))
                   ((not (uiop:directory-exists-p directory))
                     (lodds.event:push-event :error (list "local directory does not exist")))
                   ((not (q+:is-enabled download-user-selection))
                    ;; we got a folder download
                    (lodds:get-folder checksum
                                      (subseq checksum 0 (- (length checksum)
                                                            (length filename)))
                                      directory
                                      user))
                   ;; file download
                   ((eql 0 (length filename))
                     (lodds.event:push-event :error (list "no local filename given")))
                   (t (lodds:get-file (concatenate 'string
                                                   ;; in case trailing slash does not exists, add it
                                                   directory
                                                   filename)
                                      checksum
                                      (unless (string= user "Any")
                                        user)))))))))

(define-subwidget (main-window info-log-widget) (make-instance 'info-log))
(define-subwidget (main-window user-list-widget) (make-instance 'user-list))
(define-subwidget (main-window shared-widget) (make-instance 'shared))

(define-subwidget (main-window list-of-shares) (q+:make-qtreewidget main-window)
  (connect list-of-shares "itemClicked(QTreeWidgetItem *, int)"
           (lambda (item column)
             (declare (ignore column))
             ;; remove all entries from download-user-selection
             (loop :repeat (- (q+:count download-user-selection) 1)
                   :do (q+:remove-item download-user-selection 1))
             (let ((info (gethash (q+:text item +los-id+) *id-mapper*)))
               (if (eql (type-of info)
                        'shares-entry-dir)
                   (with-accessors ((user shares-entry-user)
                                    (name shares-entry-name)
                                    (path shares-entry-path)) info
                     ;; directory was clicked
                     (qdoto download-file
                            (q+:set-text
                            ;; check if item is root (parent == NULL)
                             (if (qobject-alive-p (q+:parent item))
                                 name
                                 ""))
                            (q+:set-enabled nil))
                     (q+:set-text download-checksum path)
                     (qdoto download-user-selection
                            ;; TODO: add selected user here
                            (q+:add-item user)
                            (q+:set-current-index 1)
                            (q+:set-enabled nil)))
                   ;; file was clicked
                   (with-accessors ((name shares-entry-name)
                                    (checksum shares-entry-checksum)) info
                     (qdoto download-file
                            (q+:set-text name)
                            (q+:set-enabled t))
                     (q+:set-text download-checksum checksum)
                     (q+:set-enabled download-user-selection t)
                     (loop :for (user . rest) :in (lodds:get-file-info checksum)
                           :do (q+:add-item download-user-selection user)))))))
  (qdoto list-of-shares
         (q+:set-column-count 5)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Name" "Items" "Size" "Checksum" "ID"))
         (q+:hide-column +los-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t)
         (q+:set-items-expandable t)
         (q+:set-expands-on-double-click t))

  (qdoto (q+:header list-of-shares)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +los-name+ (q+:qheaderview.stretch))
         (q+:set-resize-mode +los-items+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +los-size+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +los-checksum+ (q+:qheaderview.resize-to-contents)))

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
           (q+:set-central-widget list-of-shares))))

(define-signal (main-window update-entries) (string string))
(define-signal (main-window remove-entry) (string))
(define-signal (main-window dump-table) ())
(define-signal (main-window reload-stylesheet) ())
(define-signal (main-window fix-menubar-order) ())

(define-slot (main-window interfaces) ((selected-item string))
  (declare (connected interfaces (current-index-changed string)))
  (lodds:switch-interface selected-item))

(defparameter *new-checksum* nil)
(defparameter *new-size* nil)
(defparameter *new-user* nil)
(defun add-node (path path-left get-parent-fn &optional
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
              (when (string= (car path-left)
                             (q+:text element +los-name+))
                (unless (cdr path-left)
                  ;; if there is no path left but we have a
                  ;; matching node, it means that the node
                  ;; we tried to add already exists -> we
                  ;; return nil to indicate an error
                  (return-from add-node nil))
                ;; if add-node was successfull, update size and
                ;; item count, then return t
                (when (add-node (concatenate 'string path (car path-left))
                                (cdr path-left)
                                (lambda () element)
                                (q+:child-count element)
                                (lambda (place) (q+:child element place)))
                  (with-accessors ((old-size shares-entry-size))
                      (gethash (q+:text element +los-id+) *id-mapper*)
                    (incf old-size *new-size*)
                    (q+:set-text element
                                 +los-size+
                                 (lodds.core:format-size
                                  old-size)))
                  (return-from add-node t))
                ;; if add-child-node was not successfull, just return
                ;; nil
                (return-from add-node nil))))
  ;; when we get down here it means there was no matching
  ;; node, so lets add a new one.
  (let ((new-entry (q+:make-qtreewidgetitem (funcall get-parent-fn)))
        (current-path (concatenate 'string
                                   path
                                   (car path-left))))
    (if (cdr path-left)
        ;; only add items on directories
        (make-instance 'shares-entry-dir
                       :name (car path-left)
                       :path (subseq current-path
                                     (length *new-user*))
                       :size *new-size*
                       :user *new-user*
                       :widget new-entry
                       :id (gen-id))
        ;; only add checksums on files, not on folders
        (make-instance 'shares-entry-file
                       :name (car path-left)
                       :checksum *new-checksum*
                       :path (subseq current-path
                                     (length *new-user*))
                       :size *new-size*
                       :user *new-user*
                       :widget new-entry
                       :id (gen-id)))
    (unless root-p
      (let ((parent (funcall get-parent-fn)))
        (q+:set-text parent
                     +los-items+
                     (prin1-to-string
                      (incf (shares-entry-items
                             (gethash
                              (q+:text parent +los-id+)
                              *id-mapper*)))))))
    ;; only if not root
    (unless root-p
      (q+:add-child (funcall get-parent-fn) new-entry))
    ;; if there is path left, add those under the new node. If there
    ;; is no path left it means we reached the last node, so return t
    ;; as we successfully added it. We dont have to update size here,
    ;; since its a new node and it will match the elements size.
    (if (cdr path-left)
        (add-node (concatenate 'string path (car path-left))
                  (cdr path-left)
                  (lambda () new-entry))
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
              (when (string= (car path)
                             (q+:text element +los-name+))
                ;; check if we have a path left and need to call
                ;; remove-node recursivly
                (if (cdr path)
                    (let ((size (remove-node (cdr path)
                                             (q+:child-count element)
                                             (lambda (place)
                                               (q+:child element place))
                                             (lambda (place)
                                               (let* ((removed-item (q+:take-child element place))
                                                      (size (shares-entry-size
                                                             (gethash (q+:text removed-item +los-id+)
                                                                      *id-mapper*))))
                                                 (q+:set-text element
                                                              +los-items+
                                                              (prin1-to-string
                                                               (incf (shares-entry-items
                                                                      (gethash
                                                                       (q+:text element +los-id+)
                                                                       *id-mapper*)))))
                                                 (cleanup-node removed-item)
                                                 size)))))
                      ;; remove-node will return the size of the
                      ;; removed element, if successfull.
                      (when size
                        ;; If element has no childs left (if the
                        ;; recursive call removed the last child)
                        ;; remove element too. If there are childs
                        ;; left, just update the size and item count
                        (if (eql 0 (q+:child-count element))
                            (funcall remove-child-fn i)
                            (with-accessors ((old-size shares-entry-size))
                                (gethash (q+:text element +los-id+) *id-mapper*)
                              (decf old-size size)
                              (q+:set-text element
                                           +los-size+
                                           (lodds.core:format-size
                                            old-size))))
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
                  (let* ((split-path (lodds.core:split-path combined-path))
                         (*new-checksum* checksum)
                         (*new-size* size)
                         (*new-user* (let ((user (car split-path)))
                                       (subseq user 0 (- (length user) 1)))))
                    (add-node ""
                              split-path
                              (lambda () list-of-shares)
                              (q+:top-level-item-count list-of-shares)
                              (lambda (place) (q+:top-level-item list-of-shares place))
                              t))
                  (remove-node (lodds.core:split-path combined-path)
                               (q+:top-level-item-count list-of-shares)
                               (lambda (place) (q+:top-level-item list-of-shares place))
                               (lambda (place) (cleanup-node (q+:take-top-level-item list-of-shares place)))))))
  (q+:set-updates-enabled list-of-shares t))

(define-slot (main-window remove-entry) ((path string))
  (declare (connected main-window (remove-entry string)))
  (remove-node (lodds.core:split-path path)
               (q+:top-level-item-count list-of-shares)
               (lambda (place) (q+:top-level-item list-of-shares place))
               (lambda (place) (cleanup-node (q+:take-top-level-item list-of-shares place)))))

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

(defun cb-list-update (main-window event)
  "callback which will be called on a :list-update event"
  (destructuring-bind (name type timestamp changes) event
    (declare (ignore timestamp))
    (when (eql type :all)
      (signal! main-window (remove-entry string)
               (concatenate 'string
                            name
                            "/")))
    (signal! main-window (update-entries string string)
             (container-put changes)
             name)))

(defun init-gui (main-window)
  ;; add callbacks
  (dolist (event+cb +events+)
    (when (second event+cb)
      (lodds.event:add-callback :gui
                                (lambda (event)
                                  (funcall (symbol-function (second event+cb))
                                           main-window
                                           (cdr event)))
                                (first event+cb))))
  ;; move this to list-view later
  (lodds.event:add-callback :gui
                            (lambda (event)
                              (signal! main-window
                                       (remove-entry string)
                                       (concatenate 'string
                                                    (second event)
                                                    "/")))
                            :client-removed)
  ;; reset id
  (setf *current-id* 0)
  ;; reorder menubar items
  (signal! main-window (fix-menubar-order))
  ;; add known users and their shared files
  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add all files from user
              (let ((changes nil))
                (maphash (lambda (filename file-info)
                           (destructuring-bind (checksum size) file-info
                             (push (list :add checksum size filename) changes)))
                         (lodds:c-file-table-name user-info))
                (signal! main-window (update-entries string string)
                         (container-put (reverse changes))
                         user)))))

(defun debug-ignore (&rest args)
  (format t "ERROR:---------------------------------------~%")
  (format t "~a~%" args)
  (format t "---------------------------------------------~%")
  (abort))

(defun main ()
  (let ((lodds-server lodds:*server*))
    ;; TODO: thats not supposed to be in a CALL-IN-MAIN-THREAD
    (tmt:with-body-in-main-thread ()
      (lodds:with-server lodds-server
        (with-main-window (window
                           (make-instance 'main-window)
                           :main-thread nil
                           :on-error #'debug-ignore)
          (init-gui window))
        (loop :for (event cb) :in +events+
              :when cb
              :do (lodds.event:remove-callback :gui event))
        (lodds.event:remove-callback :gui :client-removed)
        (setf *id-mapper* (make-hash-table :test 'equalp))))))
