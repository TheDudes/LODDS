#|

Contains Shares widget related stuff, The Shares widget is the
QTreeWidget in the middle, which displays all shared files

|#

(in-package #:lodds-qt)
(in-readtable :qtools)

;; shares columns
(defvar +shares-name+ 0)
(defvar +shares-size+ 1)
(defvar +shares-id+ 2)

(define-widget shares (QTreeWidget)
  ((main-window :initarg :main-window
                :initform nil)
   (changes :accessor changes
            :initform (list)
            :type list
            :documentation "contains new changes which need to be
            added to the widget. This slot is used to transfer changes
            from other threads over to the main thread. The method
            ADD-CHANGE will add changes and then signal UPDATE-ENTRIES
            which will then update the widget inside the main
            thread. To safetly retrieve a change use GET-CHANGE.")
   (changes-lock :accessor changes-lock
                 :initform (bt:make-lock)
                 :documentation "Lock to not nconc/pop two changes at
                 the same time. Lock is used by ADD-CHANGE and
                 GET-CHANGE.")
   (entries :accessor entries
            :initform (make-hash-table :test 'equal)
            :type hashtable
            :documentation "hash-table mapping generated widget id's
            to Information. Each element in shares has a hidden id
            column. If a item gets klicked you can get Information
            like the file owner or the filesize by looking up the id
            inside entries. Entries contains SHARES-ENTRY-DIR
            SHARES-ENTRY-FILE")))

(defmethod add-change ((shares shares) changes name)
  (bt:with-lock-held ((changes-lock shares))
    (if (null (changes shares))
        (setf (changes shares) (list changes))
        (nconc (changes shares) (list changes))))
  (signal! shares (update-entries string)
           name))

(defmethod get-change ((shares shares))
  (let ((change nil))
    (bt:with-lock-held ((changes-lock shares))
      (setf change (pop (changes shares))))
    change))

(defmethod get-selected-file ((shares shares) selected-item)
  "returns a list with info about the selected item. This list will be
  used by the download widget to set its fields.
  (get-selected-file shares selected-item)
  => (:file (\"21f2a2f...\"
             \"test.txt\"
             1789213
            (\"pete@192...\" \"steve@192...\")))
  (get-selected-file shares selected-item)
  => (:dir (\"/some/dir/somewhere/\"
            \"somewhere/\"
            \"pete@192.168.2.101:43210\"
            92421312
            17))"
  (let ((info (gethash (q+:text selected-item +shares-id+) (entries shares))))
    (if (eql (type-of info)
             'shares-entry-dir)
        (with-slots (user name path size items) info
          (list :dir
                (list path
                      name
                      user
                      size
                      items)))
        ;; file was clicked
        (with-slots (name checksum size) info
          (list :file
                (list checksum
                      name
                      size
                      (loop :for (user . rest)
                            :in (lodds:get-file-info checksum)
                            :collect user)))))))

(let ((current-id 0)
      (id-lock (bt:make-lock "id-lock")))
  (defun gen-id ()
    "Will return a string with a number which is increased by 1
    everytime gen-id is called"
    (bt:with-lock-held (id-lock)
      (prin1-to-string (incf current-id)))))

(defclass shares-entry ()
  ((shares :reader shares-entry-shares
           :initform (error "Please specify shares which this entry
           belongs to")
           :initarg :shares
           :documentation "Contains shares widget this shares-entry
           belongs to.")
   (id :reader shares-entry-id
       :initform (gen-id)
       :type string
       :documentation "Id which identifies a shares-entry
       object, Use id to retrieve shares-entry objects from
       entries hashtable on shares widget.")
   (is-root :reader shares-entry-is-root
            :initform nil
            :initarg :is-root
            :documentation "Flag which is t on the invisible root
            items")
   (widget :reader shares-entry-widget
           :initform (error "Please specify a widget")
           :initarg :widget
           :documentation "A qt widget corresponding to
           the given id.")
   (name :accessor shares-entry-name
         :initform (error "Please specify entry name")
         :initarg :name
         :type string
         :documentation "Name of entry")
   (user :accessor shares-entry-user
         :initform (error "Please specify a user")
         :initarg :user
         :type string
         :documentation "User which shared the given entry")
   (size :accessor shares-entry-size
         :initform (error "Please specify a widget")
         :initarg :size
         :type bignum
         :documentation "Size of the given share in bytes")
   (path :accessor shares-entry-path
         :initform (error "Please specify the entry path")
         :initarg :path
         :type string
         :documentation "Path which describes the entry")))

(defclass shares-entry-dir (shares-entry)
  ((items :accessor shares-entry-items
          :initform 0
          :type bignum
          :documentation "Amount of Children the Directory has")))

(defclass shares-entry-file (shares-entry)
  ((checksum :accessor shares-entry-checksum
             :initform (error "Please specify a checksum")
             :initarg :checksum
             :type string
             :documentation "File entries checksum")))

(defun set-column-background (entry column color)
  (with-finalizing* ((qcolor (q+:make-qcolor color))
                     (qbrush (q+:make-qbrush qcolor)))
    (q+:set-background entry column qbrush)))

(defgeneric update-entry-display (shares-entry)
  (:documentation "Updates entries displayed size and tooltip")
  (:method :around ((entry shares-entry))
    (unless (slot-value entry 'is-root)
      (call-next-method)))
  (:method ((entry shares-entry))
    (with-slots (widget size user) entry
      (when (lodds.config:get-value :show-background-color-on-size)
        (set-column-background widget
                               +shares-size+
                               (lodds.core:get-size-color size)))
      (qdoto widget
             (q+:set-text +shares-size+
                          (lodds.core:format-size size))
             (q+:set-tool-tip +shares-size+
                              (format nil "~:d bytes" size))
             (q+:set-tool-tip +shares-name+
                              (format nil "Double click, right click ~
                              or press Enter to download.")))))
  (:method ((entry shares-entry-dir))
    (call-next-method)
    (with-slots (items widget path user size) entry
      (if (string= "/" path)
          (lodds.core:split-user-identifier (name ip port) user
            (q+:set-tool-tip widget +shares-name+
                             (format nil "Ip: ~a~%Port: ~a~%Folder Shared: ~a"
                                     ip port items)))
          (q+:set-status-tip widget +shares-name+
                             (format nil "Items: ~a" items)))))
  (:method ((entry shares-entry-file))
    (call-next-method)
    (with-slots (widget checksum) entry
      (q+:set-status-tip widget +shares-name+
                         (format nil "Checksum: ~a" checksum)))))

(defmethod initialize-instance :after ((entry shares-entry) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (name size widget id shares user is-root) entry
    (setf (gethash id (entries shares)) entry)
    (let ((font (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)))
      (setf (q+:style-hint font) (q+:qfont.type-writer))
      (when (and (lodds.config:get-value :show-background-color-on-size)
                 (not is-root))
        (set-column-background widget +shares-size+
                               (lodds.core:get-size-color size)))
      (qdoto widget
             (q+:set-flags (qt:enum-or (q+:qt.item-is-selectable)
                                       (q+:qt.item-is-enabled)))
             (q+:set-font +shares-name+ font)
             (q+:set-font +shares-size+ font)
             (q+:set-text-alignment +shares-size+ (q+:qt.align-right))
             (q+:set-text +shares-name+ name)
             (q+:set-text +shares-size+ (lodds.core:format-size size))
             (q+:set-text +shares-id+ id)))))

(defmethod initialize-instance :after ((entry shares-entry-dir) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (items widget path name) entry
    (when (string= "/" path)
      (lodds.core:split-user-identifier (user ip port) name
        (q+:set-text widget +shares-name+ user)))
    (update-entry-display entry)))

(defmethod initialize-instance :after ((entry shares-entry-file) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (checksum widget) entry
    (update-entry-display entry)))

(define-signal (shares update-entries) (string))
(define-signal (shares remove-entry) (string))
(define-signal (shares dump-table) ())

(defparameter *new-checksum* nil)
(defparameter *new-size* nil)
(defparameter *new-user* nil)
(defmethod add-node ((shares shares) path path-left parent)
  "Loops over all childs of parent and checks if (car path) is already
  present. If so add-node is recursivly called on that element with
  the (cdr) of path. If not a new child will be added and add-node
  will be recursivly called on that (with (cdr path)). When the last
  element of path is reached a new element is added and t is returned,
  else nil is returned (when the node already exists)"
  (do-childs (element i parent)
    (let ((entry (gethash (q+:text element +shares-id+) (entries shares))))
      ;; check if node matches, if it does not continue
      (when (string= (car path-left)
                     (shares-entry-name entry))
        (unless (cdr path-left)
          ;; if there is no path left but we have a
          ;; matching node, it means that the node
          ;; we tried to add already exists -> we
          ;; return nil to indicate an error
          (return-from add-node nil))
        ;; but if there is path left, add the node on the matching
        ;; child node.
        (if (add-node shares
                      (concatenate 'string path (car path-left))
                      (cdr path-left)
                      element)
            ;; if add-node was successfull, update size and return. We
            ;; dont have to update items, since if the item was added
            ;; directly below the current node, items was already
            ;; updated (see below /incf (shares-entry-items ...))
            (progn
              (incf (shares-entry-size entry) *new-size*)
              (update-entry-display entry)
              (return-from add-node t))
            ;; if add-child-node was not successfull, just go up the
            ;; stack and return nil
            (return-from add-node nil)))))
  ;; when we get down here (after the do-childs) it means there was no
  ;; matching node found with the current path, so lets add a new
  ;; node.
  (let ((new-entry (q+:make-qtreewidgetitem parent))
        (current-path (concatenate 'string
                                   path
                                   (car path-left))))
    ;; if we got path left -> dir, if not -> leaf -> file
    (if (cdr path-left)
        ;; only add items on directories
        (make-instance 'shares-entry-dir
                       :shares shares
                       :name (car path-left)
                       :path (subseq current-path
                                     (length *new-user*))
                       :size *new-size*
                       :user *new-user*
                       :widget new-entry)
        ;; only add checksums on files, not on folders
        (make-instance 'shares-entry-file
                       :shares shares
                       :name (car path-left)
                       :checksum *new-checksum*
                       :path (subseq current-path
                                     (length *new-user*))
                       :size *new-size*
                       :user *new-user*
                       :widget new-entry))
    ;; The new node is set, now update the currents parent item count
    ;; (increase it by 1 since we added exactly 1 element)
    (let ((entry (gethash (q+:text parent +shares-id+)
                          (entries shares))))
      (incf (shares-entry-items entry))
      (update-entry-display entry))
    ;; Now add the new node to the tree
    (q+:add-child parent new-entry)
    ;; if there is path left, add those under the new node. If there
    ;; is no path left it means we reached the last node (leaf), so
    ;; we can return t as we successfully added it. We dont have to
    ;; update size here, since its a new node and it will match the
    ;; elements size which is known up the stack (add-node returns
    ;; there with t and they will update the node they are standing
    ;; on)
    (if (cdr path-left)
        (add-node shares
                  (concatenate 'string path (car path-left))
                  (cdr path-left)
                  new-entry)
        t)))

(defmethod get-total-shares-size ((shares shares))
  (reduce #'+
          (do-childs (element i (q+:invisible-root-item shares))
            (shares-entry-size (gethash (q+:text element +shares-id+)
                                        (entries shares))))))

(defmethod cleanup-node ((shares shares) node)
  "calls finalize on node and all its children to cleanup memory."
  (loop :while (> (q+:child-count node) 0)
        :do (cleanup-node shares (q+:child node 0)))
  (remhash (q+:text node +shares-id+) (entries shares))
  (finalize node))

(defmethod remove-node ((shares shares) path parent)
  "loops over children, and if path matches remove-node is recursivly
  called on the matching node. Remove node will return a size if a
  node was successfull removed. This is used to updated sizes when
  unwinding the recursion and the lower calls return."
  ;; return nil (failed) if loop went through and we did not
  ;; find the specified node (do-childs returns nil)
  (do-childs (element i parent)
    (let ((entry (gethash (q+:text element +shares-id+)
                          (entries shares)))
          (parent-entry (gethash (q+:text parent +shares-id+)
                                 (entries shares))))
      ;; check if node matches, if it doesn't continue
      (when (string= (car path)
                     (shares-entry-name entry))
        ;; check if we have a path left and need to call
        ;; remove-node recursivly, if not just remove the
        ;; child and return
        (if (not (cdr path))
            ;; no path left, which means we reached the searched leaf
            ;; we want to return. Decrement the parents items by 1 and
            ;; return the removed elements size so the parents can
            ;; update theirs.
            (let ((size (shares-entry-size entry)))
              (decf (shares-entry-items parent-entry))
              (update-entry-display parent-entry)
              (cleanup-node shares (q+:take-child parent i))
              (return-from remove-node size))
            ;; path left, which means we did not reach the end
            ;; yet. Just recursivly call remove-node on the matching
            ;; element and check if it was successfull (returned a
            ;; size)
            (let ((size (remove-node shares
                                     (cdr path)
                                     element)))
              (if (not size)
                  ;; In case there is no size returned, just return
                  ;; nil up the recursion.
                  (return-from remove-node nil)
                  (progn
                    ;; But if we got a size check if remove-node on the
                    ;; current element removed the last remaining
                    ;; child. Because if so, we also have to remove the
                    ;; current node.
                    (if (eql 0 (q+:child-count element))
                        (progn
                          ;; decrement the parents item count by 1,
                          ;; and remove the current node from the
                          ;; parent
                          (decf (shares-entry-items parent-entry))
                          (update-entry-display parent-entry)
                          (cleanup-node shares (q+:take-child parent i)))
                        (progn
                          ;; If there are childs left, just update the
                          ;; size (decrement it by the size of the
                          ;; removed child)
                          (decf (shares-entry-size entry) size)
                          (update-entry-display entry)))
                    ;; after doing all work on the current node,
                    ;; return size and go up the recursion stack
                    (return-from remove-node size)))))))))

(define-slot (shares update-entries) ((name string))
  (declare (connected shares (update-entries string)))
  (q+:set-updates-enabled shares nil)
  (let* ((changes (get-change shares))
         (amount (length changes))
         (current 0))
    (loop :for (type checksum size path) :in changes
          :do (let ((combined-path (concatenate 'string name path)))
                (when main-window
                  (q+:show-message
                   (q+:status-bar main-window)
                   (format nil "Updating Shares (~:d/~:d) ~a"
                           amount
                           (incf current)
                           path)))
                (if (eql type :add)
                    (let* ((split-path (lodds.core:split-path combined-path))
                           (*new-checksum* checksum)
                           (*new-size* size)
                           (*new-user* (let ((user (car split-path)))
                                         (subseq user 0 (- (length user) 1)))))
                      (add-node shares
                                ""
                                split-path
                                (q+:invisible-root-item shares)))
                    (remove-node shares
                                 (lodds.core:split-path combined-path)
                                 (q+:invisible-root-item shares))))))
  (q+:set-updates-enabled shares t))

(define-slot (shares remove-entry) ((path string))
  (declare (connected shares (remove-entry string)))
  (remove-node shares
               (lodds.core:split-path path)
               (q+:invisible-root-item shares)))

(defun dump-item (item &optional (depth 0))
  "dumps given item, and all its childs, if it has any. Just for
  debugging"
  (format t "ITEM: ~a~a~%"
          (make-string depth :initial-element #\ )
          (q+:text item +shares-name+))
  (loop :for i :from 0 :below (q+:child-count item)
        :do (dump-item (q+:child item i)
                       (+ depth 1))))

(define-slot (shares dump-table) ()
  (declare (connected shares (dump-table)))
  (loop :for i :from 0 :below (q+:top-level-item-count shares)
        :do (dump-item (q+:top-level-item shares i))))

(defun make-file-info-widget (checksum name size users)
  (declare (ignore users))
  (let* ((widget (q+:make-qwidget))
         (layout (q+:make-qformlayout widget)))
    (qdoto layout
           (q+:set-form-alignment (q+:qt.align-top))
           (q+:add-row "Checksum: " (q+:make-qlabel checksum))
           (q+:add-row "Name: " (q+:make-qlabel name))
           (q+:add-row "Size: " (q+:make-qlabel (format nil "~a (~:d bytes)"
                                                        (lodds.core:format-size size)
                                                        size)))
           (q+:add-row "Users who share the given file:" (q+:make-qlabel "")))
    (loop :for (user load size filenames)
          :in (lodds:get-file-info checksum)
          :do (progn
                (q+:add-row layout user (q+:make-qlabel (car filenames)))
                (loop :for file :in (cdr filenames)
                      :do (q+:add-row layout "" (q+:make-qlabel file)))))
    widget))

(defun open-info-file-dialog (checksum name size users)
  (make-instance 'dialog
                 :title "File Info"
                 :widget (make-file-info-widget checksum
                                                name
                                                size
                                                users)))

(defun open-info-folder-dialog (fullpath dir user size files)
  (make-instance 'dialog
                 :title "Folder Info"
                 :widget
                 (let ((widget (q+:make-qwidget)))
                   (qdoto (q+:make-qformlayout widget)
                          (q+:add-row "Fullpath:" (q+:make-qlabel fullpath))
                          (q+:add-row "Dir:" (q+:make-qlabel dir))
                          (q+:add-row "User:" (q+:make-qlabel user))
                          (q+:add-row "Size:" (q+:make-qlabel
                                               (format nil "~a (~:d bytes)"
                                                       (lodds.core:format-size size)
                                                       size)))
                          (q+:add-row "Files:" (q+:make-qlabel (format nil "~:d" files))))
                   widget)))

(defmethod download ((shares shares))
  (let ((selected-items (q+:selected-items shares)))
    (case (length selected-items)
      (0 nil)
      (1 (destructuring-bind (type info)
             (get-selected-file shares (car selected-items))
           (case type
             (:file
              (apply #'open-download-file-dialog info))
             (:dir
              (apply #'open-download-folder-dialog info)))))
      (t (open-download-multiple-dialog
          (mapcar (lambda (item)
                    (get-selected-file shares item))
                  (q+:selected-items shares)))))))

(defmethod info ((shares shares))
  (let ((selected-items (q+:selected-items shares)))
    (flet ((open-info (item)
             (destructuring-bind (type info)
                 (get-selected-file shares item)
               (case type
                 (:file
                  (apply #'open-info-file-dialog info))
                 (:dir
                  (apply #'open-info-folder-dialog info))))))
      (case (length selected-items)
        (0 nil)
        (1 (open-info (car selected-items)))
        (t (let ((items (length (q+:selected-items shares))))
             (make-instance 'dialog
                            :title "Are you Sure?"
                            :text
                            (format nil "This action would open ~a info dialogs.~%~
                                        Are you sure that you want to proceed~%~
                                        (aka open ~a info dialogs)?"
                                    items
                                    items)
                            :ok-text "Yes (I love dialogs)"
                            :cancel-text "No"
                            :on-success-fn
                            (lambda (widget)
                              (declare (ignore widget))
                              (mapcar (lambda (item)
                                        (open-info item))
                                      (q+:selected-items shares))))))))))

(define-override (shares key-press-event) (ev)
  (call-next-qmethod)
  (cond
    ((or (= (q+:key ev) (q+:qt.key_enter))
         (= (q+:key ev) (q+:qt.key_return)))
     (download shares))
    ((= (q+:key ev) (q+:qt.key_i))
     (info shares))))

(define-slot (shares prepare-menu) ((pos "const QPoint &"))
  (declare (connected shares (custom-context-menu-requested "const QPoint &")))
  (let ((widget (q+:item-at shares pos)))
    (when (qobject-alive-p widget)
      (with-finalizing ((global-pos (q+:map-to-global shares pos))
                        (menu (qdoto (q+:make-qmenu)
                                     (q+:add-action "Download")
                                     (q+:add-action "Info"))))
        (let ((option (q+:exec menu global-pos)))
          (cond
            ((null-qobject-p option))
            ((string= "Download" (q+:text option))
             (download shares))
            ((string= "Info" (q+:text option))
             (info shares))))))))

(define-initializer (shares setup-widget)
  (connect shares
           "itemDoubleClicked(QTreeWidgetItem *, int)"
           (lambda (selected-item column)
             (declare (ignore column selected-item))
             (download shares)))
  (make-instance 'shares-entry-dir
                 :shares shares
                 :is-root t
                 :name ""
                 :path ""
                 :size 0
                 :user ""
                 :widget (q+:invisible-root-item shares))
  (qdoto shares
         (q+:set-mouse-tracking t)
         (q+:set-object-name "Shares")
         (q+:set-selection-behavior (q+:qabstractitemview.select-rows))
         (q+:set-selection-mode (q+:qabstractitemview.extended-selection))
         (q+:set-column-count 3)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Name" "Size" "ID"))
         (q+:hide-column +shares-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t)
         (q+:set-items-expandable t)
         (q+:set-context-menu-policy (q+:qt.custom-context-menu))
         (q+:set-expands-on-double-click nil))
  (qdoto (q+:header shares)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shares-name+ (q+:qheaderview.stretch))
         (q+:resize-section +shares-size+ (lodds.config:get-value :size-column-width))))

(define-initializer (shares setup-callbacks)
  ;; move this to list-view later
  (lodds.event:add-callback :qt-shares
                            (lambda (user)
                              (signal! shares
                                       (remove-entry string)
                                       (concatenate 'string
                                                    user
                                                    "/")))
                            :user-removed)
  (lodds.event:add-callback :qt-shares
                            (lambda (name type timestamp changes)
                              (declare (ignore timestamp))
                              (when (eql type :all)
                                (signal! shares (remove-entry string)
                                         (concatenate 'string
                                                      name
                                                      "/")))
                              (add-change shares changes name))
                            :list-update))

(define-initializer (shares setup-add-files)
  ;; add known users and their shared files
  (loop :for user :in (lodds:get-user-list)
        :do (let ((user-info (lodds:get-user-info user)))
              ;; add all files from user
              (let ((changes nil))
                (maphash (lambda (filename file-info)
                           (destructuring-bind (checksum size) file-info
                             (push (list :add checksum size filename) changes)))
                         (lodds:user-file-table-name user-info))
                (add-change shares changes user)))))

(define-finalizer (shares cleanup-callbacks)
  (lodds.event:remove-callback :qt-shares :user-removed)
  (lodds.event:remove-callback :qt-shares :list-update))
