(in-package #:lodds-qt)
(in-readtable :qtools)

;; shares columns
(defvar +shares-name+ 0)
(defvar +shares-size+ 1)
(defvar +shares-id+ 2)

(define-widget shares (QTreeWidget)
  ((changes :accessor changes
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
        (with-accessors ((user shares-entry-user)
                         (name shares-entry-name)
                         (fullpath shares-entry-path)
                         (size shares-entry-size)
                         (items shares-entry-items)) info
          (list :dir
                (list fullpath
                      name
                      user
                      size
                      items)))
        ;; file was clicked
        (with-accessors ((name shares-entry-name)
                         (checksum shares-entry-checksum)
                         (size shares-entry-size)) info
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

(defmethod initialize-instance :after ((entry shares-entry) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((name shares-entry-name)
                   (size shares-entry-size)
                   (widget shares-entry-widget)
                   (id shares-entry-id)
                   (shares shares-entry-shares)) entry
    (setf (gethash id (entries shares)) entry)
    (let ((font (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)))
      (setf (q+:style-hint font) (q+:qfont.type-writer))
      (qdoto widget
             (q+:set-flags (qt:enum-or (q+:qt.item-is-selectable)
                                       (q+:qt.item-is-enabled)))
             (q+:set-tool-tip +shares-size+ (format nil "~:d bytes" size))
             (q+:set-font +shares-name+ font)
             (q+:set-font +shares-size+ font)
             (q+:set-text-alignment +shares-size+ (q+:qt.align-right))
             (q+:set-text +shares-name+ name)
             (q+:set-text +shares-size+ (lodds.core:format-size size))
             (q+:set-text +shares-id+ id)))))

(defmethod initialize-instance :after ((entry shares-entry-dir) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((items shares-entry-items)
                   (widget shares-entry-widget)) entry
    (q+:set-tool-tip widget +shares-name+ (format nil "Items: ~a" items))))

(defmethod initialize-instance :after ((entry shares-entry-file) &rest initargs)
  (declare (ignorable initargs))
  (with-accessors ((checksum shares-entry-checksum)
                   (widget shares-entry-widget)) entry
    (q+:set-tool-tip widget +shares-name+ (format nil "Checksum: ~a" checksum))))

(define-signal (shares update-entries) (string))
(define-signal (shares remove-entry) (string))
(define-signal (shares dump-table) ())

(defparameter *new-checksum* nil)
(defparameter *new-size* nil)
(defparameter *new-user* nil)
(defmethod add-node ((shares shares) path path-left parent)
  "Loops over all childs of parent and checks if (car path) is already
  present. If so add-node is recursivly called on that element. If not
  a new child will be added and add-node will be recursivly called on
  that. When the last element of path is reached a new element is
  added and its size is returned."
  (do-childs (element i parent)
    ;; check if node matches, if it does not continue
    (when (string= (car path-left)
                   (q+:text element +shares-name+))
      (unless (cdr path-left)
        ;; if there is no path left but we have a
        ;; matching node, it means that the node
        ;; we tried to add already exists -> we
        ;; return nil to indicate an error
        (return-from add-node nil))
      ;; if add-node was successfull, update size and
      ;; item count, then return t
      (when (add-node shares
                      (concatenate 'string path (car path-left))
                      (cdr path-left)
                      element)
        (with-accessors ((old-size shares-entry-size))
            (gethash (q+:text element +shares-id+) (entries shares))
          (incf old-size *new-size*)
          (qdoto element
                 (q+:set-text +shares-size+
                              (lodds.core:format-size
                               old-size))
                 (q+:set-tool-tip +shares-size+
                                  (format nil "~:d bytes" old-size))))
        (return-from add-node t))
      ;; if add-child-node was not successfull, just return
      ;; nil
      (return-from add-node nil)))
  ;; when we get down here it means there was no matching
  ;; node, so lets add a new one.
  (let ((new-entry (q+:make-qtreewidgetitem parent))
        (current-path (concatenate 'string
                                   path
                                   (car path-left))))
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
    (q+:set-tool-tip parent
                     +shares-name+
                     (format nil "Items: ~a"
                             (incf (shares-entry-items
                                    (gethash
                                     (q+:text parent +shares-id+)
                                     (entries shares))))))
    (q+:add-child parent new-entry)
    ;; if there is path left, add those under the new node. If there
    ;; is no path left it means we reached the last node, so return t
    ;; as we successfully added it. We dont have to update size here,
    ;; since its a new node and it will match the elements size.
    (if (cdr path-left)
        (add-node shares
                  (concatenate 'string path (car path-left))
                  (cdr path-left)
                  new-entry)
        t)))

(defmethod cleanup-node ((shares shares) node)
  "calls finalize on node and all its children to cleanup memory."
  (loop :while (> (q+:child-count node) 0)
        :do (cleanup-node shares (q+:child node 0)))
  (remhash (q+:text node +shares-id+) (entries shares))
  (finalize node))

(defmethod remove-node ((shares shares) path parent)
  "loops over children with get-child-fn, and if path matches
  remove-node is recursivly called on the matching node. Remove node
  will return a Size if a node was successfull removed. This is used to
  updated sizes on parents."
  ;; return nil (failed) if loop went through and we did not
  ;; find the specified node (loop returns nil)
  (do-childs (element i parent)
    ;; check if node matches, if it does not continue
    (flet ((update-parent-items ()
             (q+:set-tool-tip parent
                              +shares-name+
                              (format nil "Items: ~a"
                                      (decf (shares-entry-items
                                             (gethash
                                              (q+:text parent +shares-id+)
                                              (entries shares))))))))
      (when (string= (car path)
                     (q+:text element +shares-name+))
        ;; check if we have a path left and need to call
        ;; remove-node recursivly, if not just remove the
        ;; child and return
        (if (not (cdr path))
            ;; return size (will be returned by
            ;; remove-child-fn) here, since remove was
            ;; successfull
            (let ((size (shares-entry-size
                         (gethash (q+:text element +shares-id+)
                                  (entries shares)))))
              (update-parent-items)
              (cleanup-node shares (q+:take-child parent i))
              (return-from remove-node size))
            (let ((size (remove-node shares
                                     (cdr path)
                                     element)))
              ;; remove-node will return the size of the
              ;; removed element, if successfull.
              (when size
                ;; If element has no childs left (if the
                ;; recursive call removed the last child)
                ;; remove element too. If there are childs
                ;; left, just update the size and item count
                (if (eql 0 (q+:child-count element))
                    (progn
                      (update-parent-items)
                      (cleanup-node shares (q+:take-child parent i)))
                    (with-accessors ((old-size shares-entry-size))
                        (gethash (q+:text element +shares-id+) (entries shares))
                      (decf old-size size)
                      (qdoto element
                             (q+:set-text +shares-size+
                                          (lodds.core:format-size
                                           old-size))
                             (q+:set-tool-tip +shares-size+
                                              (format nil "~:d bytes" old-size)))))
                ;; return size here, since recursive remove
                ;; was successfull
                (return-from remove-node size))))))))

(define-slot (shares update-entries) ((name string))
  (declare (connected shares (update-entries string)))
  (q+:set-updates-enabled shares nil)
  (loop :for (type checksum size path) :in (get-change shares)
        :do (let ((combined-path (concatenate 'string name path)))
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
                               (q+:invisible-root-item shares)))))
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

(defmethod download-item ((shares shares) selected-item)
  (destructuring-bind (type info)
      (get-selected-file shares selected-item)
    (case type
      (:file
       (apply #'open-download-file-dialog info))
      (:dir
       (apply #'open-download-folder-dialog info)))))

(define-override (shares key-press-event) (ev)
  (call-next-qmethod)
  (when (or (= (q+:key ev) (q+:qt.key_enter))
            (= (q+:key ev) (q+:qt.key_return)))
    (let ((selected-items (q+:selected-items shares)))
      (case (length selected-items)
        (0 nil)
        (1 (download-item shares (car selected-items)))
        (t (open-download-multiple-dialog
            (mapcar (lambda (item)
                      (get-selected-file shares item))
                    (q+:selected-items shares))))))))

(define-initializer (shares setup-widget)
  (connect shares
           "itemDoubleClicked(QTreeWidgetItem *, int)"
           (lambda (selected-item column)
             (declare (ignore column))
             (download-item shares selected-item)))
  (make-instance 'shares-entry-dir
                 :shares shares
                 :name ""
                 :path ""
                 :size 0
                 :user ""
                 :widget (q+:invisible-root-item shares))
  (qdoto shares
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
         (q+:set-expands-on-double-click nil))
  (qdoto (q+:header shares)
         (q+:hide)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shares-name+ (q+:qheaderview.stretch))
         (q+:resize-section +shares-size+ 55)))

(define-initializer (shares setup-callbacks)
  ;; move this to list-view later
  (lodds.event:add-callback :qt-shares
                            (lambda (event)
                              (signal! shares
                                       (remove-entry string)
                                       (concatenate 'string
                                                    (second event)
                                                    "/")))
                            :client-removed)
  (lodds.event:add-callback :qt-shares
                            (lambda (event)
                              (destructuring-bind (name type timestamp changes) (cdr event)
                                (declare (ignore timestamp))
                                (when (eql type :all)
                                  (signal! shares (remove-entry string)
                                           (concatenate 'string
                                                        name
                                                        "/")))
                                (add-change shares changes name)))
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
                         (lodds:c-file-table-name user-info))
                (add-change shares changes user)))))

(define-finalizer (shares cleanup-callbacks)
  (lodds.event:remove-callback :qt-shares :client-removed)
  (lodds.event:remove-callback :qt-shares :list-update))
