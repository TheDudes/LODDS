(in-package #:lodds-qt)
(in-readtable :qtools)

;; shares columns
(defvar +shares-name+ 0)
(defvar +shares-items+ 1)
(defvar +shares-size+ 2)
(defvar +shares-checksum+ 3)
(defvar +shares-id+ 4)

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
                 GET-CHANGE.")))

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

(define-initializer (shares setup-widget)
  (qdoto shares
         (q+:set-column-count 5)
         (q+:set-uniform-row-heights t)
         (q+:set-header-labels (list "Name" "Items" "Size" "Checksum" "ID"))
         (q+:hide-column +shares-id+)
         (q+:set-alternating-row-colors t)
         (q+:set-animated t)
         (q+:set-items-expandable t)
         (q+:set-expands-on-double-click t))

  (qdoto (q+:header shares)
         (q+:set-stretch-last-section nil)
         (q+:set-resize-mode +shares-name+ (q+:qheaderview.stretch))
         (q+:set-resize-mode +shares-items+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +shares-size+ (q+:qheaderview.resize-to-contents))
         (q+:set-resize-mode +shares-checksum+ (q+:qheaderview.resize-to-contents))))

(defun get-selected-file (selected-item)
  "returns a list with info about the selected item. This list will be
  used by the download widget to set its fields"
  (let ((info (gethash (q+:text selected-item +shares-id+) *id-mapper*)))
    (if (eql (type-of info)
             'shares-entry-dir)
        (with-accessors ((user shares-entry-user)
                         (name shares-entry-name)
                         (path shares-entry-path)) info
          (list path
                ;; checksum
                name
                ;; name
                (if (qobject-alive-p (q+:parent selected-item))
                    name
                    "")
                ;; user
                user))
        ;; file was clicked
        (with-accessors ((name shares-entry-name)
                         (checksum shares-entry-checksum)) info
          (list nil
                ;; checksum
                checksum
                ;; name
                name
                ;; users
                (loop :for (user . rest) :in (lodds:get-file-info checksum)
                      :collect user))))))

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
             (q+:set-font +shares-name+ font)
             (q+:set-font +shares-items+ font)
             (q+:set-font +shares-size+ font)
             (q+:set-font +shares-checksum+ font)
             (q+:set-text-alignment +shares-size+ (q+:qt.align-right))
             (q+:set-text-alignment +shares-items+ (q+:qt.align-right))
             (q+:set-text +shares-name+ name)
             (q+:set-text +shares-size+ (lodds.core:format-size size))
             (q+:set-text +shares-id+ id)))))

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
           (q+:set-text +shares-items+ (prin1-to-string items))
           (q+:set-text +shares-checksum+ ""))))

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
           (q+:set-text +shares-items+ "")
           (q+:set-text +shares-checksum+ checksum))))

(define-signal (shares update-entries) (string))
(define-signal (shares remove-entry) (string))
(define-signal (shares dump-table) ())

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
                             (q+:text element +shares-name+))
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
                      (gethash (q+:text element +shares-id+) *id-mapper*)
                    (incf old-size *new-size*)
                    (q+:set-text element
                                 +shares-size+
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
                     +shares-items+
                     (prin1-to-string
                      (incf (shares-entry-items
                             (gethash
                              (q+:text parent +shares-id+)
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
  (remhash (q+:text node +shares-id+) *id-mapper*)
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
                             (q+:text element +shares-name+))
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
                                                             (gethash (q+:text removed-item +shares-id+)
                                                                      *id-mapper*))))
                                                 (q+:set-text element
                                                              +shares-items+
                                                              (prin1-to-string
                                                               (incf (shares-entry-items
                                                                      (gethash
                                                                       (q+:text element +shares-id+)
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
                                (gethash (q+:text element +shares-id+) *id-mapper*)
                              (decf old-size size)
                              (q+:set-text element
                                           +shares-size+
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
                    (add-node ""
                              split-path
                              (lambda () shares)
                              (q+:top-level-item-count shares)
                              (lambda (place) (q+:top-level-item shares place))
                              t))
                  (remove-node (lodds.core:split-path combined-path)
                               (q+:top-level-item-count shares)
                               (lambda (place) (q+:top-level-item shares place))
                               (lambda (place) (cleanup-node (q+:take-top-level-item shares place)))))))
  (q+:set-updates-enabled shares t))

(define-slot (shares remove-entry) ((path string))
  (declare (connected shares (remove-entry string)))
  (remove-node (lodds.core:split-path path)
               (q+:top-level-item-count shares)
               (lambda (place) (q+:top-level-item shares place))
               (lambda (place) (cleanup-node (q+:take-top-level-item shares place)))))

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
  ;; reset id
  (setf *current-id* 0)
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
  (lodds.event:remove-callback :qt-shares :list-update)
  ;; TODO: remove this
  (setf *id-mapper* (make-hash-table :test 'equalp)))
