(in-package #:lodds-qt)
(in-readtable :qtools)

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

(defvar +events+
  '((:advertiser)
    (:listener)
    (:info)
    (:client-added)
    (:client-removed)
    (:client-updated)
    (:debug)
    (:watcher)
    (:tasker)
    (:handler)
    (:list-update)
    (:shared-directory)
    (:unshared-directory))
  "list of events with their callback. For every event there will also
  be checkbox to show/hide the corresponding log message, see
  log-checkboxes-widget.")

(defparameter *selected-file* nil
  "Contains a list containing the currently selected file.
  Containing: Path, Checksum, Filename and List out of Users who
  share the selected file. Path is set to a path if the selected Item
  is a Folder, users will then just be a single user (not a list of
  users).")

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

(defparameter *current-id* 0
  "each time a new widget gets added it will increment the *curren-id*
  and take the new value as its own id.")

(defparameter *id-mapper*
  (make-hash-table :test 'equalp)
  "hash-table mapping generated widget id's to Information. For
  example: Each element in list-of-shares has a hidden id column. If a
  item gets klicked you can get Information like the file owner or the
  filesize by looking up the id inside *id-mapper*.")

(let ((current-id 0)
      (id-lock (bt:make-lock "id-lock")))
  (defun gen-id ()
    "Will return a string with a number which is increased by 1
    everytime gen-id is called"
    (bt:with-lock-held (id-lock)
      (prin1-to-string (incf current-id)))))

(defclass info ()
  ((info-id :reader info-id
            :initform (error "Please specify a id")
            :initarg :id
            :type string
            :documentation "Id which identifies a id-info object, Use
            id to retrieve id-info objects from *id-mapper* table.")
   (info-widget :reader info-widget
                :initform (error "Please specify a widget")
                :initarg :widget
                :documentation "A qt widget corresponding to the given
                id.")))
