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
  '(:advertiser
    :listener
    :info
    :client-added
    :client-removed
    :client-updated
    :debug
    :watcher
    :tasker
    :handler
    :list-update
    :shared-directory
    :unshared-directory)
  "For every event there will also be checkbox to show/hide the
  corresponding log message, see info-log.lisp info-log-settings

  TODO: this should be inside lodds lib.")

(defparameter *selected-file* nil
  "Contains a list containing the currently selected file.
  Containing: Path, Checksum, Filename and List out of Users who
  share the selected file. Path is set to a path if the selected Item
  is a Folder, users will then just be a single user (not a list of
  users).")
