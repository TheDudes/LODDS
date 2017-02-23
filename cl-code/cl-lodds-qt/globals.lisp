(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter *style-sheet*
  "QTreeView#Shares {
     alternate-background-color: #eeeeef;
     background-color: #ffffff;
   }

   QTreeView#Shares::branch:has-children:!has-siblings:closed,
   QTreeView#Shares::branch:closed:has-children:has-siblings {
     border-image: none;
     image: url(./res/folder-closed.png);
   }

   QTreeView#Shares::branch:open:has-children:!has-siblings,
   QTreeView#Shares::branch:open:has-children:has-siblings  {
     border-image: none;
     image: url(./res/folder-open.png);
   }

(defvar +events+
  '(:advertiser
    :listener
    :tasker
    :task-finished
    :task-failed
    :task-canceled
    :info
    :client-added
    :client-removed
    :client-updated
    :debug
    :watcher
    :handler
    :list-update
    :shared-directory
    :unshared-directory
    :name-changed)
  "For every event there will also be checkbox to show/hide the
  corresponding log message, see info-log.lisp info-log-settings

  TODO: this should be inside lodds lib.")
