(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter *intro*
  #.(uiop:read-file-string
     (merge-pathnames
      "../intro.html"
      (uiop:current-lisp-file-pathname))))

(defvar +events+
  '(:advertiser
    :listener
    :task-finished
    :task-failed
    :task-canceled
    :info
    :user-added
    :user-removed
    :user-updated
    :debug
    :watcher
    :handler
    :list-update
    :shared-directory
    :unshared-directory
    :directory-error
    :folder-download-error
    :send-permission
    :config-changed)
  "For every event there will also be checkbox to show/hide the
  corresponding log message, see info-log.lisp info-log-settings

  TODO: this should be inside lodds lib.")
