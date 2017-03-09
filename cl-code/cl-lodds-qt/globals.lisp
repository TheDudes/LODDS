(in-package #:lodds-qt)
(in-readtable :qtools)

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
    :directory-error
    :folder-download-error
    :send-permission
    :config-changed)
  "For every event there will also be checkbox to show/hide the
  corresponding log message, see info-log.lisp info-log-settings

  TODO: this should be inside lodds lib.")
