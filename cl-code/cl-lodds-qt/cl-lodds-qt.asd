;;;; cl-lodds.asd

(asdf:defsystem #:cl-lodds-qt
  :description "Common lisp Implementation of LODDS-Protocol - QT gui"
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "none yet"
  :serial t
  :depends-on (:cl-lodds
               :qtools
               :qtcore
               :qtgui
               :cl-strings
               :qtools-ui-debugger)
  :components ((:file "package")
               (:file "globals")
               (:file "misc")
               (:file "log")
               (:file "user-list")
               (:file "shared")
               (:file "shares")
               (:file "interface")
               (:file "dialog")
               (:file "dock")
               (:file "send-permission")
               (:file "send-file")
               (:file "info")
               (:file "directory-error")
               (:file "download-file")
               (:file "download-folder")
               (:file "gui")))
