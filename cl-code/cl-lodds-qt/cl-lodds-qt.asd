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
               (:file "download")
               (:file "shares")
               (:file "interface")
               (:file "dialog")
               (:file "dock")
               (:file "select-file")
               (:file "send-file")
               (:file "gui")))
