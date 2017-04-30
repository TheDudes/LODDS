;;;; cl-lodds.asd

(asdf:defsystem #:cl-lodds-qt
  :version "0.1.0"
  :description "Common lisp Implementation of LODDS-Protocol - Qt gui"
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "GPLv2"
  :maintainer "d4ryus <d4ryus@openmailbox.org>"
  :homepage "https://github.com/TheDudes/Lodds"
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "lodds-qt"
  :entry-point "lodds-qt:main"
  :serial t
  :depends-on (:cl-lodds
               :qtools
               :qtcore
               :qtgui
               :qtsvg
               :cl-strings
               :qtools-ui-debugger)
  :components ((:file "package")
               (:file "globals")
               (:file "misc")
               (:file "graph")
               (:file "log")
               (:file "user-list")
               (:file "shared")
               (:file "shares")
               (:file "dialog")
               (:file "dock")
               (:file "send-permission")
               (:file "send-file")
               (:file "info")
               (:file "selection")
               (:file "download-file")
               (:file "download-folder")
               (:file "download-multiple")
               (:file "settings")
               (:file "gui")))
