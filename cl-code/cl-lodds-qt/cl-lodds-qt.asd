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
               :cl-strings)
  :components ((:file "package")
               (:file "globals")
               (:file "misc")
               (:file "log")
               (:file "gui")))
