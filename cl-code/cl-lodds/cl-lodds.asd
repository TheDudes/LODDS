;;;; cl-lodds.asd

(asdf:defsystem #:cl-lodds
  :version "0.1.0"
  :description "Common lisp Implementation of LODDS-Protocol"
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "GPLv2"
  :maintainer "d4ryus <d4ryus@openmailbox.org>"
  :homepage "https://github.com/TheDudes/Lodds"
  :serial t
  :depends-on (:ironclad
               :usocket
               :ip-interfaces
               :flexi-streams
               :cl-strings
               :cl-ppcre
               :bordeaux-threads
               :cl-fs-watcher
               :lparallel
               :uiop
               :asdf)
  :components ((:file "package")
               (:file "core")
               (:file "config")
               (:file "low-level-api")
               (:file "classes")
               ;; (:file "subsystem")
               ;;(:file "task")
               (:file "watcher")
               (:file "event")
               (:file "advertiser")
               (:file "event-loop")
               (:file "listener")
               ;;(:file "handler")
               (:file "lodds")))
