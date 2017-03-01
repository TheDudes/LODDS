;;;; cl-lodds.asd

(asdf:defsystem #:cl-lodds
  :description "Common lisp Implementation of LODDS-Protocol"
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "none yet"
  :serial t
  :depends-on (:ironclad
               :usocket
               :ip-interfaces
               :flexi-streams
               :cl-strings
               :cl-ppcre
               :bordeaux-threads
               :cl-fs-watcher
               :lparallel)
  :components ((:file "package")
               (:file "core")
               (:file "config")
               (:file "low-level-api")
               (:file "classes")
               (:file "subsystem")
               (:file "task")
               (:file "watcher")
               (:file "event")
               (:file "listener")
               (:file "advertiser")
               (:file "handler")
               (:file "lodds")))
