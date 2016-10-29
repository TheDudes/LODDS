;;;; lodds.asd

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
               :stmx
               :cffi
               :cl-fs-watcher)
  :components ((:file "package")
               (:file "core")
               (:file "watcher")
               (:file "low-level-api")
               (:file "lodds")))

