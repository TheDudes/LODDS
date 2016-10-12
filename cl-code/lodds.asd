;;;; lodds.asd

(asdf:defsystem #:lodds
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
               :stmx)
  :components ((:file "package")
               (:file "core")
               (:file "low-level-api")
               (:file "lodds")))

