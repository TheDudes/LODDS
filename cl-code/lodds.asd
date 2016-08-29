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
               :cl-strings)
  :components ((:file "package")
               (:file "lodds-core")
               (:file "lodds-low-level-api")
               (:file "lodds")))

