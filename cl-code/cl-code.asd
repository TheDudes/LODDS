;;;; cl-code.asd

(asdf:defsystem #:cl-code
  :description "Common lisp Implementation of _NAME_"
  :author "d4ryus <d4ryus.openmailbox.org>"
  :license "none yet"
  :serial t
  :depends-on (:ironclad)
  :components ((:file "package")
               (:file "cl-code-core")
               (:file "cl-code-low-level-api")))

