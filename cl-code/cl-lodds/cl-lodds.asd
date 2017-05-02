;;;; cl-lodds.asd

(asdf:defsystem #:cl-lodds
  :version "0.1.0"
  :description "Common lisp Implementation of LODDS-Protocol"
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "GPLv2"
  :maintainer "d4ryus <d4ryus@openmailbox.org>"
  :homepage "https://github.com/TheDudes/Lodds"
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
               :asdf
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "core")
                             (:file "config")
                             (:file "low-level-api")
                             (:file "classes")
                             (:file "task")
                             (:file "watcher")
                             (:file "event")
                             (:file "event-loop")
                             (:file "listener")
                             (:file "handler")
                             (:file "lodds")))
               (:module "test"
                :serial t
                :components ((:test-file "core")
                             (:test-file "config")
                             (:test-file "low-level-api"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
