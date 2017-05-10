(in-package :cl-user)

(defpackage test-config
  (:use :cl :prove :lodds.config))
(in-package :test-config)

(plan nil)

(lodds.config::generate-log-color-setting "#123123" :blub)

(defun test-config (testfile)

  (subtest "key-to-keyword"
    (is (lodds.config::key-to-log-keyword "test")
        :log-test-color)

    (is (lodds.config::key-to-log-keyword "something")
        :log-something-color))

  (subtest "generate-log-color-setting"
    (is (lodds.config::generate-log-color-setting "#123123" :test)
        (list :log-test-color
              "#123123"
              :color
              (format nil
                      "Background Color of TEST~%~
                      Event on log widget when show-log-type-colors~%~
                      is true~%~
                      Default: #123123"))
        :test #'equalp
        "basic test"))

  (subtest "*color-scanner*"
    (ok (cl-ppcre:scan *color-scanner* "#123123"))
    (ok (cl-ppcre:scan *color-scanner* "#FFFFFF"))
    (ok (cl-ppcre:scan *color-scanner* "#000000"))
    (ok (cl-ppcre:scan *color-scanner* "#a03f12"))
    (ok (not (cl-ppcre:scan *color-scanner* "123123")))
    (ok (not (cl-ppcre:scan *color-scanner* "#3123123")))
    (ok (not (cl-ppcre:scan *color-scanner* "#123:23")))
    (ok (not (cl-ppcre:scan *color-scanner* "#123G23")))
    (ok (not (cl-ppcre:scan *color-scanner* "#-123323"))))

  (subtest "split-key-value"
    (is-values (lodds.config::split-key-value "somekey=430")
               '(:somekey "430")
               "basic split")

    (is-values (lodds.config::split-key-value "another-key=something with spaces   ")
               '(:another-key "something with spaces   ")
               "value with trailing spaces")

    (is-values (lodds.config::split-key-value "another-key=  something with spaces   ")
               '(:another-key "something with spaces   ")
               "value with trailing spaces and spaces after equals")

    (is-values (lodds.config::split-key-value "another-key  =  something with spaces   ")
               '(:another-key "something with spaces   ")
               "value with trailing spaces and spaces around equals"))

  (subtest "string-to-type"
    (subtest ":boolean"
      (is-values (lodds.config::string-to-type "true" :boolean)
                 '(t nil))
      (is-values (lodds.config::string-to-type "false" :boolean)
                 '(nil nil))
      (is-values (lodds.config::string-to-type "True" :boolean)
                 '(t nil))
      (is-values (lodds.config::string-to-type "False" :boolean)
                 '(nil nil))

      (multiple-value-bind (value error)
          (lodds.config::string-to-type "crap" :boolean)
        (ok (not value) "return nil on error")
        (is-type error 'string "return string describing error")))

    (subtest ":list"
      (is-values (lodds.config::string-to-type "some,strange,list" :list)
                 '(("some" "strange" "list") nil)
                 "basic list")

      (is-values (lodds.config::string-to-type "some  ,  strange  , list" :list)
                 '(("some" "strange" "list") nil)
                 "list with spaces around comma")

      (is-values (lodds.config::string-to-type "some  ,,   ,  strange  , list" :list)
                 '(("some" "" "" "strange" "list") nil)
                 "list with spaces around comma and empty entries")

      (is-values (lodds.config::string-to-type "" :list)
                 '(nil nil)
                 "empty list"))

    (subtest ":string"
      (is-values (lodds.config::string-to-type "test" :string)
                 '("test" nil)
                 "basic string"))

    (subtest ":integer"
      (is-values (lodds.config::string-to-type "10" :integer)
                 '(10 nil)
                 "basic integer")

      (multiple-value-bind (value error)
          (lodds.config::string-to-type "crap" :integer)
        (ok (not value) "return nil on error")
        (is-type error 'string "return string describing error")))

    (subtest ":folder"
      (flet ((check (folder absolute?)
               (subtest (format nil "folder ~a" folder)
                 (multiple-value-bind (value error)
                     (lodds.config::string-to-type folder :folder)
                   (ok (not error) "no error returned")
                   (is-type value 'pathname)
                   (if absolute?
                       (ok (uiop:absolute-pathname-p value)
                           "is absolute pathname")
                       (ok (not (uiop:absolute-pathname-p value))
                           "is relative pathname"))
                   (ok (uiop:directory-pathname-p value) "is directory pathname")))))
        (check "/this/is/a/test" t)
        (check "/this/is/another/test/" t)
        (check "some/relative/folder" nil)
        (check "another/relative/folder/" nil)))

    (subtest ":selection"
      (is-values (lodds.config::string-to-type "test" :selection)
                 '("test" nil)
                 "basic selection"))

    (subtest ":color"
      (is-values (lodds.config::string-to-type "#123123" :color)
                 '("#123123" nil)
                 "basic color"))

    (subtest ":font"
      (is-values (lodds.config::string-to-type "monospace" :font)
                 '("monospace" nil)
                 "basic font")))

  (subtest "validate-config"
    (ok (not (validate-config (generate-default-config)))
        "validate default configuration")

    (let ((config (generate-default-config)))
      (setf (gethash :random config) "error")
      (is-type (validate-config config) 'string
               "catch corrupt entries")))

  (subtest "save-to/load-from-file"
    (let* ((matching 0)
           (failed 0)
           (config (generate-default-config))
           (key-count (hash-table-count config)))
      (subtest "save-to"
        (handler-case (progn
                        (save-to-file testfile config)
                        (pass "successfully saved"))
          (error (err)
            (fail (format nil "raised error: ~a" err)))))
      (subtest "load-from"
        (maphash (lambda (k v)
                   (if (equalp (gethash k config) v)
                       (incf matching)
                       (progn
                         (incf failed)
                         (fail (format nil "key: ~a did not match (expected: ~a, got: ~a)"
                                       k v (gethash k config))))))
                 (load-from-file testfile))
        (if (> failed 0)
            (fail (format nil "~a out of ~a keys did not match"
                          failed key-count))
            (if (not (eql (+ failed matching) key-count))
                (fail (format nil "only ~a keys found, ~a expected"
                              (+ failed matching) key-count))
                (pass "keys and values match"))))))


  (subtest "config accessors"
    (let ((config (generate-default-config)))
      (flet ((test-key (key value type description)
               (is (get-value key config) value "value")
               (is (get-type key config) type "type")
               (is (get-description key config) description "description")))
        (subtest ":string"
          (test-key :name (machine-instance) :string
                    (format nil
                            "Displayed/broadcasted name of lodds client.~%~
                             Default: OS username")))

        (subtest ":integer"
          (test-key :port 4567 :integer
                    (format nil
                            "The port where lodds is listening on for incomming~%~
                            requests. Value between 1024 and 65535.~%~
                            Restart is needed for changes to take effect~%~
                            (Lodds -> Restart).~%~
                            Default: 4567"))
          (is (get-integer-min :port config) 1024 "integer-min")
          (is (get-integer-max :port config) 65535 "integer-max"))

        (subtest ":selection"
          (let ((data (list "test-value-1"
                            :selection
                            "nothing special"
                            (lambda ()
                              (list "test-value-1"
                                    "test-value-2"
                                    "test-value-3")))))
            (setf (gethash :selection-test config) data)
            (apply #'test-key :selection-test (butlast data))
            (is (get-selection-options :selection-test config)
                (funcall (car (last data)))
                :test #'equalp
                "selection options")))

        (subtest ":list"
          (let ((data (list "list-value-1"
                            :list
                            "nothing special here"
                            (lambda ()
                              (list "list-value-1"
                                    "list-value-2"
                                    "list-value-3")))))
            (setf (gethash :list-test config) data)
            (apply #'test-key :list-test (butlast data))
            (is (get-suggestions :list-test config)
                (funcall (car (last data)))
                :test #'equalp
                "suggestions")))

        (subtest ":boolean"
          (test-key :deny-requests nil :boolean
                    (format nil
                            "If set to true all incomming send requests will~%~
                            denied.~%~
                            Default: false")))

        (subtest ":color"
          (let ((data (lodds.config::generate-log-color-setting "#123123" :test)))
            (setf (gethash :color-test config) data)
            (apply #'test-key :color-test (butlast data))))

        (subtest "get-log-event-color"
          (let ((config (generate-default-config)))
            (is (get-log-event-color :debug config)
                "#888888")

            (is (get-log-event-color :task-finished config)
                "#1ed760")))

        (subtest ":font"
          (test-key :log-font "Monospace, Inconsolata, Consolas" :font
                    (format nil
                            "Font used on Log widget.~%~
                            Default: monospace")))))))

(let ((testfile (make-pathname :name "___test-config")))
  (if (uiop:file-exists-p testfile)
      (skip 1 (format nil "Test file ~a exists in file system" testfile))
      (progn
        (test-config testfile)
        (delete-file testfile))))

(finalize)
