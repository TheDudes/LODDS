(in-package :cl-user)

(defpackage test-core
  (:use :cl :prove :lodds.core))
(in-package :test-core)

(plan nil)

(subtest "escape-wildcards"
  (is (escape-wildcards "/test/*/some file [10].txt?")
      "/test/\\*/some file \\[10].txt\\?"
      "escape string which contains wildcards")

  (let ((data "some string without wildcards"))
    (is (escape-wildcards data) data
        "escape string without wildcards")))

(subtest "generate-fake-checksum"
    (is (length (generate-fake-checksum 10))
        10
        "length equal to 10")
    (is (length (generate-fake-checksum 40))
        40
        "length equal to 40")
    (is-type (generate-fake-checksum 40) 'string
             "type string"))

(subtest "format-timestamp"
  (is (format-timestamp 0)
      "1970-01-01 01:00:00"
      "0 equal to \"1970-01-01 01:00:00\""))

(subtest "copy-stream"
  (flet ((make-random-binary-vector (size)
           (map '(vector (unsigned-byte 8))
                (lambda (element)
                  (declare (ignore element))
                  (random 256))
                (make-array size :element-type '(unsigned-byte 8)))))

    (is (copy-stream nil nil 0) 0
        "(copy-stream nil nil 0)")

    (subtest "errors"
      (is-error (copy-stream (flex:make-in-memory-input-stream nil)
                             (flex:make-in-memory-output-stream)
                             10)
                'could-not-read-error
                "from stream empty")

      (is-error (copy-stream (flex:make-in-memory-input-stream
                              (make-random-binary-vector 10))
                             (flex:make-in-memory-output-stream)
                             20)
                'out-of-data-error
                "size > from stream data")

      (is-error (copy-stream (flex:make-in-memory-input-stream
                              (map 'vector #'char-code "to short"))
                             (flex:make-in-memory-output-stream)
                             10)
                'out-of-data-error
                "from stream to short"))

    (flet ((check-random-content (size)
             (let* ((data (make-random-binary-vector size))
                    (digester (ironclad:make-digest :sha1))
                    (from (flex:make-in-memory-input-stream data))
                    (to (flex:make-in-memory-output-stream)))
               (subtest (format nil "random data with size ~a" size)
                 (is (copy-stream from to size digester)
                     size
                     (format nil "returned size equal ~a" size))
                 (is (flex:get-output-stream-sequence to)
                     data
                     :test #'equalp
                     "data in 'to' stream matches data in 'from' stream")
                 (is (ironclad:produce-digest digester)
                     (ironclad:digest-sequence :sha1 data)
                     :test #'equalp
                     "digester equalp")))))
      (check-random-content 5000)
      (check-random-content 4096)
      (check-random-content 2048))))

(subtest "str-case"
  (ok (str-case "correct"
        ("no" nil)
        ("still-no" nil)
        ("correct" t)
        (t nil)))
  (ok (str-case "correct"
        ("no" nil)
        ("still-no" nil)
        ("correct" t)
        (t nil)))
  (ok (not (str-case "correct"
             ("no" nil)
             ("still-no" nil)
             ("nope" nil)))))

(subtest "format-size"
  (ok (format-size 0 t)
      "returns given when-zero (t)")

  (ok (not (format-size 0 nil))
      "returns given when-zero (nil)")

  (is (format-size 0)
      "0Byt"
      "returns size string (0 => 0Byt)"))

(subtest "red-yellow-green-gradient-generator"
  (let ((len 10))
    (is (length (lodds.core::red-yellow-green-gradient-generator len))
        len
        "check amount of generated elements")))

(subtest "get-size-color"
  (is (get-size-color 0)
      "#00FF00")

  (is (get-size-color (ash 1 43))
      (get-size-color (ash 1 49))))

(subtest "split-user-identifier"
  (split-user-identifier (name ip port) "test@123.456.789.000:12345"
    (is name "test")
    (is ip "123.456.789.000")
    (is port "12345"))

  (split-user-identifier (name ip port t) "test@123.456.789.000:12345"
    (is name "test")
    (is ip #(123 456 789 000) :test #'equalp)
    (is port 12345)))

(subtest "split-path"
  (is (split-path "/this/is/some/path")
      (list "/" "this/" "is/" "some/" "path")
      :test #'equalp
      "path without trailing slash")
  (is (split-path "/this/is/some/path/")
      (list "/" "this/" "is/" "some/" "path/")
      :test #'equalp
      "path with trailing slash")
  (is (split-path "/")
      (list "/")
      :test #'equalp
      "just root"))

(subtest "ensure-trailing-slash"
  (let ((data "/this/is/a/test"))
    (is (ensure-trailing-slash data)
        (concatenate 'string data "/")
        "path without trailing slash"))
  (let ((data "/this/is a test /string/"))
    (is (ensure-trailing-slash data)
        data
        "path with trailing slash")))

(subtest "format-seconds"
  (is (format-seconds 10)
      "00:10")

  (is (format-seconds 60)
      "01:00")

  (is (format-seconds 90)
      "01:30"))

(let ((interfaces (get-interfaces)))
  (if (not interfaces)
      (skip 1 "Could not get list of interfaces")
      (subtest "list-of-interfaces"
        (flet ((test-interface (interface)
                 (subtest (format nil "interface (~a)" interface)
                   (ok (get-interface-info interface))
                   (ok (get-broadcast-address interface))
                   (ok (get-ip-address interface)))))
          (map nil #'test-interface interfaces)))))

(finalize)
