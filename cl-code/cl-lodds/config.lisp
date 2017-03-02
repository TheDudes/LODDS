;;;; config.lisp

(in-package #:lodds.config)

;; TODO: should be a better way to do this, but ok for now
(defparameter *load-path*
  (list "/etc/lodds.config"
        (format nil "~a.lodds.config"
                (user-homedir-pathname))
        (format nil "~a.config/lodds.config"
                (user-homedir-pathname))
        (format nil "./lodds.config")))

(defun generate-default-list ()
  "Used to generate the default configuration, each element contains
  the configuration key, its default value, a description and the
  type, in that order."
  (list
   (list :interface
         (car (lodds.core:get-interfaces))
         (format nil
                 "Interface Lodds uses to get his own Ip and Broadcast~%~
                 Address.")
         :selection
         (lambda ()
           (lodds.core:get-interfaces)))
   (list :remember-shares
         nil
         (format nil
                 "If true: shared directories will be rememberd on restart")
         :boolean)
   (list :port
         4567
         (format nil
                 "The Port where Lodds is listning on for incomming~%~
                 requests. Value between 1024 and 65535.")
         :integer
         1024
         65535)
   (list :broadcast-port
         9002
         (format nil
                 "The Port where Lodds will listen for Broadcast~%~
                 Messages from other clients. This Port is also~%~
                 used for advertisements of the Client (as expected,~%~
                 i guess). Value between 1024 and 65535.")
         :integer
         1024
         65535)
   (list :name
         (machine-instance)
         (format nil
                 "Displayed/broadcasted name of Lodds Client.")
         :string)
   (list :trusted-users
         nil
         (format nil
                 "Comma seperated list of users which are trused.~%~
                 This means that if users added to this list send~%~
                 you a file you automatically accept it and save~%~
                 it to the default upload folder.")
         :list)
   (list :blocked-users
         nil
         (format nil
                 "Comma seperated list of users which are not~%~
                 trused. If a User on this list tries to send~%~
                 you a file, he will automatically be blocked.")
         :list)
   (list :client-timeout
         5
         (format nil
                 "If a client does not send a advertise message~%~
                 after the given timeout (seconds), he will be~%~
                 removed. Value between 1 and 3600.")
         :integer
         1
         3600)
   (list :advertise-timeout
         1
         (format nil
                 "Timeout between advertisements. Specified~%~
                 in seconds. Value between 1 and 10")
         :integer
         1
         10)
   ;; TODO: QT gui related settings, these should be splitted out to
   ;; lodds-qt package and then be hooked somehow, but i couldn't
   ;; think of any good way to achieve it. Thats why i will just leave
   ;; them here for now.
   (list :download-folder
         (format nil "~adownload"
                 (user-homedir-pathname))
         (format nil
                 "Default Download folder, where files get saved.")
         :folder)
   (list :upload-folder
         (format nil "~aupload"
                 (user-homedir-pathname))
         (format nil
                 "Default Upload folder, incomming files will~%~
                 be saved there. If a 'trused-user' is sending~%~
                 a file, its saved there too.")
         :folder)
   (list :resources-folder
         "."
         (format nil
                 "Folder which contains resources for Lodds,~%~
                 like icons, stylesheets, ...")
         :folder)
   (list :timeout-send-file
         300
         (format nil
                 "Default Timeout in seconds when sending a~%~
                 file. Value between 1 and 3600")
         :integer
         1
         3600)
   (list :info-update-interval
         100
         (format nil
                 "Timeout (in milliseconds) between updates~%~
                 on 'info' widget. Value between 50 and 100.000")
         :integer
         50
         100000)))

(defun split-key-value (line)
  (let* ((equal-pos (position #\= line))
         (key (subseq line 0 equal-pos))
         (value (subseq line (+ 1 equal-pos) (length line))))
    (values (intern (string-upcase (string-right-trim '(#\Space #\Tab) key)) :keyword)
            (string-left-trim '(#\Space #\Tab) value))))

(defun string-to-type (value type)
  "converts given value to given type. returns two values, the
  convertes value and a flag which is nil when everything was ok, or
  contains a string with a error message"
  (case type
    (:boolean (cond ((equalp "true" value) (values t nil))
                    ((equalp "false" value) (values nil nil))
                    (t (values nil
                               (format nil
                                       "Value is not a boolean ~
                                       value: ~a" value)))))
    (:list (values (mapcar (lambda (str)
                             (string-trim '(#\Space) str))
                           (unless (eql 0 (length (string-trim '(#\Space) value)))
                             (cl-strings:split value #\,)))
                   nil))
    (:string (values value nil))
    (:integer (handler-case
                  (values (parse-integer value) nil)
                (error (e)
                  (declare (ignore e))
                  (values nil (format nil "~a is not a integer" value)))))
    (:folder (values value nil))
    (:selection (values value nil))
    (t (values nil (format nil "Type ~a not recognised" type)))))

(defun validate-new-entry (key value config)
  "Returns nil on success, or a error string on failure"
  (unless (case (get-type key config)
            (:boolean   (or (eql t value)
                            (eql nil value)))
            (:list      (listp value))
            (:string    (stringp value))
            (:integer   (integerp value))
            (:folder    (stringp value))
            (:selection (stringp value))
            (t nil))
    (return-from validate-new-entry
      (format nil "~a is not of expected key (~a) type ~a"
              value
              key
              (get-type key config))))

  (case (get-type key config)
    (:integer (progn
                (when (< value (get-integer-min key))
                  (return-from validate-new-entry
                    (format nil "~a is lower then allowed min (~a)"
                            value
                            (get-integer-min key))))
                (when (> value (get-integer-max key))
                  (return-from validate-new-entry
                    (format nil "~a is higher then allowed max (~a)"
                            value
                            (get-integer-max key))))))
    (:selection (let ((valid (get-selection-options key)))
                  (unless (find value valid :test #'equal)
                    (return-from validate-new-entry
                      (format nil "Invalid selection (~a), valid are only: ~a"
                              value valid)))))
    (:folder (unless (uiop:directory-exists-p value)
               (return-from validate-new-entry
                 (format nil "Folder ~a does not exist"
                         value)))))
  nil)

(defun generate-default-config ()
  (let ((config (make-hash-table)))
    (loop :for (key . value) :in (generate-default-list)
          :do (setf (gethash key config)
                    value))
    config))

(defun save-to-file (filename &optional (config (generate-default-config)))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (maphash (lambda (key value)
               (destructuring-bind (val desc type &rest nil) value
                 (format stream "# ~a~%~a=~a~%~%"
                         (cl-strings:replace-all desc
                                                 (format nil "~%")
                                                 (format nil "~%# "))
                         (string-downcase (string key))
                         (case type
                           (:boolean (if val
                                         "true"
                                         "false"))
                           (:list (format nil
                                          "~{~a~^,~}" val))
                           (t val)))))
             config)))

(defun validate-config (config)
  "returns nil if given config is valid, error string if not"
  (maphash (lambda (key value)
             (let ((err (validate-new-entry key (car value) config)))
               (when err
                 (return-from validate-config err))))
           config))

(defun update-entry (key new-value &optional
                                     (push-event-p t)
                                     (config (slot-value lodds:*server* 'lodds:settings)))
  "updates a single entry, returns nil if successfull, error-string if
  failed"
  (let* ((entry (gethash key config))
         (old-value (car entry)))
    (unless entry
      (return-from update-entry
        (format nil "Unrecognized key ~a" (string-downcase (string key)))))
    (let ((err (validate-new-entry key new-value config)))
      (when err
        (return-from update-entry err))
      (unless (equal old-value
                     new-value)
        (setf (car entry) new-value)
        (setf (gethash key config) entry)
        (when push-event-p
          (lodds.event:push-event :config-changed
                                  (list key old-value new-value)))
        nil))))

(defun load-from-file (filename &optional (config (generate-default-config)))
  "returns the config and nil if config file was parsed without errors
  and nil plus a error string describing the error on failure"
  (with-open-file (stream filename
                          :direction :input)
    (let ((line-number 0))
      (loop :for line = (read-line stream nil nil)
            :while line
            :do
            (progn
              (incf line-number)
              (unless (or (eql 0 (length line))
                          (cl-strings:starts-with line "#")
                          (not (position #\= line)))
                (multiple-value-bind (key value)
                    (split-key-value line)

                  (multiple-value-bind (converted-value error)
                      (string-to-type value
                                      (get-type key config))

                    (when error
                      (return-from load-from-file
                        (values nil
                                (format nil "Config file error (~a:~a): ~a"
                                        filename
                                        line-number
                                        error))))
                    (let ((err (update-entry key converted-value nil config)))
                      (when err
                        (return-from load-from-file
                          (values nil (format nil "Config file error (~a:~a): ~a"
                                              filename
                                              line-number
                                              err)))))))))
            :finally (return (values config nil))))))

(defun load-default-config-files ()
  "returns two values, the config and nil on success, or nil and a
  error message on failure"
  (let ((config (generate-default-config)))
    (loop :for element :in *load-path*
          :do (when (uiop:file-exists-p element)
                (multiple-value-bind (result error-msg)
                    (load-from-file element config)
                  (unless result
                    (return-from load-default-config-files
                      (values nil error-msg)))))
          :finally (return (values config nil)))))

(defun get-all-keys (&optional (config (slot-value lodds:*server* 'lodds:settings)))
  "Returns all known configuration keys"
  (loop :for key :being :the :hash-key :of config
        :collect key))

(defun get-value (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  "Returns the value of a given configuration key"
  (first (gethash key config)))

(defun get-description (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  "Returns the description of a given configuration key"
  (second (gethash key config)))

(defun get-type (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  "returns the type of a given configuration key"
  (third (gethash key config)))

(defun get-selection-options (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  "returns the selection options of a configuration key of type
  :selection, if key is not of type :selection nil is returned"
  (let ((fn (fourth (gethash key config))))
    (when fn
      (funcall fn))))

(defun get-integer-min (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  (fourth (gethash key config)))

(defun get-integer-max (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  (fifth (gethash key config)))
