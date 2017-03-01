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
   (list "interface"
         (car (lodds.core:get-interfaces))
         "Interface Lodds uses to get his own Ip and Broadcast Address."
         :selection
         (lambda ()
           (lodds.core:get-interfaces)))
   (list "remember-shares"
         nil
         "If true: shared directories will be rememberd on restart"
         :boolean)
   (list "port"
         4567
         "The Port where Lodds is listning on for incomming requests"
         :integer)
   (list "broadcast-port"
         9002
         (format nil
                 "The Port where Lodds will listen for Broadcast ~
                 Messages from other clients. This Port is also used ~
                 for advertisements of the Client (as expected, i guess).")
         :integer)
   (list "name"
         (machine-instance)
         "Displayed/broadcasted name of Lodds Client."
         :string)
   (list "trusted-users"
         nil
         (format nil
                 "Comma seperated list of users which are trused. ~
                       This means that if users added to this list send you ~
                       a file you automatically accept it and save it to ~
                       the default upload folder.")
         :list)
   (list "blocked-users"
         nil
         (format nil
                 "Comma seperated list of users which are not trused. ~
                       If a User on this list tries to send you a file, he ~
                       will automatically be blocked.")
         :list)
   ;; QT gui related settings
   (list "download-folder"
         (format nil "~adownload"
                 (user-homedir-pathname))
         "Default Download folder, where files get saved."
         :folder)
   (list "upload-folder"
         (format nil "~aupload"
                 (user-homedir-pathname))
         (format nil
                 "Default Upload folder, incomming files will be ~
                       saved there. If a 'trused-user' is sending a file, ~
                       its saved there too.")
         :folder)
   (list "resources-folder"
         "."
         "Folder which contains resources for Lodds, like icons, stylesheets, ..."
         :folder)
   (list "timeout-send-file"
         300
         "Default Timeout in seconds when sending a file."
         :integer)
   (list "info-update-interval"
         200
         "Timeout (in milliseconds) between updates on 'info' widget."
         :integer)))

(defun split-key-value (line)
  (format t "splitting: ~a~%" line)
  (let* ((equal-pos (position #\= line))
         (key (subseq line 0 equal-pos))
         (value (subseq line (+ 1 equal-pos) (length line))))
    (values (string-right-trim '(#\Space #\Tab) key)
            (string-left-trim '(#\Space #\Tab) value))))

(defun convert-type (value type)
  "converts given value to given type. returns two values, the
  convertes value and a flag which is nil when everything was ok, or
  contains a string with a error message"
  (case type
    (:boolean (cond ((eql t value) (values t nil))
                    ((eql nil value) (values nil nil))
                    ((equalp "true" value) (values t nil))
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
    (:folder (if (not (uiop:directory-exists-p value))
                 (values nil (format nil "Directory ~a does not exist." value))
                 (values value nil)))
    (:selection (values value nil))
    (t (values nil (format nil "Type ~a not recognised" type)))))

(defun validate-key-value (key value)
  "Returns nil on success, or a error string on failure"
  (cond
    ((equal key "interface")
     (let ((valid (lodds.core:get-interfaces)))
       (unless (find value valid :test #'equal)
         (format nil "Invalid interface (~a), valid are only: ~a"
                 value valid))))))

(defun generate-default-config ()
  (let ((config (make-hash-table :test 'equalp)))
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
               (destructuring-bind (val desc type &optional nil) value
                 (format stream "# ~a~%~a=~a~%~%"
                         desc
                         key
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
             (let ((err (validate-key-value key (car value))))
               (when err
                 (return-from validate-config err))))
           config))

(defun update-entry (config key new-value &optional (push-event-p t))
  "updates a single entry, returns nil if successfull, error-string if
  failed"
  (let ((entry (gethash key config)))
    (unless entry
      (return-from update-entry
        (format nil "Unrecognized key ~a" key)))
    (multiple-value-bind (value error) (convert-type new-value (third entry))
      (when error
        (return-from update-entry error))
      (setf error (validate-key-value key value))
      (when error
        (return-from update-entry error))
      (setf (car entry) value)
      (setf (gethash key config) entry)
      (when push-event-p
        (lodds.event:push-event :config-updated (list key value)))
      nil)))

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
                  (let ((err (update-entry config key value nil)))
                    (when err
                      (return-from load-from-file
                        (values nil (format nil "Config file error (~a:~a): ~a"
                                            filename
                                            line-number
                                            err))))))))
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
          :finally (return config))))

(defun get-all-keys (config)
  "Returns all known configuration keys"
  (loop :for key :being :the :hash-key :of config
        :collect key))

(defun get-value (key config)
  "Returns the value of a given configuration key"
  (first (gethash key config)))

(defun get-description (key config)
  "Returns the description of a given configuration key"
  (second (gethash key config)))

(defun get-type (key config)
  "returns the type of a given configuration key"
  (third (gethash key config)))

(defun get-selection-options (key config)
  "returns the selection options of a configuration key of type
  :selection, if key is not of type :selection nil is returned"
  (let ((fn (fourth (gethash key config))))
    (when fn
      (funcall fn))))
