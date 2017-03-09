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
                 Address.~%~
                 Default: The first interface with an ip (excluding loopback)")
         :selection
         (lambda ()
           (lodds.core:get-interfaces)))
   (list :remember-shares
         nil
         (format nil
                 "If true: shared directories will be rememberd on restart~%~
                  Default: false")
         :boolean)
   (list :port
         4567
         (format nil
                 "The Port where Lodds is listning on for incomming~%~
                 requests. Value between 1024 and 65535.~%~
                 Default: 4567")
         :integer
         1024
         65535)
   (list :broadcast-port
         9002
         (format nil
                 "The Port where Lodds will listen for Broadcast~%~
                 Messages from other clients. This Port is also~%~
                 used for advertisements of the Client (as expected,~%~
                 i guess). Value between 1024 and 65535.~%~
                 Default: 9002")
         :integer
         1024
         65535)
   (list :name
         (machine-instance)
         (format nil
                 "Displayed/broadcasted name of Lodds Client.~%~
                  Default: User Name of System")
         :string)
   (list :deny-requests
         nil
         (format nil
                 "If set to true all incomming send requests will~%~
                 denied.~%~
                 Default: false")
         :boolean)
   (list :allow-unkown-user-send
         t
         (format nil
                 "If a Send Request is received Lodds tries to~%~
                 Figure out which user sent the request (via the~%~
                 Ip). If the User does not Advertise himself~%~
                 Lodds cannot determine the User who send the request~%~
                 Since the user is unkown. If allow-unknown-user-send~%~
                 is false, users which are unknown to Lodds wont~%~
                 be able to send files.~%~
                 Default: true")
         :boolean)
   (list :trusted-users
         nil
         (format nil
                 "Comma seperated list of users which are trused.~%~
                 This means that if users added to this list send~%~
                 you a file you automatically accept it and save~%~
                 it to the default upload folder. Each user needs~%~
                 to be specified with ip and port in the following~%~
                 notation: username@ip:port~%~
                 for example: pete@192.168.2.100:1234~%~
                 To get the ip and port of a user hover over the User~%~
                 on the Userlist.~%~
                 Default: empty")
         :list
         (lambda ()
           (lodds:get-user-list)))
   (list :blocked-users
         nil
         (format nil
                 "Comma seperated list of users which are not~%~
                 trused. If a User on this list tries to send~%~
                 you a file, he will automatically be blocked.~%~
                 Each user needs to be specified with ip and port~%~
                 in the following notation: username@ip:port~%~
                 for example: pete@192.168.2.100:1234~%~
                 To get the ip and port of a user hover over the~%~
                 User on the Userlist.~%~
                 Default: empty")
         :list
         (lambda ()
           (lodds:get-user-list)))
   (list :client-timeout
         5
         (format nil
                 "If a client does not send a advertise message~%~
                 after the given timeout (seconds), he will be~%~
                 removed. Value between 1 and 3600.~%~
                 Default: 5")
         :integer
         1
         3600)
   (list :advertise-timeout
         1
         (format nil
                 "Timeout between advertisements. Specified~%~
                 in seconds. Value between 1 and 10.~%~
                 Default: 1")
         :integer
         1
         10)
   (list :socket-timeout
         1
         (format nil
                 "Timeout which will be set on sockets, if any~%~
                 socket does not respond after the given timeout~%~
                 it will be closed and the connection aborted.~%~
                 The timeout is specified in seconds.~%~
                 Value between 1 and 30.~%~
                 Default: 1")
         :integer
         1
         30)
   (list :incognito-mode
         nil
         (format nil
                 "When Incognito Mode is on lodds the advertiser~%~
                 will be stopped, this means other clients can not~%~
                 Find you anymore and you cannot share your files~%~
                 them. You are still able to download files from them.~%~
                 Default: false")
         :boolean)
   ;; TODO: QT gui related settings, these should be splitted out to
   ;; lodds-qt package and then be hooked somehow, but i couldn't
   ;; think of any good way to achieve it. Thats why i will just leave
   ;; them here for now.
   (list :minimize-to-tray
         nil
         (format nil
                 "If true Lodds will not Quit when being closed~%~
                 and rather minimize to Tray. Lodds will always~%~
                 quit when 'Quit' on the system tray or Menubar~%~
                 is clicked.~%~
                 Default: false")
         :boolean)
   (list :download-folder
         (format nil "~adownload"
                 (user-homedir-pathname))
         (format nil
                 "Default Download folder, where files get saved.~%~
                 Default: download folder inside home directory.")
         :folder)
   (list :upload-folder
         (format nil "~aupload"
                 (user-homedir-pathname))
         (format nil
                 "Default Upload folder, incomming files will~%~
                 be saved there. If a 'trused-user' is sending~%~
                 a file, its saved there too.~%~
                 Default: upload folder inside home directory")
         :folder)
   (list :resources-folder
         "./res/"
         (format nil
                 "Folder which contains resources for Lodds,~%~
                 like icons, stylesheets, ...~%~
                 Changing this value will have no direct effect,~%~
                 Restart (stop/start) lodds, reload the~%~
                 stylesheet (Lodds -> Reload Stylesheet) and~%~
                 reshare (unshare/share) all folders for changes~%~
                 to take effect~%~
                 Default: ./res/ (res folder inside the current directory)")
         :folder)
   (list :timeout-send-file
         300
         (format nil
                 "Default Timeout in seconds when sending a~%~
                 file. Value between 1 and 3600~%~
                 Default: 300")
         :integer
         1
         3600)
   (list :info-update-interval
         100
         (format nil
                 "Timeout (in milliseconds) between updates~%~
                 on 'info' widget. Value between 50 and 100.000~%~
                 Default: 100")
         :integer
         50
         100000)
   (list :directory-busy-check
         1000
         (format nil
                 "Timeout (in milliseconds) between checks~%~
                 if a directory is busy (This is used to~%~
                 display the Spinner when a directory is~%~
                 busy). Value between 50 and 100.000~%~
                 Default: 1000")
         :integer
         50
         100000)
   (list :log-message-max
         500
         (format nil
                 "Maximum of displayed Log Messages. If a new entry~%~
                 is logged, the oldest (top) will be removed. Value between~%~
                 10 and 10000.~%~
                 Default: 500")
         :integer
         10
         10000)
   (list :show-log-type-color
         t
         (format nil
                 "Set to false to not colorize some log message~%~
                 event types inside the Log Area.~%~
                 Default: true")
         :boolean)
   (list :show-background-color-on-size
         t
         (format nil
                 "Show a background color on sizes depending on the given~%~
                 size. For example bigger files/folders will be colored red,~%~
                 while smaller will be colored in a greenish color.~%~
                 Changing this setting will not have a direct effect,~%~
                 Restarting Lodds (Stop & Start) to build a new tree is~%~
                 needed.~%~
                 Default: true")
         :boolean)))

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
      (format nil "~a is not of expected key type ~a (key: ~a)"
              value
              (get-type key config)
              key)))
  (case (get-type key config)
    (:integer (progn
                (when (< value (get-integer-min key config))
                  (return-from validate-new-entry
                    (format nil "~a is lower then allowed min (~a)"
                            value
                            (get-integer-min key config))))
                (when (> value (get-integer-max key config))
                  (return-from validate-new-entry
                    (format nil "~a is higher then allowed max (~a)"
                            value
                            (get-integer-max key config))))))
    (:selection (let ((valid (get-selection-options key)))
                  (unless (find value valid :test #'equal)
                    (return-from validate-new-entry
                      (format nil "Invalid selection (~a), valid are only: ~a"
                              value valid)))))
    (:folder (unless (lodds.core:directory-exists value)
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
  (with-open-file (stream (lodds.core:escape-wildcards filename)
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
         (old-value (car entry))
         (type (get-type key config)))
    (unless entry
      (return-from update-entry
        (format nil "Unrecognized key ~a" (string-downcase (string key)))))
    (let ((err (validate-new-entry key new-value config)))
      (when err
        (return-from update-entry err))
      (unless (if (eql type :list)
                  (equalp old-value
                          new-value)
                  (equal old-value
                         new-value))
        (setf (car entry) new-value)
        (setf (gethash key config) entry)
        (when push-event-p
          (lodds.event:push-event :config-changed
                                  (list key old-value new-value)))
        nil))))

(defun load-from-file (filename &optional (config (generate-default-config)))
  "returns the config and nil if config file was parsed without errors
  and nil plus a error string describing the error on failure"
  (with-open-file (stream (lodds.core:escape-wildcards filename)
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
                  (if (not (gethash key config))
                      (format t "Config file warning (~a:~a): ~
                                 Unrecognized key: ~a~%"
                              filename
                              line-number
                              (string-downcase (string key)))
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
                                                  err))))))))))
            :finally (return (values config nil))))))

(defun load-default-config-files ()
  "returns two values, the config and nil on success, or nil and a
  error message on failure"
  (let ((config (generate-default-config)))
    (loop :for element :in *load-path*
          :do (when (lodds.core:file-exists element)
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

(defun get-suggestions (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  (let ((fn (fourth (gethash key config))))
    (when fn
      (funcall fn))))
