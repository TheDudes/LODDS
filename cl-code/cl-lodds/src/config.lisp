#|

This file contains all configuration related functions and the default
config. A 'config' is basically a hashmap which has the config name as
key and information about it as value.

Each config key exists of at least a value, a description and a type,
see generate-default-list for more info. Some types contain addition
fields, like :integer for example. It contains a minimum and a maximum
value (to verify the new value on change).

|#

(in-package #:lodds.config)

(defun load-path ()
  (list #+os-unix (pathname "/etc/lodds.config")
        #+linux (merge-pathnames (pathname "lodds.config")
                                 (uiop:xdg-config-home))
        (merge-pathnames (pathname ".lodds.config")
                         (user-homedir-pathname))
        (pathname "lodds.config")))

(defvar *color-scanner*
  (cl-ppcre:create-scanner
   "^#([a-f]|[A-F]|[0-9]){6}$")
  "To check if color is correct")

(defun key-to-log-keyword (key)
  (intern (string-upcase (format nil "log-~a-color" key))
          :keyword))

(defun generate-log-color-setting (color key)
  (list (key-to-log-keyword key)
        color
        :color
        (format nil
                "Background Color of ~a~%~
                 Event on log widget when show-log-type-colors~%~
                 is true~%~
                 Default: ~a"
                key color)))

(defun generate-default-list ()
  "Used to generate the default configuration, each element contains
  the configuration key, its default value, a description and the
  type, in that order."
  (list
   (list :interface
         (car (lodds.core:get-interfaces))
         :selection
         (format nil
                 "Interface lodds uses to get his own ip and broadcast~%~
                 address. You have to restart lodds for changes to~%~
                 take effect.~%~
                 Default: The first interface with an ip (excluding loopback)")
         (lambda ()
           (lodds.core:get-interfaces)))
   (list :remember-shares
         nil
         :boolean
         (format nil
                 "If true: shared directories will be rememberd on restart~%~
                 [Not implemented yet]~%~
                 Default: false"))
   (list :port
         4567
         :integer
         (format nil
                 "The port where lodds is listning on for incomming~%~
                 requests. Value between 1024 and 65535.~%~
                 Restart is needed for changes to take effect.~%~
                 Default: 4567")
         1024
         65535)
   (list :broadcast-port
         9002
         :integer
         (format nil
                 "The port where lodds will listen for broadcast~%~
                 messages from other users. This port is also~%~
                 used for advertisements of the client (as expected,~%~
                 i guess). Value between 1024 and 65535.~%~
                 Default: 9002")
         1024
         65535)
   (list :name
         (machine-instance)
         :string
         (format nil
                 "Displayed/broadcasted name of lodds client.~%~
                  Default: OS username"))
   (list :deny-requests
         nil
         :boolean
         (format nil
                 "If set to true all incomming send requests will~%~
                 denied.~%~
                 Default: false"))
   (list :allow-unkown-user-send
         t
         :boolean
         (format nil
                 "If a send request is received lodds tries to~%~
                 figure out which user sent the request (via the~%~
                 ip). If the user does not advertise himself~%~
                 lodds cannot determine the user who send the request~%~
                 since the user is unkown. If allow-unknown-user-send~%~
                 is false, users which are unknown to lodds wont~%~
                 be able to send files.~%~
                 Default: true"))
   (list :trusted-users
         nil
         :list
         (format nil
                 "Comma seperated list of users which are trused.~%~
                 This means that if users added to this list send~%~
                 you a file you automatically accept it and save~%~
                 it to the default upload folder. Each user needs~%~
                 to be specified with ip and port in the following~%~
                 notation: username@ip:port~%~
                 for example: pete@192.168.2.100:1234~%~
                 To get the ip and port of a user hover over the user~%~
                 on the userlist.~%~
                 Default: empty")
         (lambda ()
           (lodds:get-user-list)))
   (list :blocked-users
         nil
         :list
         (format nil
                 "Comma seperated list of users which are not~%~
                 trused. if a user on this list tries to send~%~
                 you a file, he will automatically be blocked.~%~
                 each user needs to be specified with ip and port~%~
                 in the following notation: username@ip:port~%~
                 For example: pete@192.168.2.100:1234~%~
                 To get the ip and port of a user hover over the~%~
                 user on the userlist.~%~
                 Default: empty")
         (lambda ()
           (lodds:get-user-list)))
   (list :user-timeout
         5
         :integer
         (format nil
                 "If a user does not send a advertise message~%~
                 after the given timeout (seconds), he will be~%~
                 removed. Value between 1 and 3600.~%~
                 Default: 5")
         1
         3600)
   (list :advertise-timeout
         1
         :integer
         (format nil
                 "Timeout between advertisements. Specified~%~
                 in seconds. Value between 1 and 10.~%~
                 Default: 1")
         1
         10)
   (list :socket-timeout
         1
         :integer
         (format nil
                 "Timeout which will be set on sockets, if any~%~
                 socket does not respond after the given timeout~%~
                 it will be closed and the connection aborted.~%~
                 The timeout is specified in seconds.~%~
                 Value between 1 and 30.~%~
                 Default: 1")
         1
         30)
   (list :incognito-mode
         nil
         :boolean
         (format nil
                 "When Incognito mode is on lodds the advertiser~%~
                 will be stopped, this means other users can not~%~
                 find you anymore and you cannot share your files~%~
                 them. You are still able to download files from them.~%~
                 Default: false"))
   (list :validate-checksum
         t
         :boolean
         (format nil
                 "Can be unchecked to not validate the checksum of~%~
                 downloaded files. Even if set to true, the checksum~%~
                 will only be validated when the file is getting~%~
                 downloaded from multiple users (folder download,~%~
                 file download with user set to 'Any' or multiple~%~
                 selection download), because if the files gets~%~
                 directly downloaded from a user we dont trust~%~
                 he could just 'fake' the checksum too and the~%~
                 validation would be useless~%~
                 Default: true"))
   (list :fake-checksum
         nil
         :boolean
         (format nil
                 "Can be checked to 'fake' checksums and not calculate~%~
                 them. This can be usefull to share huge files without~%~
                 having to calculate the checksum, which would take~%~
                 a long time. The Downside to this is that a file can~%~
                 not be matched with another file. The downloading user~%~
                 has to disable :validate-checksum, since the checksum he~%~
                 calculates wont match.~%~
                 Default: false"))
   (list :task-cleanup-timeout
         1
         :integer
         (format nil "Time in seconds the task cleaner is executed~%~
                     The task cleaner will look for tasks which are~%~
                     not responding and save them. If the task still~%~
                     exists when he gets executed again he will force~%~
                     quit the task.~%~
                     Default: 1")
         1
         30)
   ;; TODO: QT gui related settings, these should be splitted out to
   ;; lodds-qt package and then be hooked somehow, but i couldn't
   ;; think of any good way to achieve it. Thats why i will just leave
   ;; them here for now.
   (list :minimize-to-tray
         nil
         :boolean
         (format nil
                 "If true lodds will not quit when being closed~%~
                 and rather minimize to tray. Lodds will always~%~
                 quit when 'Quit' on the system tray or menubar~%~
                 is clicked.~%~
                 Default: false"))
   (list :show-log-widget
         nil
         :boolean
         (format nil
                 "If true the log widget will be tabbed on the info~%~
                 Widget at the bottom of the main-window on startup~%~
                 Default: false"))
   (list :download-folder
         (lodds.core:ensure-directory-pathname
          (merge-pathnames (pathname "download")
                           (user-homedir-pathname)))
         :folder
         (format nil
                 "Default download folder, where files get saved.~%~
                 Default: 'download' folder inside home directory."))
   (list :upload-folder
         (lodds.core:ensure-directory-pathname
          (merge-pathnames (pathname "upload")
                           (user-homedir-pathname)))
         :folder
         (format nil
                 "Default upload folder, incomming files will~%~
                 be saved there. If a 'trused-user' is sending~%~
                 a file, its saved there too.~%~
                 Default: upload folder inside home directory"))
   (list :resources-folder
         (lodds.core:ensure-directory-pathname
          (make-pathname :directory '(:relative "res")))
         :folder
         (format nil
                 "Folder which contains resources for lodds,~%~
                 like icons, stylesheets, ...~%~
                 Changing this value will have no direct effect,~%~
                 Restart lodds (Lodds -> Restart), reload the~%~
                 stylesheet (Lodds -> Reload Stylesheet) and~%~
                 reshare (unshare/share) all folders for changes~%~
                 to take effect~%~
                 Default: res/ (res folder inside the current directory)"))
   (list :filetype-icon-folder
         (lodds.core:ensure-directory-pathname
          (make-pathname :directory '(:relative "res" "filetype-icons")))
         :folder
         (format nil
                 "Folder which contains filetype icons which are~%~
                 displayed left of the shared files.~%~
                 Changing this value will have no direct effect,~%~
                 Restart lodds (Lodds -> Restart) for changes~%~
                 to take effect.~%~
                 Default: res/filetype-icons (res/filetype-icons/~%~
                 folder inside the current directory)"))
   (list :show-filetype-icons
         t
         :boolean
         (format nil
                 "Can be unchecked/set to false to not show icons~%~
                 based on file types. Restarting lodds is needed for~%~
                 changes to take effekt~%~
                 Default: true."))
   (list :timeout-send-file
         300
         :integer
         (format nil
                 "Default timeout in seconds when sending a~%~
                 file. Value between 1 and 3600~%~
                 Default: 300")
         1
         3600)
   (list :info-update-interval
         200
         :integer
         (format nil
                 "Timeout (in milliseconds) between updates~%~
                 on 'info' widget. Value between 50 and 100.000~%~
                 Default: 100")
         50
         100000)
   (list :status-update-interval
         1000
         :integer
         (format nil
                 "Interval (in milliseconds) the statusbar and tray~%~
                 tooltip status message refresh.~%~
                 Value between 50 and 100.000~%~
                 Default: 1000")
         50
         100000)
   (list :directory-busy-check
         1000
         :integer
         (format nil
                 "Timeout (in milliseconds) between checks~%~
                 if a directory is busy (This is used to~%~
                 display the spinner when a directory is~%~
                 busy). Value between 50 and 100.000~%~
                 Default: 1000")
         50
         100000)
   (list :log-message-max
         500
         :integer
         (format nil
                 "Maximum of displayed log messages. If a new entry~%~
                 is logged, the oldest (top) will be removed. Value~%~
                 between 10 and 10000.~%~
                 Default: 500")
         10
         10000)
   (list :size-column-width
         60
         :integer
         (format nil
                 "Width in pixel of the column which displays the folder~%~
                 and file sizes.~%~
                 Default: 60")
         0
         5000)
   (list :show-background-color-on-size
         t
         :boolean
         (format nil
                 "Show a background color on sizes depending on the given~%~
                 size. For example bigger files/folders will be colored red,~%~
                 while smaller will be colored in a greenish color.~%~
                 Changing this setting will not have a direct effect,~%~
                 Restarting Lodds (Lodds -> Restart) to build a new~%~
                 tree is needed.~%~
                 Default: true"))
   (list :show-log-type-color
         t
         :boolean
         (format nil
                 "Set to false to not colorize some log message~%~
                 event types inside the Log Area.~%~
                 Default: true"))
   (list :log-default-color
         "#000000"
         :color
         (format nil
                 "Default Background color of Events which did~%~
                 not set any other color~%~
                 Default: #000000"))
   (generate-log-color-setting "#ff79f0" :advertiser)
   (generate-log-color-setting "#c9ff34" :listener)
   (generate-log-color-setting "#1ed760" :task-finished)
   (generate-log-color-setting "#ff0000" :task-failed)
   (generate-log-color-setting "#ff5c14" :task-canceled)
   (generate-log-color-setting "#e6dd21" :info)
   (generate-log-color-setting "#00a102" :user-added)
   (generate-log-color-setting "#ff6a6a" :user-removed)
   (generate-log-color-setting "#639cff" :user-updated)
   (generate-log-color-setting "#888888" :debug)
   (generate-log-color-setting "#ff23db" :watcher)
   (generate-log-color-setting "#ff4e4e" :handler)
   (generate-log-color-setting "#fffd82" :list-update)
   (generate-log-color-setting "#1ef760" :shared-directory)
   (generate-log-color-setting "#FFa720" :unshared-directory)
   (generate-log-color-setting "#ff0000" :directory-error)
   (generate-log-color-setting "#ff0000" :folder-download-error)
   (generate-log-color-setting "#a0d84c" :send-permission)
   (generate-log-color-setting "#35ffeb" :config-changed)))

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
    (:folder (values (lodds.core:ensure-directory-pathname value)
                     nil))
    (:selection (values value nil))
    (:color (values value nil))
    (t (values nil (format nil "Type ~a not recognised" type)))))

(defun validate-new-entry (key value config)
  "Returns nil on success, or a error string on failure"
  (unless (case (get-type key config)
            (:boolean   (or (eql t value)
                            (eql nil value)))
            (:list      (listp value))
            (:string    (stringp value))
            (:integer   (integerp value))
            (:folder    (pathnamep value))
            (:selection (stringp value))
            (:color     (cl-ppcre:scan *color-scanner* value))
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
    (:selection (let ((valid (get-selection-options key config)))
                  (unless (find value valid :test #'equal)
                    (return-from validate-new-entry
                      (format nil "Invalid selection (~a), valid are only: ~a"
                              value valid)))))
    (:folder (cond
               ((not (uiop:directory-pathname-p value))
                (format nil "~a is not a directory pathname" value))
               ((uiop:file-exists-p value)
                (format nil "~a is a file" value))
               (t
                (handler-case
                    (prog1 nil
                      (ensure-directories-exist value))
                  (error (e)
                    (format nil "Could not create directory ~a~%~a"
                            value e)))))))
  nil)

(defun generate-default-config ()
  (let ((config (make-hash-table)))
    (loop :for (key . value) :in (generate-default-list)
          :do (setf (gethash key config)
                    value))
    config))

(defun save-to-file (pathname &optional (config (generate-default-config)))
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (maphash (lambda (key value)
               (destructuring-bind (val type desc &rest ignored) value
                 (declare (ignore ignored))
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
             (let ((err (handler-case (validate-new-entry key (car value) config)
                          (error (e) (format nil "validate error: ~a" e)))))
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
                  (equalp old-value new-value)
                  (equal  old-value new-value))
        (setf (car entry) new-value)
        (setf (gethash key config) entry)
        (when push-event-p
          (lodds.event:push-event :config-changed
                                  key
                                  old-value
                                  new-value))
        nil))))

(defun load-from-file (pathname &optional (config (generate-default-config)))
  "returns the config and nil if config file was parsed without errors
  and nil plus a error string describing the error on failure"
  (with-open-file (stream pathname
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
                              pathname
                              line-number
                              (string-downcase (string key)))
                      (multiple-value-bind (converted-value error)
                          (string-to-type value
                                          (get-type key config))

                        (when error
                          (return-from load-from-file
                            (values nil
                                    (format nil "Config file error (~a:~a): ~a"
                                            pathname
                                            line-number
                                            error))))
                        (let ((err (update-entry key converted-value nil config)))
                          (when err
                            (return-from load-from-file
                              (values nil (format nil "Config file error (~a:~a): ~a"
                                                  pathname
                                                  line-number
                                                  err))))))))))
            :finally (return (values config nil))))))

(defun load-default-config-files ()
  "returns two values, the config and nil on success, or nil and a
  error message on failure"
  (let ((config (generate-default-config)))
    (loop :for element :in (load-path)
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
  (third (gethash key config)))

(defun get-type (key &optional (config (slot-value lodds:*server* 'lodds:settings)))
  "returns the type of a given configuration key"
  (second (gethash key config)))

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

(defun get-log-event-color (event &optional (config (slot-value lodds:*server* 'lodds:settings)))
  (lodds.config:get-value (key-to-log-keyword event)
                          config))
