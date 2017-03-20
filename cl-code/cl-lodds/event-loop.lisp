#|

TODO: Description

ev- functions/methods are running inside the event-loop, do not call
them from any other thread

|#

;; Event-loop methods to retrieve all kinds of data/information

(in-package #:lodds.event-loop)

(defmethod get-load ((event-loop event-loop))
  (with-slots (tasks lock) event-loop
    (let ((loads nil))
      (bt:with-recursive-lock-held (lock)
        (setf loads
              (loop :for task :being :the :hash-value :of tasks
                    :collect (task-load task))))
      (if loads
          (reduce #'+ loads)
          0))))

(defmethod get-task-progresses ((event-loop event-loop))
  "returns a list containing each tasks id, total-load, progress, type
  and info.
  CL-USER> (get-task-progresses tasker)
  => ((\"id1234\" 5903 1324 :send-file \"blub.txt\")
      (\"id3142\" 141204 4214 :get-file \"somename@123.123.1.2:8282\"))
  Each of the inner lists describe a task, with its id, its maximum
  load, already processed load, type and info."
  (with-slots (tasks lock) event-loop
    (bt:with-recursive-lock-held (lock)
      (loop :for task :being :the :hash-value :of tasks
            :collect (with-slots (id total-load bytes-transfered) task
                       (list id
                             total-load
                             bytes-transfered
                             (type-of task)
                             (task-info task)))))))

(defmethod get-task-count ((event-loop event-loop))
  (hash-table-count (slot-value event-loop 'tasks)))

(defun get-task-by-id (task-id &optional (event-loop (lodds:get-event-loop)))
  "Returns task with given id, nil if task is not found"
  (gethash task-id (slot-value event-loop 'tasks)))

(defmacro with-event-loop ((event-loop) &body body)
  `(lparallel.queue:push-queue (lambda () ,@body)
                               (slot-value ,event-loop 'queue)))

;; initialize-instance for each task to set the id on init

(defmethod initialize-instance :after ((task task) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (event-loop id) task
    (with-slots (lock tasks id-counter) event-loop
      (bt:with-lock-held (lock)
        (setf id (format nil "id-~a" (incf id-counter)))
        (setf (gethash id tasks) task)))))

;; helper functions

(defun on-task-error (task message ev)
  (ev-task-cleanup task ev)
  (lodds.event:push-event :error message ev))

;; print-object functions for all tasks

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~a"
            (task-id task))))

(defmethod print-object ((task task-request-file) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id checksum start end) task
      (format stream "~a :checksum ~a :start ~:d :end ~:d"
              id
              (format nil "~a..." (subseq checksum 0 7))
              start
              end))))

(defmethod print-object ((task task-request-send-permission) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id total-load timeout filename) task
      (format stream "~a :size ~a :timeout ~a :filename ~a"
              id
              (lodds.core:format-size total-load)
              timeout
              filename))))

(defmethod print-object ((task task-get-file-from-user) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id local-file-path total-load user checksum) task
      (format stream "~a :local-file-path ~a size: ~a :user ~a :checksum ~a"
              id
              local-file-path
              (lodds.core:format-size total-load)
              user
              checksum))))

(defmethod print-object ((task task-get-file-from-users) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id local-file-path part-size total-load checksum) task
      (format stream "~a :local-file-path ~a size: ~a :part-size ~a :checksum ~a"
              id
              local-file-path
              (lodds.core:format-size total-load)
              (lodds.core:format-size part-size)
              checksum))))

(defmethod print-object ((task task-send-file) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id ip filename timeout total-load) task
      (format stream "~a filename: ~a timeout: ~a size: ~a"
              id
              filename
              timeout
              (lodds.core:format-size total-load)))))

(defmethod print-object ((task task-get-folder) stream)
  (print-unreadable-object (task stream :type t)
    (with-slots (id items items-done remote-path) task
      (format stream "~a items: ~a items-done: ~a folder: ~a"
              id
              (length items)
              (length items-done)
              remote-path))))

;; task-load methods

(defgeneric task-load (task)
  (:documentation "Method returning the load the given task
  produces"))

(defmethod task-load ((task task))
  (with-slots (total-load bytes-transfered) task
    (- total-load bytes-transfered)))

;; ev-task-cleanup methods

(defgeneric ev-task-cleanup (task err)
  (:documentation "Closes open socket and file streams. Will be called
  once a task finishes."))

(defmethod ev-task-cleanup ((task task) err)
  (with-slots (id socket file-stream event-loop canceled on-cancel
               on-finish on-error) task
    (remhash id (slot-value event-loop 'tasks))
    (when (and socket
               (not (as:socket-closed-p socket)))
      (as:close-socket socket)
      (setf socket nil))
    (when file-stream
      (close file-stream)
      (setf file-stream nil))
    ;; check if the error was raised due to a task-cancel
    ;; (canceled would be set to t then)
    (cond
      (canceled
       (when on-cancel
         (funcall on-cancel))
       (lodds.event:push-event :task-canceled
                               id task))
      (err
       (when on-error
         (funcall on-error))
       (lodds.event:push-event :task-failed
                               id task err))
      (t
       (when on-finish
         (funcall on-finish))
       (lodds.event:push-event :task-finished
                               id task)))))

;; ev-task-init methods to initialize the tasks

(defgeneric ev-task-init (task)
  (:documentation "Method which is run to initialize the task, it will
  open up file streams, set filenames etc. Returns t on succes and nil
  on failure."))

(defmethod ev-task-init :around ((task task))
  (handler-case
      (progn
        (call-next-method)
        (setf (slot-value task 'initialized-p) t))
    (error (err)
      (lodds.event:push-event :error
                              "Could not initialize Task"
                              err)
      (ev-task-cleanup task err)
      nil)))

(defmethod ev-task-init ((task task-request-file))
  (with-slots (checksum start end filename file-stream total-load
               bytes-transfered)
      task
    ;; set filename
    (let ((name (lodds.watcher:get-file-info checksum)))
      (unless name
        (error "Requested file not found"))
      (setf filename name))
    ;; open local file stream
    (setf file-stream
          (open (lodds.core:escape-wildcards filename)
                :direction :input
                :element-type '(unsigned-byte 8)))
    (let ((size (- end start))
          (file-size (file-length file-stream)))
      ;; do some checks if request would error
      (cond
        ((< size 0) (error "Requested size < 0"))
        ((> size file-size) (error "Requested size > file-length"))
        ((> end file-size) (error "Requested end > file-length")))
      ;; setup load and max-load
      (setf total-load size)
      ;; move to right file position
      (unless (eql start 0)
        (file-position file-stream start)))))

(defmethod ev-task-init ((task task-request-send-permission))
  (with-slots (file-stream socket filename) task
    (setf file-stream (open (lodds.core:escape-wildcards filename)
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8)))
    (setf (as:socket-data socket) task)
    (as:write-socket-data socket
                          (flexi-streams:string-to-octets
                           (lodds.low-level-api:format-respond-send-permission))
                          ;; :read-cb
                          ;; (lambda (socket data)
                          ;;   (declare (ignore socket))
                          ;;   (lodds.event:push-event :info "override")
                          ;;   (ev-received-data task data))
                          ;; :event-cb
                          ;; (lambda (ev)
                          ;;   (on-task-error task
                          ;;                  "init-task-request-send-permission"
                          ;;                  ev))
                          )))

(defmethod ev-task-init ((task task-get-file-from-user))
  (with-slots (user local-file-path checksum file-stream total-load) task
    (let ((size (car (lodds:get-file-info checksum user))))
      (if size
          (setf total-load size)
          (error "File not found"))
      (setf file-stream
            (open (lodds.core:escape-wildcards local-file-path)
                  :direction :output
                  :if-does-not-exist :create
                  :if-exists :supersede
                  :element-type '(unsigned-byte 8))))))

(defmethod ev-task-init ((task task-get-file-from-users))
  (with-slots (total-load part-size checksum file-stream local-file-path) task
    (let ((size (third (first (lodds:get-file-info checksum)))))
      (unless size
        (error "File not found"))
      (setf file-stream
            (open (lodds.core:escape-wildcards local-file-path)
                  :direction :output
                  :if-does-not-exist :create
                  :if-exists :supersede
                  :element-type '(unsigned-byte 8)))
      (setf total-load size)
      (setf part-size
            (let ((64mb (ash 1 26))
                  (32mb (ash 1 25)))
              (if (> size 64mb)
                  (ash size -4) ;; divide by 16 (split up into 16 parts)
                  (if (> size 32mb)
                      (ash size -3) ;; divide by 8 (split up into 8 parts)
                      ;; if file is smaller than 32 mb just
                      ;; download it in one go
                      size)))))))

(defmethod ev-task-init ((task task-get-folder))
  (with-slots (user remote-path items total-load) task
    (setf items (lodds:get-folder-info remote-path user))
    (let ((size (reduce #'+ items :key #'third)))
      (setf total-load size))))

(defmethod ev-task-init ((task task-send-file))
  (with-slots (filename file-stream)
      task
    (setf file-stream
          (open (lodds.core:escape-wildcards filename)
                :direction :input
                :element-type '(unsigned-byte 8)))))

;; Helper fn writing data from the tasks file-stream to the socket

(defmethod ev-write-to-socket ((task task))
  (with-slots (socket bytes-transfered total-load file-stream buffer)
      task
    (let* ((outstanding-bytes (- total-load bytes-transfered))
           (buffer-size (length buffer))
           (last-part-p (>= buffer-size
                            outstanding-bytes)))
      (when last-part-p
        (setf buffer-size outstanding-bytes
              buffer (make-array outstanding-bytes
                                 :element-type '(unsigned-byte 8))))
      (read-sequence buffer file-stream)
      (as:write-socket-data socket
                            buffer
                            :write-cb
                            (lambda (socket)
                              (declare (ignore socket))
                              ;; will be called once a chunk has been written
                              (incf bytes-transfered buffer-size)
                              (if last-part-p
                                  (ev-task-cleanup task nil)
                                  (ev-write-to-socket task)))
                            :event-cb
                            (lambda (ev)
                              (if last-part-p
                                  (ev-task-cleanup task nil)
                                  (on-task-error task
                                                 "ev-write-to-socket"
                                                 ev)))))))

;; ev-task-run methods to run a task

(defgeneric ev-task-run (task)
  (:documentation "Will run the task until it finishes, fails or gets
  canceled"))

(defmethod ev-task-run :around ((task task))
  ;; run the task when state is normal
  (with-slots (id canceled initialized-p) task
    (when (and (not initialized-p)
               (not (ev-task-init task)))
      ;; return if the task was not initialized and we where not
      ;; able to initialize it
      (return-from ev-task-run))
    (handler-case
        (call-next-method)
      (error (err)
        (ev-task-cleanup task err)))))

(defmethod ev-task-run ((task task-request-file))
  (ev-write-to-socket task))

(defmethod ev-task-run ((task task-get-file-from-user))
  (with-slots (socket user file-stream bytes-transfered total-load checksum) task
    (lodds.core:split-user-identifier (name ip port) user
      (as:tcp-connect ip port
                      (lambda (socket data)
                        (write-sequence data file-stream)
                        (incf bytes-transfered (length data))
                        (when (>= bytes-transfered total-load)
                          (close file-stream)
                          (when (not (as:socket-closed-p socket))
                            (as:close-socket socket))
                          (when (> bytes-transfered total-load)
                            (format t
                                    "Error: received-data (task-request-send-permission): ~
                                    bytes-transfered (~a) > total-load (~a)"
                                    bytes-transfered total-load))))
                      :data (lodds.low-level-api:format-get-file checksum 0 total-load)
                      :connect-cb
                      (lambda (sock)
                        (setf socket sock))
                      :event-cb
                      (lambda (ev)
                        (on-task-error task
                                       "task-get-file-from-user"
                                       ev))
                      :read-timeout 1
                      :write-timeout 1))))

(defmethod ev-task-run ((task task-get-file-from-users))
  (with-slots (total-load checksum digester current-part part-size
               part-bytes-transfered local-file-path file-stream
               bytes-transfered socket current-user) task
    (labels ((get-data ()
               (lodds.low-level-api:format-get-file
                checksum
                bytes-transfered
                (let ((end-next-part (* (+ 1 current-part)
                                        part-size)))
                  (if (> total-load end-next-part)
                      end-next-part
                      (progn
                        (setf part-size
                              (- total-load
                                 bytes-transfered))
                        total-load)))))
             (read-cb (socket data)
               (let ((transfered (length data)))
                 (incf bytes-transfered transfered)
                 (incf part-bytes-transfered transfered))
               (when (or (> part-bytes-transfered part-size)
                         (> bytes-transfered total-load))
                 (ev-task-cleanup task "Too much input"))
               (write-sequence data file-stream)
               (when digester
                 (ironclad:update-digest digester data))
               (if (eql total-load bytes-transfered)
                   (cond
                     ;; if there is no digester
                     ;; (:validate-checksum is false)
                     ;; just finish the task
                     ((not digester)
                      (ev-task-cleanup task nil))
                     ;; if there is a digester, check if the checksum matches
                     ((string= checksum
                               (ironclad:byte-array-to-hex-string
                                (ironclad:produce-digest digester)))
                      (ev-task-cleanup task nil))
                     ;; if the checksum did not match, delete the downloaded file
                     ;; and throw a error
                     (t (progn
                          (delete-file (lodds.core:escape-wildcards local-file-path))
                          (ev-task-cleanup task "Checksum validation Failed"))))
                   (when (eql part-bytes-transfered part-size)
                     (incf current-part)
                     (setf part-bytes-transfered 0)
                     (as:close-socket socket)
                     (get-next-part))))
             (get-next-part ()
               (if (not (setf current-user (lodds:find-best-user checksum)))
                   (ev-task-cleanup task "Could not find user who shares the given file")
                   (lodds.core:split-user-identifier (name ip port t) current-user
                     (as:tcp-connect ip port
                                     #'read-cb
                                     :data (get-data)
                                     :connect-cb
                                     (lambda (sock)
                                       (setf socket sock))
                                     :event-cb
                                     (lambda (ev)
                                       (on-task-error task
                                                      "task-get-file-from-users"
                                                      ev))
                                     :read-timeout 1
                                     :write-timeout 1)))))
      (get-next-part))))

(defmethod ev-task-run ((task task-get-folder))
  (with-slots (local-path remote-path remote-root
               items items-done user state info id
               bytes-transfered event-loop
               current-task)
      task
    (if (not items)
        (progn
          (ev-task-cleanup task nil)
          (lodds.event:push-event :info
                                  (format nil "folder ~a sucessfully downloaded to ~a"
                                          remote-path
                                          local-path)))
        (destructuring-bind (file checksum size) (pop items)
          (setf items-done (append (list (list file checksum size))
                                   items-done))
          (unless checksum
            ;; the file might have changed, if so we should be able to
            ;; get the checksum from remote-path again, if this also
            ;; fails checksum will just be nil, so the following if
            ;; will catch that.
            (setf checksum (lodds:get-checksum-from-path remote-path user)))
          ;; remove size from load since the new task will add it again
          (incf bytes-transfered size)
          (let* ((retry-fn
                   (lambda ()
                     (let ((failed-file (pop items-done)))
                       ;; add item back onto list
                       (setf items
                             (append (list failed-file)
                                     items))
                       (decf bytes-transfered (third failed-file))
                       (with-event-loop (event-loop)
                         (ev-task-run task)))))
                 (skip-fn
                   (lambda ()
                     (with-event-loop (event-loop)
                       (ev-task-run task))))
                 (abort-fn
                   (lambda ()
                     (with-event-loop (event-loop)
                       (ev-task-cleanup task "Aborted"))))
                 (on-error
                   (lambda ()
                     (if (lodds.event:callback-exists-p :folder-download-error)
                         (lodds.event:push-event :folder-download-error
                                                 remote-path
                                                 file
                                                 retry-fn
                                                 skip-fn
                                                 abort-fn)
                         ;; just skip the file
                         (funcall skip-fn)))))
            (if (not checksum)
                (funcall on-error)
                (let ((local-file-path (concatenate 'string
                                                    local-path
                                                    (subseq file (length remote-root)))))
                  (lodds.core:escaped-ensure-directories-exist
                   local-file-path)
                  (let ((task (make-instance 'task-get-file-from-users
                                             :event-loop event-loop
                                             :checksum checksum
                                             :local-file-path local-file-path
                                             ;; resubmit current task-get-folder when file
                                             ;; download is complete
                                             :on-finish skip-fn
                                             :on-error on-error
                                             :on-cancel on-error)))
                    (setf current-task task)
                    (ev-task-run task)))))))))

(defmethod ev-task-run ((task task-send-file))
  (with-slots (user timeout filename bytes-transfered
               total-load socket file-stream)
      task
    (lodds.core:split-user-identifier (name ip port t) user
      (as:tcp-connect ip port
                      (lambda (socket data)
                        (if (string= (format nil "OK~c" #\newline)
                                     (flexi-streams:octets-to-string data))
                            (progn
                              (format t "Got the Ok~%")
                              (as:set-socket-timeouts socket 4 4)
                              (ev-write-to-socket task))
                            (ev-task-cleanup task "Did not send a OK")))
                      :data
                      (lodds.low-level-api:format-get-send-permission
                       total-load
                       timeout
                       (file-namestring filename))
                      :read-timeout timeout
                      :write-timeout timeout
                      :event-cb
                      (lambda (ev)
                        (on-task-error task
                                       "task-send-file init"
                                       ev))
                      :connect-cb
                      (lambda (sock)
                        (setf socket sock))))))

;; send-permission cb, will be called when data is received

(defmethod ev-received-data ((task task-request-send-permission) data)
  (with-slots (socket bytes-transfered total-load file-stream) task
    (incf bytes-transfered (length data))
    (write-sequence data file-stream)
    (cond
      ((> bytes-transfered total-load) (ev-task-cleanup task "Too much data"))
      ((= bytes-transfered total-load) (ev-task-cleanup task nil)))))

;; task-cancel methods

(defgeneric task-cancel (task)
  (:documentation "can be used to cancel a running task"))

(defmethod task-cancel ((task task))
  (with-slots (canceled event-loop) task
    (setf canceled t)
    (with-event-loop (event-loop)
      (ev-task-cleanup task nil))))

(defmethod task-cancel ((id string))
  (let ((task (get-task-by-id id)))
    (when task
      (task-cancel task))))

(defmethod task-cancel ((task task-get-folder))
  (with-slots (current-task) task
    (when current-task
      (task-cancel current-task)))
  (call-next-method))

;; task-info methods to retrieve a information string from a given task

(defgeneric task-info (task)
  (:documentation "Returns a information string about a given task"))

(defmethod task-info ((task task))
  "TODO: set default task info")

(defmethod task-info ((id string))
  (let ((task (get-task-by-id id)))
    (when task
      (task-info task))))

(defmethod task-info ((task task-request-file))
  (with-slots (user total-load filename) task
    (format nil "[Upload] (~a - ~a): ~a"
            user
            (lodds.core:format-size total-load)
            filename)))

(defmethod task-info ((task task-request-send-permission))
  (with-slots (filename users) task
    (format nil "[Request] ~a: ~a" users filename)))

(defmethod task-info ((task task-get-file-from-user))
  (with-slots (local-file-path user) task
    (format nil "[Download] (~a): ~a "
            user
            local-file-path)))

(defmethod task-info ((task task-get-file-from-users))
  (with-slots (current-user local-file-path) task
    (format nil "[Download] (~a): ~a"
            (or current-user "none")
            local-file-path)))

(defmethod task-info ((task task-get-folder))
  (with-slots (items items-done) task
    (let* ((left (length items))
           (done (length items-done))
           (total (+ left done)))
      (format nil "[Folder Download] (File ~a/~a): ~a"
              done
              total
              (or (car (car items-done)) "none")))))

;; event-loop callbacks on given requests

(defun ev-file-request (event-loop socket checksum start end)
  "Function which gets called by the eventloop when a user requests a
file"
  (ev-task-run (make-instance 'task-request-file
                              :event-loop event-loop
                              :socket socket
                              :checksum checksum
                              :start start
                              :end end)))

(defun ev-info-request (socket timestamp)
  "Function which gets called by the eventloop when a user requests
info"
  (as:write-socket-data socket
                        (flex:string-to-octets
                         (apply #'lodds.low-level-api:format-respond-info
                                (lodds:generate-info-response timestamp)))
                        :write-cb (lambda (socket)
                                    (as:close-socket socket))
                        :event-cb (lambda (ev)
                                    (lodds.event:push-event :error
                                                            "On ev-info-request"
                                                            ev))))

(defun update-user (user update)
  (let ((user-info (lodds:get-user-info user)))
    (when user-info
      (with-slots ((name lodds:c-name)
                   (last-change lodds:c-last-change)
                   (table-hash lodds:c-file-table-hash)
                   (table-name lodds:c-file-table-name))
          user-info
        (destructuring-bind (type timestamp changes) update
          (when (eql type :all)
            (setf table-hash (make-hash-table :test 'equal)
                  table-name (make-hash-table :test 'equal)))
          (loop :for (typ cs size name) :in changes
                :if (eql typ :add)
                :do (let ((val (gethash cs table-hash)))
                      (unless (find name val :test #'string=)
                        (setf (gethash cs table-hash)
                              (cons name val)))
                      (setf (gethash name table-name)
                            (list cs size)))
                :else
                :do (let ((new-val (remove name (gethash cs table-hash)
                                           :test #'string=)))
                      (if new-val
                          (setf (gethash cs table-hash)
                                new-val)
                          (remhash cs table-hash))
                      (remhash name table-name)))
          (setf last-change timestamp)
          (lodds.event:push-event :list-update
                                  name
                                  type
                                  timestamp
                                  changes))))))

(defun ev-update-user-info (user)
  (let ((user-info (lodds:get-user-info user)))
    (when user-info
      (with-slots ((ip lodds:c-ip)
                   (port lodds:c-port)
                   (lock lodds:c-lock)
                   (ts lodds:c-last-change))
          user-info
        (when (bt:acquire-lock lock nil)
          (let ((data-unparsed (make-string 0))
                (header nil)
                (count 0)
                (lines-parsed 0)
                (changes (list)))
            (as:tcp-connect ip port
                            (lambda (socket data)
                              (let ((lines (cl-strings:split
                                            (flexi-streams:octets-to-string data
                                                                            :external-format :utf8)
                                            #\linefeed)))
                                (unless header
                                  (if (cl-ppcre:scan lodds.low-level-api::*info-head-scanner* (car lines))
                                      (destructuring-bind (type timestamp cnt) (cl-strings:split (car lines))
                                        (setf count (parse-integer cnt))
                                        (setf header
                                              (list (cond
                                                      ((equal type "all") :all)
                                                      ((equal type "upd") :upd))
                                                    (parse-integer timestamp)))
                                        (setf lines (cdr lines)))
                                      (progn
                                        (lodds.event:push-event :error "Header line did not match")
                                        (bt:release-lock lock)
                                        (as:close-socket socket))))
                                ;; concatenate last message with current first line
                                (setf (car lines)
                                      (concatenate 'string data-unparsed (car lines)))
                                (setf data-unparsed (car (last lines)))
                                (setf lines (butlast lines))
                                (loop :for line :in lines
                                      :do
                                      (if (> (incf lines-parsed) count)
                                          (progn
                                            (lodds.event:push-event :error "He wants to send more than spec.")
                                            (bt:release-lock lock)
                                            (as:close-socket socket))
                                          (if (cl-ppcre:scan lodds.low-level-api::*info-body-scanner*
                                                             line)
                                              (destructuring-bind (type checksum size . name)
                                                  (cl-strings:split line)
                                                (push (list (cond
                                                              ((equal type "add") :add)
                                                              ((equal type "del") :del))
                                                            checksum
                                                            (parse-integer size)
                                                            (cl-strings:join name :separator " "))
                                                      changes))
                                              (progn
                                                (lodds.event:push-event :error "Body line did not match")
                                                (bt:release-lock lock)
                                                (as:close-socket socket)))))))
                            :data (lodds.low-level-api:format-get-info ts)
                            :event-cb
                            (lambda (ev)
                              (bt:release-lock lock)
                              (typecase ev
                                (as:socket-eof () ;; done reading, can update
                                 (update-user user (append header
                                                           (list (reverse changes)))))
                                (t (lodds.event:push-event :error
                                                           "ev-request-info"
                                                           ev))))
                            :read-timeout 1
                            :write-timeout 1)))))))

(defun ev-send-permission-request (event-loop socket size timeout filename)
  "Function which gets called by the eventloop when a user sends a
send-permission request"
  (let* ((users (lodds:get-user-by-ip #(192 168 2 116)))
         (action
           ;; get type of action
           (cond
             ((lodds.config:get-value :deny-requests)
              :blocked)
             ((null users)
              (if (lodds.config:get-value :allow-unkown-user-send)
                  :ask
                  :unknown))
             ((intersection users (lodds.config:get-value :blocked-users)
                            :test #'equal)
              :deny)
             ((subsetp users (lodds.config:get-value :trusted-users)
                       :test #'equal)
              :accept)
             (t :ask))))
    (when (and (eql action :ask)
               (not (lodds.event:callback-exists-p :send-permission)))
      (setf action :no-callback))
    (case action
      (:blocked
       (lodds.event:push-event
        :send-permission
        "received and denied (deny requests true)"))
      ((:accept :ask)
       (let ((task (make-instance 'task-request-send-permission
                                  :event-loop event-loop
                                  :users (or users (list "unknown"))
                                  :socket socket
                                  :total-load size
                                  :timeout timeout
                                  :filename (format nil "~a~a"
                                                    (lodds.core:add-missing-slash
                                                     (lodds.config:get-value :upload-folder))
                                                    filename))))
         (case action
           (:accept (progn
                      (ev-task-init task)
                      (lodds.event:push-event
                       :send-permission
                       (format nil "accepted (user ~{~a~^and~} trusted)"
                               users))))
           (:ask (progn
                   (as:set-socket-timeouts socket timeout timeout)
                   (lodds.event:push-event :send-permission
                                           (task-id task)
                                           filename
                                           timeout
                                           (list "TODO@0.0.0.0:11111")
                                           size
                                           (lambda ()
                                             (with-event-loop (event-loop)
                                               (ev-task-cleanup task nil)))
                                           (lambda (&optional new-filename)
                                             (when new-filename
                                               (setf (slot-value task 'filename)
                                                     new-filename))
                                             (with-event-loop (event-loop)
                                               (ev-task-init task)))))))))
      (:deny
       (lodds.event:push-event
        :send-permission
        (format nil "received and denied (user ~{~a~^and~} blocked)"
                users)))
      (:no-callback
       (lodds.event:push-event
        :send-permission
        "received and denied (blocking send-permissions)"))
      (:unknown
       (lodds.event:push-event
        :send-permission
        "received and denied (blocking receiving files from unkown users)"))
      (t (lodds.event:push-event
          :send-permission
          (format nil "Error: unexpected action: ~a" action))))))

(defun ev-new-connection (event-loop socket data)
  "Function which gets called by the event-loop when a new connection
is established"
  (multiple-value-bind (err request) (lodds.low-level-api:parse-request data)
    (if (> err 0)
        (progn
          (lodds.event:push-event :error
                                  (format nil "low level api returned ~a on parse-request"
                                          err))
          (as:close-socket socket))
        (case (car request)
          (:file (apply 'ev-file-request event-loop socket (cdr request)))
          (:info (apply 'ev-info-request socket (cdr request)))
          (:send-permission (apply 'ev-send-permission-request event-loop socket (cdr request)))))))

;; event-loop initialization

(defun ev-init-handler (event-loop)
  "Initializes the Handler, which accepts incomming connections"
  (as:tcp-server nil
                 (lodds.config:get-value :port)
                 (lambda (socket data)
                   (let ((type (as:socket-data socket)))
                     (cond
                       ((eql type nil)
                        (progn
                          (setf (as:socket-data socket) :known)
                          (ev-new-connection event-loop socket data)))
                       ((eql (type-of type)
                             'task-request-send-permission)
                        (progn
                          (lodds.event:push-event :info "Nope, iam sry :(")
                          (ev-received-data type data)))
                       ((eql (as:socket-data socket) :known)
                        (progn
                          (format t "Received data on :known socket, closing~%")
                          (as:close-socket socket)))
                       (t (progn
                            (format t "Socket Data unknown, closing~%")
                            (as:close-socket socket))))))
                 :event-cb
                 (lambda (ev)
                   (lodds.event:push-event :error
                                           "On ev-init-handler"
                                           ev))))

(defun ev-idle (queue)
  "will be called on every event-loop iteration and call all functions
which have been pushed onto the queue. This enables calling functions
inside the event loop."
  (loop :while (not (lparallel.queue:queue-empty-p queue))
        :do (funcall (lparallel.queue:pop-queue queue))))

(defun ev-init-advertiser ()
  (labels ((send ()
              (as:delay
                (lambda ()
                  (lodds.advertiser:send-advertise)
                  ;; just delay another send
                  (send))
                :event-cb (lambda (ev)
                            (lodds.event:push-event :error
                                                    "On ev-init-advertiser"
                                                    ev))
                :time (lodds.config:get-value :advertise-timeout))))
    (send)))

(defun ev-init (event-loop)
  "Initializes the event-loop"
  (as:idle (lambda ()
             (ev-idle (slot-value event-loop 'queue))))
  (ev-init-handler event-loop)
  (ev-init-advertiser))

(defun start ()
  (let ((ev-loop (lodds:get-event-loop)))
    (with-slots (thread alive-p) ev-loop
      (unless alive-p
        (setf thread
              (bt:make-thread
               (lambda ()
                 (setf alive-p t)
                 (as:start-event-loop
                  (lambda () (ev-init ev-loop)))
                 (setf alive-p nil))
               :name "Lodds - Event Loop"))))))

(defun stop ()
  (with-event-loop ((lodds:get-event-loop))
    (as:exit-event-loop)))
