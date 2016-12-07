(in-package #:lodds-qt)

(in-package #:lodds-qt)
(in-readtable :qtools)

(defparameter +log-count+ 1000)

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window start) (q+:make-qpushbutton "Start" main-window))
(define-subwidget (main-window stop) (q+:make-qpushbutton "Stop" main-window))
(define-subwidget (main-window interfaces) (q+:make-qcombobox main-window)
  (q+:add-items interfaces (lodds:get-interfaces))
  (let ((current-interface (lodds:interface lodds:*server*)))
    (if current-interface
        (let ((current-interface-index (q+:find-text interfaces
                                                     current-interface)))
          (when (> current-interface-index 0)
            (q+:set-current-index interfaces current-interface-index)))
        (q+:set-current-index interfaces -1))))

(define-subwidget (main-window list-of-shares) (q+:make-qtreewidget main-window)
  (q+:set-header-labels list-of-shares (list "Name" "Size" "Checksum"))
  (q+:set-alternating-row-colors list-of-shares t)
  (q+:set-animated list-of-shares t)
  (q+:set-items-expandable list-of-shares t)
  (q+:set-expands-on-double-click list-of-shares t)
  (q+:set-style-sheet list-of-shares "QTreeView {
                                        alternate-background-color: #eeeeef;
                                        background-color: #ffffff;
                                      }"))

(define-subwidget (main-window log) (q+:make-qtreewidget main-window)
  (q+:set-header-labels log (list "Event" "Message"))
  (q+:set-alternating-row-colors log t)
  (q+:set-style-sheet log "QTreeView {
                             alternate-background-color: #eeeeef;
                             background-color: #ffffff;
                           }"))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "LODDS")
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner start)
    (q+:add-widget inner stop)
    (q+:add-widget inner interfaces)
    (q+:add-item layout inner))
  (let ((splitter (q+:make-qsplitter)))
     (q+:set-orientation splitter (#_Vertical "Qt"))
    (q+:add-widget splitter list-of-shares)
    (q+:add-widget splitter log)
    (q+:add-widget layout splitter)))

(define-signal (main-window add-entry) (string string string))
(define-signal (main-window remove-entry) (string))
(define-signal (main-window dump-table) ())
(define-signal (main-window add-log-msg) (string string))

(define-slot (main-window start) ()
  (declare (connected start (pressed)))
  ;; TODO: well, this is not final :D
  (lodds.subsystem:start (lodds:get-subsystem :event-queue))
  (lodds.subsystem:start (lodds:get-subsystem :tasker))
  (lodds.subsystem:start (lodds:get-subsystem :listener))
  (lodds.subsystem:start (lodds:get-subsystem :advertiser))
  (lodds.subsystem:start (lodds:get-subsystem :handler))
  (lodds.watcher:share-folder "~/ffff/A/")
  (lodds.watcher:share-folder "~/ffff/B/")
  (lodds.watcher:share-folder "~/ffff/C/"))

(define-slot (main-window stop) ()
  (declare (connected stop (pressed)))
  (lodds.subsystem:stop (lodds:get-subsystem :tasker))
  (lodds.subsystem:stop (lodds:get-subsystem :listener))
  (lodds.subsystem:stop (lodds:get-subsystem :advertiser))
  (lodds.subsystem:stop (lodds:get-subsystem :handler))
  (lodds.watcher:unshare-folder "~/ffff/A/")
  (lodds.watcher:unshare-folder "~/ffff/B/")
  (lodds.watcher:unshare-folder "~/ffff/C/"))

(define-slot (main-window interfaces) ((selected-item string))
  (declare (connected interfaces (current-index-changed string)))
  (lodds:switch-interface selected-item))

(defun add-node (path size checksum get-parent-fn &optional
                                                    (child-count 0)
                                                    (get-child-fn nil)
                                                    (root-p nil))
  "loop over childs with get-child-fn and check if current entry (car
  path) is already present, if so call add-node on that entry
  recursivly, if not add a new entry and then call add-node with the
  new entry. If add-node returns t, the entry was added, otherwise nil
  is returned. The return is used to check if size on parents needs to
  be increased."
  (loop :for i :from 0 :below child-count
        :do (let ((element (funcall get-child-fn i)))
              ;; check if node matches
              (when (string= (car path) (q+:text element 0))
                (unless (cdr path)
                  ;; if there is no path left but we have a
                  ;; matching node, it means that the node
                  ;; we tried to add already exists -> we
                  ;; return nil to indicate an error
                  (return-from add-node nil))
                ;; if add-node was successfull, update size and
                ;; return t
                (when (add-node (cdr path) size checksum
                                (lambda () element)
                                (q+:child-count element)
                                (lambda (place) (q+:child element place)))
                  (q+:set-text element 1
                               (prin1-to-string
                                (+ (parse-integer size)
                                   (parse-integer (q+:text element 1)))))
                  (return-from add-node t))
                ;; if add-child-node was not successfull, just return
                ;; nil
                (return-from add-node nil))))
  ;; when we get down here it means there was no matching
  ;; node, so lets add a new one.
  ;; TODO: fix mem leak, finalize new-item when list is destroyed
  (let ((new-entry (q+:make-qtreewidgetitem (funcall get-parent-fn))))
    (q+:set-text new-entry 0 (car path))
    (q+:set-text new-entry 1 size)
    ;; only add checksums on files, not on folders
    (unless (cdr path)
      (q+:set-text new-entry 2 checksum))
    ;; only if not root
    (unless root-p
      (q+:add-child (funcall get-parent-fn) new-entry))
    ;; if there is path left, add those under the new node. If there
    ;; is no path left it means we reached the last node, so return t
    ;; as we successfully added it. We dont have to update size here,
    ;; since its a new node and it will match the elements size.
    (if (cdr path)
        (add-node (cdr path) size checksum (lambda () new-entry))
        t)))

(defun remove-node (path child-count get-child-fn remove-child-fn)
  "loops over children with get-child-fn, and if path matches
  remove-node is recursivly called on the matching node. Remove node
  will return a Size if a node was successfull removed. This is used to
  updated sizes on parents."
  (loop :for i :from 0 :below child-count
        :do (let ((element (funcall get-child-fn i)))
              ;; check if node matches
              (when (string= (car path) (q+:text element 0))
                ;; check if we have a path left and need to call
                ;; remove-node recursivly
                (if (cdr path)
                    (let ((size (remove-node (cdr path)
                                             (q+:child-count element)
                                             (lambda (place)
                                               (q+:child element place))
                                             (lambda (place)
                                               (parse-integer
                                                (q+:text (q+:take-child element place) 1))))))
                      ;; remove-node will return the size of the
                      ;; removed element, if successfull.
                      (when size
                        ;; If element has no childs left (if the
                        ;; recursive call removed the last child)
                        ;; remove element too. If there are childs
                        ;; left, just update the size.
                        (if (eql 0 (q+:child-count element))
                            (funcall remove-child-fn i)
                            (q+:set-text element 1
                                         (prin1-to-string
                                          (- (parse-integer (q+:text element 1))
                                             size))))
                        ;; return size here, since recursive remove
                        ;; was successfull
                        (return-from remove-node size)))
                    ;; return size (will be returned by
                    ;; remove-child-fn) here, since remove was
                    ;; successfull
                    (return-from remove-node (funcall remove-child-fn i)))))
        ;; return nil (failed) if loop went through and we did not
        ;; find the specified node
        :finally (return nil)))

(define-slot (main-window add-entry) ((path string)
                                      (size string)
                                      (checksum string))
  (declare (connected main-window (add-entry string
                                             string
                                             string)))
  (add-node (cl-strings:split path #\/) size checksum
            (lambda () list-of-shares)
            (q+:top-level-item-count list-of-shares)
            (lambda (place) (q+:top-level-item list-of-shares place))
            t))

(define-slot (main-window remove-entry) ((path string))
  (declare (connected main-window (remove-entry string)))
  (remove-node (cl-strings:split path #\/)
               (q+:top-level-item-count list-of-shares)
               (lambda (place) (q+:top-level-item list-of-shares place))
               (lambda (place) (q+:take-top-level-item list-of-shares place))))

(define-slot (main-window add-log-msg) ((event string)
                                        (msg string))
  (declare (connected main-window (add-log-msg string
                                               string)))
  (let ((new-entry (q+:make-qtreewidgetitem log)))
    (q+:set-text new-entry 0 event)
    (q+:set-text new-entry 1 msg)
    (let* ((scrollbar (q+:vertical-scroll-bar log))
           (position (q+:value scrollbar)))
      (loop :while (> (q+:top-level-item-count log) +log-count+)
            :do (finalize (q+:take-top-level-item log 0)))
      (q+:set-value scrollbar (- position 1)))
    (let* ((item-above (q+:item-above log new-entry))
           (visual-rect (q+:visual-item-rect log item-above)))
      (when (and item-above
                 (< (- (q+:bottom visual-rect)
                       (q+:height (q+:viewport log)))
                    (q+:height visual-rect)))
        (q+:scroll-to-item log new-entry)))))

(defun dump-item (item &optional (depth 0))
  "dumps given item, and all its childs, if it has any. Just for
  debugging"
  (format t "ITEM: ~a~a~%"
          (make-string depth :initial-element #\ )
          (q+:text item 0))
  (loop :for i :from 0 :below (q+:child-count item)
        :do (dump-item (q+:child item i)
                       (+ depth 1))))

(define-slot (main-window dump-table) ()
  (declare (connected main-window (dump-table)))
  (loop :for i :from 0 :below (q+:top-level-item-count list-of-shares)
        :do (dump-item (q+:top-level-item list-of-shares i))))

(defun cb-list-update (main-window event)
  "callback which will be called on a :list-update event"
  (destructuring-bind (name type timestamp changes) event
    (format t "got event-callback from: ~a type: ~a, timestamp: ~a."
            name type timestamp)
    (format t "~{change: ~a~%~}" changes)
    (when (eql type :all)
      (signal! main-window (remove-entry string) name))
    (loop :for (type checksum size path) :in changes
          :do (let ((combined-path (concatenate 'string name path)))
                (if (eql type :add)
                    (signal! main-window
                             (add-entry string string string)
                             combined-path (prin1-to-string size) checksum)
                    (signal! main-window
                             (remove-entry string)
                             combined-path))))))

(defun cb-client-removed (main-window client-name)
  "callback which will get called if a client was removed"
  (signal! main-window (remove-entry string) client-name))

(defun main ()
  (let ((lodds-server lodds:*server*))
    ;; TODO: thats not supposed to be in a CALL-IN-MAIN-THREAD
    (trivial-main-thread:call-in-main-thread
     (lambda ()
       (lodds:with-server lodds-server
         (with-main-window (window (make-instance 'main-window))
           (lodds.event:add-callback :gui (lambda (event)
                                            (cb-list-update window (cdr event)))
                                     :event-type :list-update)
           (lodds.event:add-callback :gui (lambda (event)
                                            (cb-client-removed window (second event)))
                                     :event-type :client-removed)
           (lodds.event:add-callback :gui (lambda (event)
                                            (signal! window
                                                     (add-log-msg string string)
                                                     (format nil "~a" (first event))
                                                     (format nil "~a" (cdr event)))))
           ;; add known clients
           (maphash (lambda (name info)
                      (maphash (lambda  (filename file-info)
                                 (destructuring-bind (checksum size) file-info
                                   (signal! window
                                            (add-entry string string string)
                                            (concatenate 'string name filename)
                                            (prin1-to-string size)
                                            checksum)))
                               (lodds:c-file-table-name info)))
                    (lodds:clients lodds-server)))
         (lodds.event:remove-callback :gui
                                      :event-type :list-update)
         (lodds.event:remove-callback :gui
                                      :event-type :client-removed)
         (lodds.event:remove-callback :gui))))))
