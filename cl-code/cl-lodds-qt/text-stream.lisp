(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget text-stream (QPlainTextEdit
                            trivial-gray-streams:fundamental-character-output-stream)
  ((buffer-init-size :initform 64
                     :initarg :buffer-init-size)
   (buffer :initform nil)
   (maximum-block-count :initform 0
                        :initarg :maximum-block-count
                        :accessor maximum-block-count)
   (center-on-scroll :initform t
                     :initarg :center-on-scroll
                     :accessor center-on-scroll)
   (read-only :initform t
              :initarg :read-only
              :accessor read-only)
   (html-mode :initform nil
              :initarg :html-mode
              :accessor html-mode)
   (font :initform nil
         :initarg :font
         :accessor font)
   (write-fn)))

(defmethod (setf maximum-block-count) :after (maximum-block-count (stream text-stream))
  (q+:set-maximum-block-count stream maximum-block-count))

(defmethod (setf center-on-scroll) :after (center-on-scroll (stream text-stream))
  (q+:set-center-on-scroll stream center-on-scroll))

(defmethod (setf read-only) :after (read-only (stream text-stream))
  (q+:set-read-only stream read-only))

(defmethod (setf html-mode) :after (html-mode (stream text-stream))
  (setf (slot-value stream 'write-fn)
        (if html-mode
            (lambda (text)
              (q+:append-html stream text))
            (lambda (text)
              (q+:append-plain-text stream text)))))

(defmethod (setf font) :after (font (stream text-stream))
  (q+:set-font stream
               (if (stringp font)
                   (get-font font)
                   font)))

(define-initializer (text-stream setup-widget)
  (setf (html-mode text-stream) html-mode)
  (setf (font text-stream) font)
  (qdoto text-stream
         (q+:set-maximum-block-count maximum-block-count)
         (q+:set-center-on-scroll center-on-scroll)
         (q+:set-read-only read-only)))

(define-finalizer (text-stream cleanup-font)
  (when font
    (finalize font)))

(defmethod trivial-gray-streams:stream-start-line-p ((stream text-stream))
  (if (slot-value stream 'buffer) nil t))

(defmethod trivial-gray-streams:stream-line-column ((stream text-stream))
  (with-slots (buffer) stream
    (when buffer
      (length buffer))))

(defmethod trivial-gray-streams:stream-write-char ((stream text-stream) character)
  (with-slots (buffer write-fn buffer-init-size) stream
    (unless buffer
      (setf buffer (make-array buffer-init-size
                               :element-type 'character
                               :adjustable t
                               :fill-pointer 0)))
    (prog1 character
      (case character
        (#\newline (progn
                     (funcall write-fn (or buffer ""))
                     (setf buffer nil)))
        (t (vector-push-extend character buffer))))))
