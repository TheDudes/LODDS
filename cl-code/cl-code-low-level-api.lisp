;;;; cl-code-low-level-api.lisp

(in-package #:cl-code-low-level-api)

(defun send-advertise (broadcast-stream ad-info))
(defun read-advertise (broadcast-stream ad-info))

(defun get-info-all (socket-stream))
(defun get-info-up (socket-stream timestamp))
(defun get-info-load (socket-stream))
(defun get-file (socket-stream))
(defun get-send-permission (socket-stream checksum start end))

(defun respond-info-all (socket-stream timestamp file-infos))
(defun respond-info-up (socket-stream timestamp file-infos))
(defun respond-info-load (socket-stream load))
(defun respond-file (socket-stream file-stream start end))
(defun respond-send-permission (socket-stream file-stream size))

(defun handle-info-all (socket-stream file-infos))
(defun handle-info-up (socket-stream file-infos))
(defun handle-info-load (socket-stream load))
(defun handle-file (socket-stream file-stream size))
(defun handle-send-permission (socket-stream timeout file-stream))
