(defpackage :alive/lsp/packet
    (:use :cl)
    (:export :content-length
             :create-header
             :packet
             :payload
             :to-wire))

(in-package :alive/lsp/packet)


(defclass header ()
        ((content-length :accessor content-length
                         :initform nil
                         :initarg :content-length)))


#+win32
(defvar end-line (format nil "~C" #\newline))

#-win32
(defvar end-line (format nil "~C~C" #\return #\newline))


(defun string-to-bytes (str)
    (flexi-streams:string-to-octets str :external-format :utf-8))


(defun number-to-bytes (num)
    (string-to-bytes (princ-to-string num)))


(defun to-wire (msg)
    (let* ((payload (json:encode-json-to-string msg))
           (bytes (flexi-streams:string-to-octets payload :external-format :utf-8)))
        (concatenate 'vector
            (string-to-bytes "Content-Length: ")
            (number-to-bytes (length bytes))
            #(13 10)
            #(13 10)
            bytes)))


(defun create-header ()
    (make-instance 'header))
