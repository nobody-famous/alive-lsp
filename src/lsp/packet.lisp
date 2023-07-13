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

(defun to-wire (msg)
    (with-output-to-string (str)
        (let* ((payload (json:encode-json-to-string msg)))
            (format str "Content-Length: ~A~A" (length payload) end-line)
            (format str "~A" end-line)
            (format str "~A" payload))))


(defun create-header ()
    (make-instance 'header))
