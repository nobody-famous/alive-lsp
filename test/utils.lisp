(defpackage :alive/test/utils
    (:use :cl)
    (:export :*end-line*

             :create-msg))

(in-package :alive/test/utils)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-msg (content)
    (with-output-to-string (str)
        (format str "Content-Length: ~A~A" (length content) *end-line*)
        (format str "~A" *end-line*)
        (format str "~A" content)))
