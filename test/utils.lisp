(defpackage :alive/test/utils
    (:use :cl)
    (:export :*end-line*
             :create-msg
             :check-equal
             :stream-from-string))

(in-package :alive/test/utils)


#+win32
(defparameter *end-line* (format nil "~C" #\newline))

#-win32
(defparameter *end-line* (format nil "~C~C" #\return #\newline))


(defun create-msg (content)
    (with-output-to-string (str)
        (format str "Content-Length: ~A~A" (length (flexi-streams:string-to-octets content)) *end-line*)
        (format str "~A" *end-line*)
        (format str "~A" content)))


(defun stream-from-string (str)
    (flexi-streams:make-flexi-stream
        (flexi-streams:make-in-memory-input-stream
            (flexi-streams:string-to-octets str))))


(defun check-equal (obj1 obj2)
    (unless (equalp obj1 obj2)
        (error "Not equal")))