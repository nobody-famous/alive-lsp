(defpackage :alive/compile-message
    (:use :cl)
    (:export :create
             :get-message))

(in-package :alive/compile-message)


(defun get-message (obj)
    (when obj
          (gethash "message" obj)))


(defun create (&key severity location message)
    (let ((obj (make-hash-table :test #'equalp)))

        (setf (gethash "severity" obj) severity)
        (setf (gethash "location" obj) location)
        (setf (gethash "message" obj) message)

        obj))
