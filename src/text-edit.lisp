(defpackage :alive/text-edit
    (:use :cl)
    (:export :create
             :range
             :text))

(in-package :alive/text-edit)


(defun range (obj)
    (when obj
          (gethash "range" obj)))


(defun text (obj)
    (when obj
          (gethash "text" obj)))


(defun create (&key range text)
    (let ((obj (make-hash-table :test #'equalp)))

        (setf (gethash "range" obj) range)
        (setf (gethash "text" obj) text)

        obj))
