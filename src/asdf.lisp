(defpackage :alive/asdf
    (:use :cl)
    (:export :list-systems))

(in-package :alive/asdf)


(defun list-systems ()
    (mapcar #'string-downcase (asdf:registered-systems)))
