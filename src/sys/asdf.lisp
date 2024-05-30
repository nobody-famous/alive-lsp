(defpackage :alive/sys/asdf
    (:use :cl)
    (:export :list-all))

(in-package :alive/sys/asdf)


(defun list-all ()
    (mapcar #'string-downcase (asdf:registered-systems)))
