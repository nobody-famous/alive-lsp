(defpackage :alive/location
    (:use :cl)
    (:export :create)
    (:local-nicknames (:range :alive/range)))

(in-package :alive/location)


(defun create (file range)
    (list (cons :uri file)
          (cons :range range)))
