(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p))

(in-package :alive/symbols)


(defun callable-p (name)
    #+sbcl (alive/sbcl/symbols:callable-p name))
