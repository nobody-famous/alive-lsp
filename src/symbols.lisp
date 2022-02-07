(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p))

(in-package :alive/symbols)


(defun callable-p (name &optional pkg)
    #+sbcl (alive/sbcl/symbols:callable-p name pkg))
