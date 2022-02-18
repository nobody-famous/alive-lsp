(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list))

(in-package :alive/symbols)


(defun callable-p (name &optional pkg)
    #+sbcl (alive/sbcl/symbols:callable-p name pkg))


(defun get-lambda-list (fn-name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:get-lambda-list fn-name pkg-name))
