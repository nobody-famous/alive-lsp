(defpackage :alive/file
    (:use :cl)
    (:export :do-compile
             :do-load))

(in-package :alive/file)


(defun do-compile (path out-fn)
    (error "do-compile not done yet"))


(defun do-load (path out-fn)
    #+sbcl (alive/sbcl/file:do-load path out-fn))
