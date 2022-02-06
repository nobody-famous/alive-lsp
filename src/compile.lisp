(defpackage :alive/compile
    (:use :cl)
    (:export :file))

(in-package :alive/compile)


(defun file (out-fn path)
    #+sbcl (alive/sbcl/compile:file out-fn path))
