(defpackage :alive/compile
    (:use :cl)
    (:export :file)
)

(in-package :alive/compile)


(defun file (out-fn path)
    #+sbcl (alive/compile/compat:file out-fn path)
)
