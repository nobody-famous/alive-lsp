(defpackage :alive/sys/eval
    (:use :cl)
    (:export :eval-fn))

(in-package :alive/sys/eval)


(declaim (ftype (function (stream) *) eval-fn))
(defun eval-fn (input)
    (eval (read input)))
