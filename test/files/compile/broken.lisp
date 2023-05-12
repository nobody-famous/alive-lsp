(defpackage :alive/test/files
    (:use :cl))

(in-package :alive/test/files)


(defun foo (b)
    (let* ((x 5)
           (y 10)
           (z (+ x y)))
        (format T "FOO CALLED ~A~%" z)))

(defmacro bar ()
    nil)

(defmacro bar ()
    nil)

(let ((x))
    (if (typep x 'null) "a" "b"))

(
