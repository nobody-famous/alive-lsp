(defpackage :alive/test/files
    (:use :cl))

(in-package :alive/test/files)

(declaim (optimize (speed 3)))


(defun foo (b)
    (let* ((x 5)
           (y 10)
           (z (+ x y)))
        (format T "FOO CALLED ~A~%" z)))

(defmacro bar ()
    nil)

(defmacro bar ()
    nil)

(let ((x 0))
    (declare (type fixnum x))

    (loop :with y := 0
          :while (<= y x)
          :do (incf y))
    (if (and (<= x 5)
             (typep x 'null)) "a" "b"))

(
