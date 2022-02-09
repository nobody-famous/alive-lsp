(defpackage :alive/test/compat/sbcl/symbols
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/compat/sbcl/symbols)


(defun lookup ()
    (alive/symbols:callable-p "callable-p" "alive/symbols")
    (not (alive/symbols:callable-p "callable-p" "foo")))


(defun run-all ()
    (lookup))