(defpackage :alive/sbcl/symbols
    (:use :cl)
    (:export :callable-p)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:types :alive/types)))

(in-package :alive/sbcl/symbols)


(defun callable-p (name)
    nil)
