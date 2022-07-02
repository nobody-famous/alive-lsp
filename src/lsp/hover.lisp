(defpackage :alive/lsp/hover
    (:use :cl)
    (:export :get-text))

(in-package :alive/lsp/hover)


(defun get-text (&key text pos)
    (declare (ignore text pos))
    nil)
