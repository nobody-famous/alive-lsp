(defpackage :alive/lsp/completions
    (:use :cl)
    (:export :simple))

(in-package :alive/lsp/completions)


(defun simple (&key text pos)
    (format T "SIMPLE: ~A ~A~%" pos text))
