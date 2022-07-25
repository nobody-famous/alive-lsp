(defpackage :alive/lsp/types/format-options
    (:use :cl)
    (:export :convert))

(in-package :alive/lsp/types/format-options)


(defun convert (opts)
    (mapcar (lambda (item)
                (cond ((eq :tab-size (car item)) (cons :indent-width (cdr item)))
                      (T item)))
            opts))