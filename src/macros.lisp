(defpackage :alive/macros
    (:use :cl)
    (:export :expand
             :expand-1))

(in-package :alive/macros)


(defun do-expand (fn text)
    (handler-case
            (funcall fn (read (make-string-input-stream text)))
        (T ()
           nil)))

(defun expand (text)
    (do-expand 'macroexpand text))


(defun expand-1 (text)
    (do-expand 'macroexpand-1 text))
