(defpackage :alive/sys/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn
             :untrace-fn)
    (:local-nicknames (:symbols :alive/symbols)
                      (:packages :alive/packages)))

(in-package :alive/sys/traced-fns)


(declaim (ftype (function (string) boolean) trace-fn))
(defun trace-fn (fn-name)
    (let ((to-eval (format NIL "(trace ~A)" fn-name)))
        (if (eval (read (make-string-input-stream to-eval)))
            T
            NIL)))


(declaim (ftype (function (string) boolean) untrace-fn))
(defun untrace-fn (fn-name)
    (let ((to-eval (format NIL "(untrace ~A)" fn-name)))
        (if (eval (read (make-string-input-stream to-eval)))
            T
            NIL)))


(declaim (ftype (function () (values list &optional)) list-all))
(defun list-all ()
    (loop :for name :in (trace)
          :collect (list (cons :package (string-downcase (package-name (symbol-package name))))
                         (cons :name (symbols:normalize (symbol-name name))))))
