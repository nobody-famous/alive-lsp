(defpackage :alive/sys/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn
             :trace-pkg
             :untrace-fn
             :untrace-pkg)
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


(declaim (ftype (function (string) boolean) trace-pkg))
(defun trace-pkg (fn-name)
    (if (eval `(trace ,(string-upcase fn-name)))
        T
        NIL))


(declaim (ftype (function (string) boolean) untrace-pkg))
(defun untrace-pkg (fn-name)
    (if (eval `(untrace ,(string-upcase fn-name)))
        T
        NIL))


(declaim (ftype (function () (values list &optional)) list-all))
(defun list-all ()
    (loop :for name :in (trace)
          :collect (list (cons :package (string-downcase (package-name (symbol-package name))))
                         (cons :name (symbols:normalize (symbol-name name))))))
