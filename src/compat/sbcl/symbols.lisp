(defpackage :alive/sbcl/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list
             :function-p)
    (:local-nicknames (:utils :alive/utils)))

(in-package :alive/sbcl/symbols)


(defun get-lambda-list (fn-name &optional pkg-name)
    (ignore-errors (sb-introspect:function-lambda-list
                       (utils:lookup-symbol fn-name pkg-name))))


(defun function-p (sym-name &optional pkg-name)
    (let ((sym (utils:lookup-symbol sym-name pkg-name)))
        (if (sb-introspect:function-type sym)
            T
            NIL)))
