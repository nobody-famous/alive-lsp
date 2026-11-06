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


(defun trace-fn (fn-name)
    (let ((to-eval (format NIL "(trace ~A)" fn-name)))
        (if (eval (read (make-string-input-stream to-eval)))
            fn-name
            NIL)))


(defun untrace-fn (fn-name)
    (let ((to-eval (format NIL "(untrace ~A)" fn-name)))
        (if (eval (read (make-string-input-stream to-eval)))
            T
            NIL)))


(defun trace-pkg (pkg-name)
    (if (eval `(trace ,(string-upcase pkg-name)))
        T
        NIL))


(defun untrace-pkg (fn-name)
    (if (eval `(untrace ,(string-upcase fn-name)))
        T
        NIL))


(defun list-all ()
    (loop :for name :in (trace)
          :collect (list (cons :package (string-downcase (package-name (symbol-package name))))
                         (cons :name (symbol-name name)))))
