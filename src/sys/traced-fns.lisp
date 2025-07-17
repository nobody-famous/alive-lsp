(defpackage :alive/sys/traced-fns
    (:use :cl)
    (:export :list-all)
    (:local-nicknames (:symbols :alive/symbols)))

(in-package :alive/sys/traced-fns)


(declaim (ftype (function () (values list &optional)) list-all))
(defun list-all ()
    (loop :for name :in (trace)
          :collect (list (cons :package (string-downcase (package-name (symbol-package name))))
                         (cons :name (symbols:escape (symbol-name name))))))
