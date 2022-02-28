(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list
             :function-p
             :macro-p))

(in-package :alive/symbols)


(defun function-p (name &optional pkg)
    #+sbcl (alive/sbcl/symbols:function-p name pkg))


(defun get-lambda-list (fn-name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:get-lambda-list fn-name pkg-name))


(defun macro-p (sym-name &optional pkg-name)
    (let* ((pkg-str (if pkg-name
                        pkg-name
                        "CL-USER"))
           (pkg (find-package (string-upcase pkg-str)))
           (sym (when pkg
                      (find-symbol (string-upcase sym-name) pkg))))

        (if (macro-function sym)
            T
            NIL)))


(defun callable-p (sym-name &optional pkg-name)
    (or (function-p sym-name pkg-name)
        (macro-p sym-name pkg-name)))
