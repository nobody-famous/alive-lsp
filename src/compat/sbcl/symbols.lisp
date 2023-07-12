(defpackage :alive/sbcl/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list
             :function-p))

(in-package :alive/sbcl/symbols)


(defun get-lambda-list (fn-name &optional pkg-name)
    (let* ((pkg (ignore-errors (if pkg-name
                                   (find-package (string-upcase pkg-name))
                                   *package*)))
           (*package* (if pkg
                          pkg
                          *package*)))

        (ignore-errors (sb-introspect:function-lambda-list
                           (find-symbol (string-upcase fn-name))))))


(defun function-p (sym-name &optional pkg-name)
    (let* ((pkg (if pkg-name
                    (find-package (string-upcase pkg-name))
                    *package*))
           (sym (when pkg
                      (find-symbol (string-upcase sym-name) pkg))))

        (if (sb-introspect:function-type sym)
            T
            NIL)))
