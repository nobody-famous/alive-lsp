(defpackage :alive/sbcl/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:types :alive/types)))

(in-package :alive/sbcl/symbols)


(defun get-lambda-list (fn-name &optional pkg-name)
    (let* ((pkg-str (if pkg-name
                        pkg-name
                        "CL-USER"))
           (pkg (ignore-errors (find-package (string-upcase pkg-str))))
           (*package* (if pkg
                          pkg
                          *package*)))

        (ignore-errors (sb-introspect:function-lambda-list
                        (find-symbol (string-upcase fn-name))))))


(defun callable-p (sym-name &optional pkg-name)
    (if (get-lambda-list sym-name pkg-name)
        T
        NIL))
