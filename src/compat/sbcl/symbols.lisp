(defpackage :alive/sbcl/symbols
    (:use :cl)
    (:export :callable-p
             :get-lambda-list
             :function-p
             :macro-p)
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


(defun function-p (sym-name &optional pkg-name)
    (let* ((pkg-str (if pkg-name
                        pkg-name
                        "CL-USER"))
           (pkg (find-package (string-upcase pkg-str)))
           (sym (when pkg
                      (find-symbol (string-upcase sym-name) pkg))))

        (if (sb-introspect:function-type sym)
            T
            NIL)))


(defun callable-p (sym-name &optional pkg-name)
    (or (function-p sym-name pkg-name)
        (macro-p sym-name pkg-name)))
