(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :get-all
             :get-all-names
             :get-lambda-list
             :external-p
             :function-p
             :lookup
             :macro-p))

(in-package :alive/symbols)


(defun function-p (name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:function-p name pkg-name))


(defun get-lambda-list (fn-name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:get-lambda-list fn-name pkg-name))


(defun has-lambda-list-p (sym-name &optional pkg-name)
    (if (get-lambda-list sym-name pkg-name)
        T
        NIL))


(defun macro-p (sym-name &optional pkg-name)
    (let* ((pkg (if pkg-name
                    (find-package (string-upcase pkg-name))
                    *package*))
           (sym (when pkg
                      (find-symbol (string-upcase sym-name) pkg))))

        (if (macro-function sym)
            T
            NIL)))


(defun callable-p (sym-name &optional pkg-name)
    (or (function-p sym-name pkg-name)
        (macro-p sym-name pkg-name)
        (has-lambda-list-p sym-name pkg-name)))


(defun external-p (sym-name &optional pkg-name)
    (let* ((pkg (if pkg-name
                    (find-package (string-upcase pkg-name))
                    *package*)))

        (when pkg
              (multiple-value-bind (sym status)

                      (find-symbol (string-upcase sym-name) pkg)

                  (declare (ignore sym))

                  (or (eq status :external)
                      (eq status :inherited))))))


(defun lookup (name pkg-name)
    (let ((pkg (find-package (string-upcase pkg-name))))
        (when pkg
              (find-symbol (string-upcase name) pkg))))


(defun get-all (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push s syms))))


(defun get-all-names (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push (string-downcase (string s)) syms))))
