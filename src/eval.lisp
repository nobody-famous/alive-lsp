(defpackage :alive/eval
    (:use :cl)
    (:export :from-string)
    (:local-nicknames (:pkgs :alive/packages)))

(in-package :alive/eval)


(defun from-string (str &key pkg-name)
    (let* ((input (make-string-input-stream str))
           (pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (when (and pkg-name (not pkg))
              (error (make-condition 'pkgs:package-not-found :name pkg-name)))

        (eval (read input))))
