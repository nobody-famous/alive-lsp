(defpackage :alive/packages
    (:use :cl)
    (:export :list-all))

(in-package :alive/packages)


(defclass lisp-package ()
    ((name :accessor name
           :initform nil
           :initarg :name)
     (exports :accessor exports
              :initform nil
              :initarg :exports)))


(defmethod print-object ((obj lisp-package) out)
    (format out "{name: ~A; exports: ~A}"
            (name obj)
            (exports obj)))


(defun get-all-exports (pkg)
    (let ((syms nil))
        (do-external-symbols (s pkg syms)
            (push s syms))))


(defun create-package (pkg)
    (let ((name (package-name pkg))
          (exports (get-all-exports pkg)))
        (make-instance 'lisp-package
                       :name name
                       :exports exports)))


(defun list-all ()
    (mapcar (lambda (pkg)
                (create-package pkg))
            (list-all-packages)))
