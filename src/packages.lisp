(defpackage :alive/packages
    (:use :cl)
    (:export :for-pos
             :list-all
             :lookup
             :package-not-found
             :unexport-symbol)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:logger :alive/logger)))

(in-package :alive/packages)


(define-condition package-not-found (error)
    ((name :accessor name
           :initform nil
           :initarg :name))
    (:report (lambda (condition stream) (format stream "Package Not Found: ~A" (name condition)))))


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


(defun lookup (name)
    (find-package (string-upcase name)))


(defun unexport-symbol (pkg-name sym-name)
    (let* ((pkg (lookup pkg-name))
           (sym (when pkg (find-symbol (string-upcase sym-name) pkg))))
        (when sym
              (unexport sym pkg))))


(defun for-pos (text pos logger)
    (declare (ignore pos))

    (loop :with forms := (forms:from-stream (make-string-input-stream text))

          :for form :in forms :do
              (logger:error-msg logger "FORM ~A POS ~A~%" (form:get-start form) pos)

          :finally (return "cl-user")))
