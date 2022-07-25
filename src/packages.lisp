(defpackage :alive/packages
    (:use :cl)
    (:export :for-pos
             :for-string
             :list-all
             :lookup
             :package-not-found
             :do-remove
             :unexport-symbol)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:logger :alive/logger)
                      (:pos :alive/position)))

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
                  :initarg :exports)
         (nicknames :accessor nicknames
                    :initform nil
                    :initarg :nicknames)))


(defmethod print-object ((obj lisp-package) out)
    (format out "{name: ~A; exports: ~A; nicknames: ~A}"
        (name obj)
        (exports obj)
        (nicknames obj)))


(defun get-all-exports (pkg)
    (let ((syms nil))
        (do-external-symbols (s pkg syms)
            (push (string-downcase (string s))
                  syms))))


(defun get-all-nicknames (pkg)
    (package-nicknames pkg))


(defun create-package (pkg)
    (let ((name (package-name pkg))
          (exports (get-all-exports pkg))
          (nicknames (get-all-nicknames pkg)))
        (list (cons :name name)
              (cons :exports exports)
              (cons :nicknames nicknames))))


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


(defun for-string (str)
    (lookup (ignore-errors
                (read
                    (make-string-input-stream str)))))


(defun name-from-string (str)
    (let ((pkg (for-string str)))
        (when pkg (string-downcase (package-name pkg)))))


(defun for-pos (text pos)
    (loop :with forms := (forms:from-stream (make-string-input-stream text))
          :with pkg := "cl-user"

          :for form :in forms
          :until (pos:less-or-equal pos (form:get-start-new form))
          :do (when (and (form:is-in-pkg-new form)
                         (<= 2 (length (form:get-kids-new form))))
                    (setf pkg (name-from-string (subseq text
                                                        (form:get-start-offset-new (elt (form:get-kids-new form) 1))
                                                        (form:get-end-offset-new (elt (form:get-kids-new form) 1))))))

          :finally (return pkg)))


(defun do-remove (name)
    (let ((pkg (lookup name)))
        (when pkg (delete-package pkg))))