(defpackage :alive/packages
    (:use :cl)
    (:export :xyz-for-pos
             :for-pos
             :for-string
             :for-tokens
             :list-all
             :lookup
             :package-not-found
             :do-remove
             :unexport-symbol)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:logger :alive/logger)
                      (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:types :alive/types)
                      (:utils :alive/utils)))

(in-package :alive/packages)


(define-condition package-not-found (error)
        ((name :accessor name
               :initform nil
               :initarg :name))
    (:report (lambda (condition stream) (format stream "Package Not Found: ~A" (name condition)))))


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
        (if (packagep pkg)
            (string-downcase (package-name pkg))
            "cl-user")))


(defun for-tokens (tokens pkg-name)
    (let* ((token1 (car tokens))
           (token2 (cadr tokens))
           (token3 (caddr tokens))
           (token4 (cadddr tokens))
           (pkg (lookup pkg-name))
           (*package* (or pkg *package*)))

        (unless token4
            (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                        (eq (token:get-type-value token2) types:*colons*)
                        (eq (token:get-type-value token3) types:*symbol*))
                      (let* ((real-pkg (lookup (token:get-text token1)))
                             (real-pkg-name (if real-pkg
                                                (package-name real-pkg)
                                                (token:get-text token1))))
                          (values (token:get-text token3) real-pkg-name)))

                  ((eq (token:get-type-value token1) types:*symbol*)
                      (values (token:get-text token1) pkg-name))))))


(defun in-package-p (form pkg)
    (let* ((kids (form:get-kids form))
           (kid (first kids))
           (in-pkg-sym (find-symbol "IN-PACKAGE" "CL-USER")))
        (when (and (eq alive/types:*open-paren* (form:get-form-type form))
                   kid)
              (multiple-value-bind (name pkg-name)
                      (for-tokens (form:get-tokens kid) pkg)
                  (eq (utils:lookup-symbol name pkg-name) in-pkg-sym)))))


(defun for-pos (text pos)
    (loop :with forms := (forms:from-stream (make-string-input-stream text))
          :with prev-form := nil
          :with pkg := "cl-user"

          :for form :in forms
          :until (pos:less-or-equal pos (form:get-start form))
          :do (when (and (not (eq alive/types:*ifdef-false* (form:get-form-type prev-form)))
                         (in-package-p form pkg)
                         (= 2 (length (form:get-kids form))))
                    (setf pkg (name-from-string (subseq text
                                                        (form:get-start-offset (elt (form:get-kids form) 1))
                                                        (form:get-end-offset (elt (form:get-kids form) 1))))))
              (setf prev-form form)
          :finally (return pkg)))


(defun xyz-for-pos (forms pos)
    (loop :with prev-form := nil
          :with pkg := "cl-user"

          :for form :in forms
          :until (pos:less-or-equal pos (form:get-start form))
          :do (when (and (not (eq alive/types:*ifdef-false* (form:get-form-type prev-form)))
                         (in-package-p form pkg))
                    (setf pkg (name-from-string (form:get-sym-text form))))
              (setf prev-form form)
          :finally (return pkg)))


(defun do-remove (name)
    (let ((pkg (lookup name)))
        (when pkg (delete-package pkg))))
