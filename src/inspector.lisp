(defpackage :alive/inspector
    (:use :cl)
    (:export :create
             :get-text
             :get-pkg
             :get-result
             :to-result))

(in-package :alive/inspector)


(defclass inspector ()
        ((text :accessor text
               :initform nil
               :initarg :text)
         (pkg :accessor pkg
              :initform nil
              :initarg :pkg)
         (result :accessor result
                 :initform nil
                 :initarg :result)))


(defun create (&key text pkg result)
    (make-instance 'inspector
        :text text
        :pkg pkg
        :result result))


(defun get-text (obj)
    (when obj
          (text obj)))


(defun get-pkg (obj)
    (when obj
          (pkg obj)))


(defun get-result (obj)
    (when obj
          (result obj)))


(defmethod to-result ((obj fixnum))
    (princ-to-string obj))


(defun fn-to-result (sym name pkg-name)
    (let ((result (make-hash-table :test 'equalp)))
        (setf (gethash "documentation" result)
            (documentation sym 'function))

        (setf (gethash "lambda-list" result)
            (alive/symbols:get-lambda-list name pkg-name))

        (setf (gethash "disassemble" result)
            (with-output-to-string (str)
                (disassemble sym :stream str)))

        result))


(defmethod to-result ((obj symbol))
    (let* ((name (string obj))
           (pkg (symbol-package obj))
           (pkg-name (package-name pkg)))

        (cond ((alive/symbols:function-p name pkg-name) (fn-to-result obj name pkg-name))
              ((alive/symbols:macro-p name pkg-name) (fn-to-result obj name pkg-name))
              (T (princ-to-string obj)))))
