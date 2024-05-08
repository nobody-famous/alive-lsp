(defpackage :alive/session/handlers
    (:use :cl)
    (:export :get-handler
             :list-of-handlers
             :with-handlers))

(in-package :alive/session/handlers)


(declaim (ftype (function (T) boolean) handler-item-p))
(defun handler-item-p (data)
    (and (consp data)
         (typep (car data) 'string)
         (typep (cdr data) 'function)))


(declaim (ftype (function (T) boolean) list-of-handlers-p))
(defun list-of-handlers-p (data)
    (and (consp data)
         (every #'handler-item-p data)))


(deftype handler-item ()
    `(satisfies handler-item-p))


(deftype list-of-handlers ()
    `(satisfies list-of-handlers-p))


(declaim (type (or null list-of-handlers) *handlers*))
(defparameter *handlers* nil)


(declaim (ftype (function (string) (or null (function (cons) (or null hash-table)))) get-handler))
(defun get-handler (name)
    (cdr (assoc name *handlers* :test #'string=)))


(defmacro with-handlers (handlers &body body)
    `(let ((*handlers* ,handlers))
         ,@body))
