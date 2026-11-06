(defpackage :alive/session/handlers
    (:use :cl)
    (:export :list-of-handlers
             :get-handler))

(in-package :alive/session/handlers)


(defun handler-item-p (data)
    (and (consp data)
         (typep (car data) 'string)
         (typep (cdr data) 'function)))


(defun list-of-handlers-p (data)
    (and (consp data)
         (every #'handler-item-p data)))


(deftype handler-item ()
    `(satisfies handler-item-p))


(deftype list-of-handlers ()
    `(satisfies list-of-handlers-p))


(defun get-handler (handlers name)
    (cdr (assoc name handlers :test #'string=)))
