(defpackage :alive/session/handlers
    (:use :cl)
    (:export :list-of-handlers
             :new-get-handler))

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


(declaim (ftype (function (list-of-handlers string) (or null (function (alive/deps:dependencies cons) (or null hash-table)))) new-get-handler))
(defun new-get-handler (handlers name)
    (cdr (assoc name handlers :test #'string=)))
