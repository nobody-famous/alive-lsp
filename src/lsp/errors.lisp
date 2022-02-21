(defpackage :alive/lsp/errors
    (:use :cl)
    (:export :unhandled-request
             :*parse-error*
             :*invalid-request*
             :*method-not-found*
             :*invalid-params*
             :*internal-error*
             :*server-not-initialized*
             :*unknown-error-code*
             :*request-failed*
             :*server-cancelled*
             :*content-modified*
             :*request-cancelled*

             method-name
             id))

(in-package :alive/lsp/errors)


(defparameter *parse-error* -32700)
(defparameter *invalid-request* -32600)
(defparameter *method-not-found* -32601)
(defparameter *invalid-params* -32602)
(defparameter *internal-error* -32603)
(defparameter *server-not-initialized* -32002)
(defparameter *unknown-error-code* -32001)
(defparameter *request-failed* -32803)
(defparameter *server-cancelled* -32802)
(defparameter *content-modified* -32801)
(defparameter *request-cancelled* -32800)


(define-condition unhandled-request (error)
    ((method-name :accessor method-name
                  :initform nil
                  :initarg :method-name)
     (id :accessor id
         :initform nil
         :initarg :id))

    (:report (lambda (c stream)
                 (format stream "Unhandled request for msg ~A: ~A"
                         (id c)
                         (method-name c)))))
