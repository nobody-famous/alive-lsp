(defpackage :alive/lsp/errors
    (:use :cl))

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
