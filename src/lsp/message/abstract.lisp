(defpackage :alive/lsp/message/abstract
    (:use :cl)
    (:export :id
             :version
             :method-name
             :notification
             :params
             :request
             :result-response))

(in-package :alive/lsp/message/abstract)


(defclass message ()
    ((jsonrpc :accessor version
              :initform "2.0"
              :initarg :jsonrpc)))


(defclass notification (message)
    ((method :accessor method-name
             :initform nil
             :initarg :method)
     (params :accessor params
             :initform nil
             :initarg :params)))


(defclass request (notification)
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defclass response (message)
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defclass result-response (response)
    ((result :accessor result
             :initform nil
             :initarg :result)))


(defclass error-response (response)
    ((error :accessor error-info
            :initform nil
            :initarg :error)))
