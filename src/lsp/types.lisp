(defpackage :alive/lsp/types
    (:use :cl)
    (:export :message-header
             :content-length
             :request-params
             :request-payload
             :response-payload
             :params
    ))

(in-package :alive/lsp/types)


(defclass message-header ()
    ((content-length :accessor content-length
                     :initform nil
                     :initarg :content-length
     )))


(defclass message-payload ()
    ((id :accessor id
         :initform nil
         :initarg :id
     )))


(defclass request-payload (message-payload)
    ((jsonrpc :accessor jsonrpc
              :initform 2.0
              :initarg :jsonrpc
     )
     (method-name :accessor method-name
                  :initform nil
                  :initarg :method-name
     )
     (params :accessor params
             :initform nil
             :initarg :params
     )))


(defclass response-payload (message-payload)
    ((result :accessor result
             :initform nil
             :initarg :result
     )
     (error-info :accessor error-info
                 :initform nil
                 :initarg :error-info
     )))


(defclass message ()
    ((header :accessor header
             :initform (make-instance 'message-header)
             :initarg :header
     )
     (payload :accessor payload
              :initform nil
              :initarg :payload
     )))
