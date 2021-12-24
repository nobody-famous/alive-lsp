(defpackage :alive/lsp/message/notification
    (:use :cl)
    (:export :message
             :method-name
             :params)
    (:local-nicknames (:jsonrpc :alive/lsp/message/jsonrpc)))

(in-package :alive/lsp/message/notification)


(defclass message (jsonrpc:message)
    ((method :accessor method-name
             :initform nil
             :initarg :method)
     (params :accessor params
             :initform nil
             :initarg :params)))
