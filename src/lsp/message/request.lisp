(defpackage :alive/lsp/message/request
    (:use :cl)
    (:export :message
             :id)
    (:local-nicknames (:notification :alive/lsp/message/notification)))

(in-package :alive/lsp/message/request)


(defclass message (notification:message)
    ((id :accessor id
         :initform nil
         :initarg :id)))
