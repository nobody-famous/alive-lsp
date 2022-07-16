(defpackage :alive/lsp/message/alive/stdout
    (:use :cl)
    (:export :create)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/stdout)


(defclass notification (message:notification)
        ((method :initform "$/alive/stdout")))


(defmethod print-object ((obj notification) out)
    (format out "{method: ~A; params: ~A}"
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((data :accessor data
               :initform nil
               :initarg :data)))


(defmethod print-object ((obj params) out)
    (format out "{data: \"~A\"}" (data obj)))


(defun create (data)
    (make-instance 'notification
        :params (make-instance 'params
                    :data data)))
