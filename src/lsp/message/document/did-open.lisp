(defpackage :alive/lsp/message/document/did-open
    (:use :cl)
    (:export :did-open
             :from-wire
             :get-text
             :get-uri)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:tdi :alive/lsp/types/text-doc-item)))

(in-package :alive/lsp/message/document/did-open)


(defclass did-open (message:notification)
    ((message::method :initform "textDocument/didOpen")))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defun get-uri (msg)
    (let* ((params (message:params msg))
           (doc (text-document params)))
        (tdi:uri doc)))


(defun get-text (msg)
    (let* ((params (message:params msg))
           (doc (text-document params)))
        (tdi:text doc)))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :text-document) (setf (text-document params)
                                                       (tdi:from-wire value))))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (make-instance 'did-open :params out-params)))))