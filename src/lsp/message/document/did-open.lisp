(defpackage :alive/lsp/message/document/did-open
    (:use :cl)
    (:export :create-did-open
             :create-params
             :did-open
             :from-wire
             :get-text
             :get-uri)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:tdi :alive/lsp/types/text-doc-item)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/did-open)


(defclass did-open (message:notification)
    ((message::method :initform "textDocument/didOpen")))


(defmethod print-object ((obj did-open) out)
    (format out "{method: \"~A\"; params: ~A}"
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a did-open) (b did-open))
    (and (types:deep-equal-p (message:params a) (message:params b))))


(defun create-did-open (params)
    (make-instance 'did-open
                   :params params))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defmethod print-object ((obj params) out)
    (format out "{text-document: ~A}" (text-document obj)))


(defmethod types:deep-equal-p ((a params) (b params))
    (types:deep-equal-p (text-document a) (text-document b)))


(defun create-params (doc)
    (make-instance 'params
                   :text-document doc))


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