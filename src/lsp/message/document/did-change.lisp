(defpackage :alive/lsp/message/document/did-change
    (:use :cl)
    (:export :did-change
             :from-wire
             :get-text
             :get-uri)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:tdi :alive/lsp/types/text-doc-item)))

(in-package :alive/lsp/message/document/did-change)


(defclass did-change (message:notification)
    ((message::method :initform "textDocument/didChange")))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)
     (content-changes :accessor content-changes
                      :initform nil
                      :initarg :content-changes)))


(defclass content-changes ()
    ((text :accessor text
           :initform nil
           :initarg :text)))


(defun get-uri (msg)
    (let* ((params (message:params msg))
           (doc (text-document params)))
        (tdi:uri doc)))


(defun get-text (msg)
    (format T "GET-TEXT ~A~%" (content-changes (message:params msg)))
    (let* ((params (message:params msg))
           (changes (content-changes params)))
        (text changes)))


(defun changes-from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :text) (setf (text params) value)))))

        (loop :with out-params := (make-instance 'content-changes)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return out-params))))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (format T "DID-CHANGE ~A~%" key)
                  (cond ((eq key :text-document) (setf (text-document params)
                                                       (tdi:from-wire value)))
                        ((eq key :content-changes) (setf (content-changes params)
                                                         (changes-from-wire value))))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (make-instance 'did-change :params out-params)))))