(defpackage :alive/lsp/message/document/did-open
    (:use :cl)
    (:export :from-wire)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/document/did-open)


(defclass did-open (message:notification)
    ((message::method :initform "textDocument/didOpen")))


(defclass text-document-item ()
    ((uri :accessor uri
          :initform nil
          :initarg :uri)
     (language-id :accessor language-id
                  :initform nil
                  :initarg :language-id)
     (version :accessor version
              :initform nil
              :initarg :version)
     (text :accessor text
           :initform nil
           :initarg :text)))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defun get-text-doc (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :uri) (setf (uri params) value))
                        ((eq key :language-id) (setf (language-id params) value))
                        ((eq key :text) (setf (text params) value))
                        ((eq key :version) (setf (version params) value))
                        (t (error (format nil "Unhandled text doc key ~A" key))))))

        (loop :with item := (make-instance 'text-document-item)
              :for param :in params :do
                  (add-param item (car param) (cdr param))
              :finally (return item))))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :text-document) (setf (text-document params) (get-text-doc value))))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (make-instance 'did-open :params out-params)))))