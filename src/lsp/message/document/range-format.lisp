(defpackage :alive/lsp/message/document/range-format
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :text-document
             :range
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:utils :alive/lsp/message/document/format-utils)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:edit :alive/text-edit)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/range-format)


(defclass req-params ()
        ((text-document :accessor text-document
                        :initform nil
                        :initarg :text-document)
         (range :accessor range
                :initform nil
                :initarg :range)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document: ~A; range: ~A}"
        (text-document obj)
        (range obj)))


(defun create-params (&key text-document range)
    (make-instance 'req-params
        :text-document text-document
        :range range))


(defclass request (message:request)
        ((message::method :initform "textDocument/rangeFormatting")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
        (message:method-name obj)
        (message:params obj)))


(defun create-request (&key id jsonrpc params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                              ((eq key :range) (setf (range out-params) (range:from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
