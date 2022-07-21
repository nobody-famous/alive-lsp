(defpackage :alive/lsp/message/document/hover
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :create-response-new
             :from-wire
             :pos
             :request
             :text-document)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/hover)


(defclass request (message:request)
        ((message::method :initform "textDocument/hover")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
        (message:method-name obj)
        (message:params obj)))


(defclass response-body ()
        ((value :accessor value
                :initform nil
                :initarg :value)))


(defclass response (message:result-response)
        ())


(defun create-response (&key id value)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body :value value)))


(defun create-response-new (id &key value)
    (message:create-response id
                             :result-value (list (cons :value value))))


(defun create-request (&key id (jsonrpc "2.0") params)
    (make-instance 'request
        :id id
        :jsonrpc jsonrpc
        :params params))


(defclass req-params ()
        ((text-document :accessor text-document
                        :initform nil
                        :initarg :text-document)
         (pos :accessor pos
              :initform nil
              :initarg :pos)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document: ~A; pos ~A}"
        (text-document obj)
        (pos obj)))


(defun create-params (&key text-document pos)
    (make-instance 'req-params
        :text-document text-document
        :pos pos))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                              ((eq key :position) (setf (pos out-params) (pos:from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                   :jsonrpc jsonrpc
                                   :id id
                                   :params out-params)))))