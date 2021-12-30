(defpackage :alive/lsp/message/document/sem-tokens-full
    (:use :cl)
    (:export :create-response
             :req-from-wire
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:text-doc :alive/lsp/types/text-doc)))

(in-package :alive/lsp/message/document/sem-tokens-full)


(defclass request (message:request)
    ((message::method :initform "textDocument/semanticTokens/full")))


(defmethod print-object ((obj request) out)
    (format out "{sem-tokens-full-request ~A}" (message:params obj)))


(defclass req-params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document ~A}" (text-document obj)))


(defclass response (message:result-response)
    ())


(defmethod print-object ((obj response) out)
    (format out "{sem-tokens-full-result ~A}" (message:result obj)))


(defclass sem-tokens ()
    ((data :accessor data
           :initform nil
           :initarg :data)))


(defun get-sem-tokens ()
    (make-instance 'sem-tokens :data (list 0 0 2 0 0)))


(defun create-response (msg)
    (let* ((params (message:params msg))
           (doc (text-document params))
           (uri (text-doc:uri doc)))
        (format T "URI ~A~%" uri))

    (format T "SEM TOKENS RESPONSE ~A~%" msg)
    (make-instance 'response
                   :id (message:id msg)
                   :result (get-sem-tokens)))


(defun req-from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:id-from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
