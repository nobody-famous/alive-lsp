(defpackage :alive/lsp/message/document/fmt-on-type
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :pos
             :request
             :text-document)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:fmt-opts :alive/lsp/types/formatting-options)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/fmt-on-type)


(defclass request (message:request)
        ((message::method :initform "textDocument/onTypeFormatting")))


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


(defun create-request (&key id (jsonrpc "2.0") params)
    (make-instance 'request
        :id id
        :jsonrpc jsonrpc
        :params params))


(defclass params ()
        ((text-document :accessor text-document
                        :initform nil
                        :initarg :text-document)
         (pos :accessor pos
              :initform nil
              :initarg :pos)
         (ch :accessor ch
             :initform nil
             :initarg :ch)
         (options :accessor options
                  :initform nil
                  :initarg :options)))


(defmethod print-object ((obj params) out)
    (format out "{text-document: ~A; pos: ~A; ch: ~A; options: ~A}"
        (text-document obj)
        (pos obj)
        (ch obj)
        (options obj)))


(defun create-params (&key text-document pos ch options)
    (make-instance 'params
        :text-document text-document
        :pos pos
        :ch ch
        :options options))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                              ((eq key :position) (setf (pos out-params) (pos:from-wire value)))
                              ((eq key :ch) (setf (ch out-params) value))
                              ((eq key :options) (setf (options out-params) (fmt-opts:from-wire value))))))

        (loop :with out-params := (make-instance 'params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                   :jsonrpc jsonrpc
                                   :id id
                                   :params out-params)))))