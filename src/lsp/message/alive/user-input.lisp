(defpackage :alive/lsp/message/alive/user-input
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
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/user-input)


(defclass request (message:request)
        ((message::method :initform "$/alive/userInput")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A}"
        (message:id obj)
        (message:method-name obj)))


(defun create-request (&key jsonrpc id)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id))


(defclass response (message:result-response)
        ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
        (message:id obj)
        (message:result obj)))


(defclass response-body ()
        ((input :accessor input
                :initform nil
                :initarg :input)))


(defmethod print-object ((obj response-body) out)
    (format out "{input: ~A}"
        (input obj)))


(defun create-response (&key id input)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body
                    :input input)))