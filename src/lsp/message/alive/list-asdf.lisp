(defpackage :alive/lsp/message/alive/list-asdf
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :create-response-new
             :from-wire
             :get-path
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/list-asdf)


(defclass request (message:request)
        ((message::method :initform "$/alive/listAsdfSystems")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass response (message:result-response)
        ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
        (message:id obj)
        (message:result obj)))


(defclass response-body ()
        ((systems :accessor systems
                  :initform nil
                  :initarg :systems)))


(defmethod print-object ((obj response-body) out)
    (format out "{systems: ~A}" (systems obj)))


(defun create-response (id systems)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body :systems systems)))


(defun create-response-new (id systems)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "systems" data) systems)

        (message:create-response id :result-value data)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun from-wire (&key jsonrpc id params)
    (declare (ignore params))

    (create-request :jsonrpc jsonrpc
                    :id id))
