(defpackage :alive/lsp/message/alive/debugger
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-message
             :get-restarts
             :get-stack-trace
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/debugger)


(defclass request (message:request)
        ((message::method :initform "$/alive/debugger")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((message :accessor message
                  :initform nil
                  :initarg :message)
         (restarts :accessor restarts
                   :initform nil
                   :initarg :restarts)
         (stack-trace :accessor stack-trace
                      :initform nil
                      :initarg :stack-trace)))


(defmethod print-object ((obj params) out)
    (format out "{message: ~A; restarts: ~A; stack: ~A}"
        (message obj)
        (restarts obj)
        (stack-trace obj)))


(defclass response (message:result-response)
        ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
        (message:id obj)
        (message:result obj)))


(defclass response-body ()
        ((text :accessor text
               :initform nil
               :initarg :text)))


(defmethod print-object ((obj response-body) out)
    (format out "{text: ~A}" (text obj)))


(defun create-response (id text)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body :text text)))


(defun get-message (obj)
    (let ((params (message:params obj)))
        (message params)))


(defun get-restarts (obj)
    (let ((params (message:params obj)))
        (restarts params)))


(defun get-stack-trace (obj)
    (let ((params (message:params obj)))
        (stack-trace params)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun create-params (&key message restarts stack-trace)
    (make-instance 'params
        :message message
        :restarts restarts
        :stack-trace stack-trace))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :message) (setf (message params) value))
                              ((eq key :restarts) (setf (restarts params) value))
                              ((eq key :stack-trace) (setf (stack-trace params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))