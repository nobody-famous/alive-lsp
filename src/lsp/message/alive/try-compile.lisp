(defpackage :alive/lsp/message/alive/try-compile
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-path
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/try-compile)


(defclass request (message:request)
    ((message::method :initform "$/alive/tryCompile")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
            (message:id obj)
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (message:id a) (message:id b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defclass params ()
    ((path :accessor path
           :initform nil
           :initarg :path)))


(defmethod print-object ((obj params) out)
    (format out "{path: ~A}"
            (path obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (string= (path a) (path b))))


(defclass response (message:result-response)
    ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
            (message:id obj)
            (message:result obj)))


(defclass response-body ()
    ((messages :accessor messages
               :initform nil
               :initarg :messages)))


(defmethod print-object ((obj response-body) out)
    (format out "{messages: ~A}" (messages obj)))


(defun create-response (id msgs)
    (make-instance 'response
                   :id id
                   :result (make-instance 'response-body :messages msgs)))


(defun get-path (obj)
    (let ((params (message:params obj)))
        (path params)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
                   :jsonrpc jsonrpc
                   :id id
                   :params params))


(defun create-params (&key path (show-stdout T) (show-stderr T))
    (make-instance 'params :path path
                   :show-stdout show-stdout
                   :show-stderr show-stderr))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                  (cond ((eq key :path) (setf (path params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
