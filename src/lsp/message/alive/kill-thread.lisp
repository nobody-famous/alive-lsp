(defpackage :alive/lsp/message/alive/kill-thread
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-id
             :request
             :show-stdout-p
             :show-stderr-p)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/kill-thread)


(defclass request (message:request)
    ((message::method :initform "$/alive/killThread")))


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
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defmethod print-object ((obj params) out)
    (format out "{id: ~A}"
            (id obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (id a) (id b))))


(defun get-id (msg)
    (let ((params (message:params msg)))
        (when params
              (id params))))


(defclass response (message:result-response)
    ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
            (message:id obj)
            (message:result obj)))


(defun create-response (id)
    (make-instance 'response
                   :id id
                   :result T))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
                   :jsonrpc jsonrpc
                   :id id
                   :params params))


(defun create-params (&key id)
    (make-instance 'params :id id))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                  (cond ((eq key :id) (setf (id params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
