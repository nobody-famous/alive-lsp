(defpackage :alive/lsp/message/alive/remove-pkg
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :name
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/remove-pkg)


(defclass request (message:request)
    ((message::method :initform "$/alive/removePackage")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
            (message:id obj)
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (message:id a) (message:id b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
                   :jsonrpc jsonrpc
                   :id id
                   :params params))


(defclass params ()
    ((name :accessor name
           :initform nil
           :initarg :name)))


(defmethod print-object ((obj params) out)
    (format out "{name: ~A}"
            (name obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (name a) (name b))))


(defun create-params (&key name)
    (make-instance 'params
                   :name name))


(defclass response (message:result-response)
    ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
            (message:id obj)
            (message:result obj)))


(defclass response-body ()
    ())


(defmethod print-object ((obj response-body) out)
    (format out "{}"))


(defun create-response (&key id)
    (make-instance 'response
                   :id id
                   :result (make-instance 'response-body)))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :name) (setf (name out-params) value)))))

        (loop :with out-params := (make-instance 'params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
