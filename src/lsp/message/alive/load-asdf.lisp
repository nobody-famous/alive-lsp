(defpackage :alive/lsp/message/alive/load-asdf
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-name
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/load-asdf)


(defclass request (message:request)
        ((message::method :initform "$/alive/loadAsdfSystem")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((name :accessor name
               :initform nil
               :initarg :name)))


(defmethod print-object ((obj params) out)
    (format out "{name: ~A}"
        (name obj)))


(defun get-name (params)
    (when params
          (name params)))


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


(defun create-response (id)
    (message:create-response id :result-value T)
    #+n (make-instance 'response
            :id id
            :result (make-instance 'response-body)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun create-params (&key name)
    (make-instance 'params :name name))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :name) (setf (name params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
