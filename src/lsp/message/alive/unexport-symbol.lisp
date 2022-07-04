(defpackage :alive/lsp/message/alive/unexport-symbol
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-package
             :get-symbol
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/unexport-symbol)


(defclass request (message:request)
        ((message::method :initform "$/alive/unexportSymbol")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((symbol :accessor sym-name
                 :initform nil
                 :initarg :sym-name)
         (package :accessor pkg-name
                  :initform nil
                  :initarg :pkg-name)))


(defmethod print-object ((obj params) out)
    (format out "{symbol: ~A; package: ~A}"
        (sym-name obj)
        (pkg-name obj)))


(defun get-symbol (msg)
    (let ((params (message:params msg)))
        (when params
              (sym-name params))))


(defun get-package (msg)
    (let ((params (message:params msg)))
        (when params
              (pkg-name params))))


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


(defun create-params (&key sym-name pkg-name)
    (make-instance 'params
        :sym-name sym-name
        :pkg-name pkg-name))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :symbol) (setf (sym-name params) value))
                              ((eq key :package) (setf (pkg-name params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
