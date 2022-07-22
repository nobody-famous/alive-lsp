(defpackage :alive/lsp/message/alive/do-eval
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :create-response-new
             :from-wire
             :get-package
             :get-text
             :store-result-p
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/do-eval)


(defclass request (message:request)
        ((message::method :initform "$/alive/eval")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((package :accessor pkg-name
                  :initform nil
                  :initarg :pkg-name)
         (text :accessor text
               :initform nil
               :initarg :text)
         (store-result :accessor store-result
                       :initform nil
                       :initarg :store-result)))

(defmethod print-object ((obj params) out)
    (format out "{package: ~A; text: ~A; store-result: ~A}"
        (pkg-name obj)
        (text obj)
        (store-result obj)))


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


(defun create-response-new (id text)
    (let ((data (make-hash-table)))
        (setf (gethash "text" data) text)
        (message:create-response id
                                 :result-value data)))


(defun create-response (id text)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body :text text)))


(defun get-package (obj)
    (let ((params (message:params obj)))
        (pkg-name params)))


(defun get-text (obj)
    (let ((params (message:params obj)))
        (text params)))


(defun store-result-p (obj)
    (let ((params (message:params obj)))
        (store-result params)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun create-params (&key pkg-name text store-result)
    (make-instance 'params
        :pkg-name pkg-name
        :text text
        :store-result store-result))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :package) (setf (pkg-name params) value))
                              ((eq key :text) (setf (text params) value))
                              ((eq key :store-result) (setf (store-result params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))