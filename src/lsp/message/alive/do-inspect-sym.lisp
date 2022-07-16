(defpackage :alive/lsp/message/alive/do-inspect-sym
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

(in-package :alive/lsp/message/alive/do-inspect-sym)


(defclass request (message:request)
        ((message::method :initform "$/alive/inspectSymbol")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
        (message:id obj)
        (message:method-name obj)
        (message:params obj)))


(defclass params ()
        ((package :accessor pkg-name
                  :initform nil
                  :initarg :pkg-name)
         (symbol :accessor sym
                 :initform nil
                 :initarg :sym)))

(defmethod print-object ((obj params) out)
    (format out "{package: ~A; symbol: ~A}"
        (pkg-name obj)
        (sym obj)))


(defclass response (message:result-response)
        ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
        (message:id obj)
        (message:result obj)))


(defclass response-body ()
        ((id :accessor id
             :initform nil
             :initarg :id)
         (result :accessor result
                 :initform nil
                 :initarg :result)))


(defmethod print-object ((obj response-body) out)
    (format out "{id: ~A; result: ~A}" (id obj) (result obj)))


(defun create-response (id insp-id result)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body
                    :id insp-id
                    :result result)))


(defun get-package (obj)
    (let ((params (message:params obj)))
        (pkg-name params)))


(defun get-symbol (obj)
    (let ((params (message:params obj)))
        (sym params)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun create-params (&key pkg-name sym)
    (make-instance 'params
        :pkg-name pkg-name
        :sym sym))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :package) (setf (pkg-name params) value))
                              ((eq key :symbol) (setf (sym params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))