(defpackage :alive/lsp/message/alive/do-inspect
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :get-package
             :get-text
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/do-inspect)


(defclass request (message:request)
        ((message::method :initform "$/alive/inspect")))


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
               :initarg :text)))

(defmethod print-object ((obj params) out)
    (format out "{package: ~A; text: ~A}"
        (pkg-name obj)
        (text obj)))


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


(defun get-text (obj)
    (let ((params (message:params obj)))
        (text params)))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defun create-params (&key pkg-name text)
    (make-instance 'params
        :pkg-name pkg-name
        :text text))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (params key value)
                        (cond ((eq key :package) (setf (pkg-name params) value))
                              ((eq key :text) (setf (text params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))