(defpackage :alive/lsp/message/abstract
    (:use :cl)
    (:export :create-error-resp
             :id
             :version
             :method-name
             :notification
             :params
             :request
             :result
             :result-response))

(in-package :alive/lsp/message/abstract)


(defclass message ()
    ((jsonrpc :accessor version
              :initform "2.0"
              :initarg :jsonrpc)))


(defclass notification (message)
    ((method :accessor method-name
             :initform nil
             :initarg :method)
     (params :accessor params
             :initform nil
             :initarg :params)))


(defclass request (notification)
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defclass response (message)
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defclass result-response (response)
    ((result :accessor result
             :initform nil
             :initarg :result)))


(defclass error-data ()
    ((code :accessor code
           :initform nil
           :initarg :code)
     (message :accessor message
              :initform nil
              :initarg :message)))


(defmethod print-object ((obj error-data) out)
    (format out "{code: ~A; message ~A}"
            (code obj)
            (message obj)))


(defclass error-response (response)
    ((error :accessor error-info
            :initform nil
            :initarg :error)))


(defmethod print-object ((obj error-response) out)
    (format out "{id: ~A; error: ~A}"
            (id obj)
            (error-info obj)))


(defmethod create-error-resp (&key code message id)
    (make-instance 'error-response
                   :id id
                   :error (make-instance 'error-data
                                         :code code
                                         :message message)))
