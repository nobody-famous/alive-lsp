(defpackage :alive/lsp/message/abstract
    (:use :cl)
    (:export :create-error-resp
             :create-result-resp
             :create-response
             :error-from-wire
             :id
             :version
             :method-name
             :notification
             :params
             :request
             :response
             :result
             :error-response
             :result-response))

(in-package :alive/lsp/message/abstract)


(defun create-response (id &key result-value error-value)
    (let ((resp (list (cons :id id)
                      (cons :jsonrpc "2.0"))))

        (cond ((and error-value result-value) (error "Cannot create response with result and error"))
              (result-value (setf resp (acons :result result-value resp)))
              (error-value (setf resp (acons :error error-value resp))))

        (reverse resp)))


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
    (format out "{code: ~A; message: ~A}"
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


(defmethod create-result-resp (&key id result)
    (make-instance 'result-response
        :id id
        :result result))


(defun error-from-wire (&key id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :code) (setf (code out-params) value))
                              ((eq key :message) (setf (message out-params) value)))))

        (loop :with out-params := (make-instance 'error-data)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'error-response
                                   :id id
                                   :error out-params)))))