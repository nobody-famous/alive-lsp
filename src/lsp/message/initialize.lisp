(defpackage :alive/lsp/message/initialize
    (:use :cl)
    (:export :request-from-wire
             :request)
    (:local-nicknames (:request :alive/lsp/message/request)
                      (:notification :alive/lsp/message/notification)
                      (:jsonrpc :alive/lsp/message/jsonrpc)))

(in-package :alive/lsp/message/initialize)


(defclass client-info ()
    ((name :accessor name
           :initform nil
           :initarg :name)
     (version :accessor version
              :initform nil
              :initarg :version)))


(defclass params ()
    ((client-info :accessor client-info
                  :initform (make-instance 'client-info)
                  :initarg :client-info)
     (locale :accessor locale
             :initform nil
             :initarg :locale)
     (root-path :accessor root-path
                :initform nil
                :initarg :root-path)
     (root-uri :accessor root-uri
               :initform nil
               :initarg :root-uri)
     (process-id :accessor process-id
                 :initform nil
                 :initarg :process-id)
     (capabilities :accessor capabilities
                   :initform nil
                   :initarg :capabilities)
     (trace-enabled :accessor trace-enabled
                    :initform nil
                    :initarg :trace-enabled)
     (workspace-folders :accessor workspace-folders
                        :initform nil
                        :initarg :workspace-folders)))


(defclass request (request:message)
    ())


(defun get-client-info (info)
    (loop :with out := (make-instance 'client-info)
          :for item :in info :do
              (cond ((eq (car item) :name) (setf (name out) (cdr item)))
                    ((eq (car item) :version) (setf (version out) (cdr item)))
                    (t (error (format nil "Unhandled client info item: ~A" item))))
          :finally (return out)))


(defun update-param (params key value)
    (cond ((eq key :client-info) (setf (client-info params) (get-client-info value)))
          ((eq key :locale) (setf (locale params) value))
          ((eq key :root-path) (setf (root-path params) value))
          ((eq key :root-uri) (setf (root-uri params) value))
          ((eq key :process-id) (setf (process-id params) value))
          ((eq key :capabilities) (setf (capabilities params) value))
          ((eq key :trace) (setf (trace-enabled params) value))
          ((eq key :workspace-folders) (setf (workspace-folders params) value))
          (t (error (format nil "Unhandled init request param: ~A" key)))))


(defun add-params (req params)
    (loop :with init-params := (make-instance 'params)
          :for param :in params :do
              (update-param init-params (car param) (cdr param))
          :finally (setf (notification:params req) init-params)))


(defun request-from-wire (&key jsonrpc id params)
    (let ((req (make-instance 'request)))

        (setf (notification:method-name req) "initialize")
        (setf (jsonrpc:version req) jsonrpc)
        (setf (request:id req) id)

        (add-params req params)

        req))
