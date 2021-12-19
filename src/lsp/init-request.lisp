(defpackage :alive/lsp/init-request
    (:use :cl)
    (:export :from-wire-params)
)

(in-package :alive/lsp/init-request)


(defstruct client-info
    name
    version
)


(defstruct params
    client-info
    locale
    root-path
    root-uri
    process-id
    capabilities
    trace-enabled
    workspace-folders
)


(defun get-client-info (info)
    (loop :with out := (make-client-info)
          :for item :in info :do
              (cond ((eq (car item) :name) (setf (client-info-name out) (cdr item)))
                    ((eq (car item) :version) (setf (client-info-version out) (cdr item)))
                    (t (error (format nil "Unhandled client info item: ~A" item)))
              )
          :finally (return out)
    ))


(defun update-param (params key value)
    (cond ((eq key :client-info) (setf (params-client-info params) (get-client-info value)))
          ((eq key :locale) (setf (params-locale params) value))
          ((eq key :root-path) (setf (params-root-path params) value))
          ((eq key :root-uri) (setf (params-root-uri params) value))
          ((eq key :process-id) (setf (params-process-id params) value))
          ((eq key :capabilities) (setf (params-capabilities params) value))
          ((eq key :trace) (setf (params-trace-enabled params) value))
          ((eq key :workspace-folders) (setf (params-workspace-folders params) value))
          (t (error (format nil "Unhandled init request param: ~A" key)))
    ))


(defun from-wire-params (params)
    (loop :with init-params := (make-params)
          :for param :in params :do
              (update-param init-params (car param) (cdr param))
          :finally (return init-params)
    ))