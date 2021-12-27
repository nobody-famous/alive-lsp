(defpackage :alive/lsp/message/initialize
    (:use :cl)
    (:export :create-response
             :create-initialized-notification
             :initialized
             :request-from-wire
             :request)
    (:local-nicknames (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/initialize)


(defparameter *server-name* "Alive LSP")
(defparameter *server-version* "0.1")


(defparameter *doc-sync-none* 0)
(defparameter *doc-sync-full* 1)
(defparameter *doc-sync-incr* 2)


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


(defclass request (message:request)
    ((method :initform "initialize")))


(defclass server-info ()
    ((name :accessor name
           :initform nil
           :initarg :name)
     (version :accessor version
              :initform nil
              :initarg :version)))


(defclass doc-sync-options ()
    ((change :accessor change
             :initform *doc-sync-full*
             :initarg :change)))


(defclass sem-tokens-legend ()
    ((token-types :initform sem-tokens:*types*)
     (token-modifiers :initform sem-tokens:*mods*)))


(defclass sem-tokens-opts ()
    ((legend :initform (make-instance 'sem-tokens-legend))
     (full :initform T)))


(defclass server-capabilities ()
    ((text-document-sync :initform *doc-sync-full*)
     (hover-provider :initform T)
     (semantic-tokens-provider :initform (make-instance 'sem-tokens-opts))))


(defclass response-body ()
    ((capabilities :accessor capabilities
                   :initform (make-instance 'server-capabilities)
                   :initarg :capabilities)))


(defclass response (message:result-response)
    ((message::result :initform (make-instance 'response-body))))


(defclass initialized (message:notification)
    ((method :initform "initialized")))


(defun get-client-info (info)
    (loop :with out := (make-instance 'client-info)
          :for item :in info :do
              (cond ((eq (car item) :name) (setf (name out) (cdr item)))
                    ((eq (car item) :version) (setf (version out) (cdr item)))
                    (t (error (format nil "Unhandled client info item: ~A" item))))
          :finally (return out)))


(defun request-from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :client-info) (setf (client-info out-params) (get-client-info value)))
                        ((eq key :locale) (setf (locale out-params) value))
                        ((eq key :root-path) (setf (root-path out-params) value))
                        ((eq key :root-uri) (setf (root-uri out-params) value))
                        ((eq key :process-id) (setf (process-id out-params) value))
                        ((eq key :capabilities) (setf (capabilities out-params) value))
                        ((eq key :trace) (setf (trace-enabled out-params) value))
                        ((eq key :workspace-folders) (setf (workspace-folders out-params) value))
                        (t (error (format nil "Unhandled init request param: ~A" key))))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (make-instance 'request
                                              :id id
                                              :jsonrpc jsonrpc
                                              :params out-params)))))


(defun create-response (id)
    (make-instance 'response :id id))


(defun create-initialized-notification ()
    (make-instance 'initialized))
