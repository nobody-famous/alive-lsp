(defpackage :alive/lsp/message/initialize
    (:use :cl)
    (:export :create-client-info
             :create-capabilities
             :create-legend
             :create-request
             :create-request-params
             :create-response
             :create-sem-tokens-opts
             :create-initialized-notification
             :initialized
             :request-from-wire
             :request)
    (:local-nicknames (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

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


(defun create-request (&key id (jsonrpc "2.0") params)
    (make-instance 'request
        :id id
        :jsonrpc jsonrpc
        :params params))


(defun create-request-params (&key client-info locale root-path root-uri process-id
                                   capabilities trace-enabled workspace-folders)
    (make-instance 'params
        :client-info client-info
        :locale locale
        :root-path root-path
        :root-uri root-uri
        :process-id process-id
        :capabilities capabilities
        :trace-enabled trace-enabled
        :workspace-folders workspace-folders))


(defun create-client-info (&key name version)
    (make-instance 'client-info
        :name name
        :version version))


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
        ((token-types :accessor token-types
                      :initform sem-tokens:*types*
                      :initarg :token-types)
         (token-modifiers :accessor token-modifiers
                          :initform sem-tokens:*mods*
                          :initarg :token-modifiers)))


(defmethod print-object ((obj sem-tokens-legend) out)
    (format out "{types: ~A; modifiers: ~A}"
        (token-types obj)
        (token-modifiers obj)))


(defun create-legend (&key types modifiers)
    (make-instance 'sem-tokens-legend
        :token-types types
        :token-modifiers modifiers))


(defclass sem-tokens-opts ()
        ((legend :accessor legend
                 :initform (make-instance 'sem-tokens-legend)
                 :initarg :legend)
         (full :accessor full
               :initform T
               :initarg :full)))


(defmethod print-object ((obj sem-tokens-opts) out)
    (format out "{legend: ~A; full: ~A}"
        (legend obj)
        (full obj)))


(defun create-sem-tokens-opts (&key legend full)
    (make-instance 'sem-tokens-opts
        :legend legend
        :full full))


(defclass completion-opts ()
        ((trigger-characters :accessor trigger-characters
                             :initform (list #\:)
                             :initarg :trigger-characters)))


(defclass on-type-opts ()
        ((first-trigger-character :accessor first-trigger-character
                                  :initform #\newline
                                  :initarg :first-trigger-character)
         (more-trigger-characters :accessor more-trigger-characters
                                  :initform (list)
                                  :initarg :more-trigger-characters)))


(defclass server-capabilities ()
        ((text-document-sync :accessor text-document-sync
                             :initform *doc-sync-full*
                             :initarg :text-document-sync)
         (hover-provider :accessor hover-provider
                         :initform nil
                         :initarg :hover-provider)
         (semantic-tokens-provider :accessor semantic-tokens-provider
                                   :initform (make-instance 'sem-tokens-opts)
                                   :initarg :semantic-tokens-provider)
         (completion-provider :accessor completion-provider
                              :initform (make-instance 'completion-opts)
                              :initarg :completion-provider)
         (document-range-formatting-provider :accessor document-range-formatting-provider
                                             :initform T
                                             :initarg :document-range-formatting-provider)
         (document-on-type-formatting-provider :accessor document-on-type-formatting-provider
                                               :initform (make-instance 'on-type-opts)
                                               :initarg :document-on-type-formatting-provider)))


(defmethod print-object ((obj server-capabilities) out)
    (format out "{text-document-sync: ~A; hover-provider: ~A; semantic-tokens-provider: ~A}"
        (text-document-sync obj)
        (hover-provider obj)
        (semantic-tokens-provider obj)))


(defun create-capabilities (&key text-doc-sync hover-provider sem-tokens-provider)
    (make-instance 'server-capabilities
        :text-document-sync text-doc-sync
        :hover-provider hover-provider
        :semantic-tokens-provider sem-tokens-provider))


(defclass response-body ()
        ((capabilities :accessor capabilities
                       :initform (make-instance 'server-capabilities)
                       :initarg :capabilities)))


(defmethod print-object ((obj response-body) out)
    (format out "{capabilities: ~A}" (capabilities obj)))


(defclass response (message:result-response)
        ((message:result :initform (make-instance 'response-body))))


(defmethod print-object ((obj response) out)
    (format out "{result: ~A}" (message:result obj)))


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
