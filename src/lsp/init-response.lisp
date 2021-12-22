(defpackage :alive/lsp/init-response
    (:use :cl)
    (:export :create))

(in-package :alive/lsp/init-response)


(defparameter *server-name* "Alive LSP")
(defparameter *server-version* "0.1")


(defparameter *doc-sync-none* 0)
(defparameter *doc-sync-full* 1)
(defparameter *doc-sync-incr* 2)


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


(defclass server-capabilities ()
    ((text-document-sync :accessor text-document-sync
                         :initform (make-instance 'doc-sync-options)
                         :initarg :text-document-sync)))


(defclass result ()
    ((capabilities :accessor capabilities
                   :initform (make-instance 'server-capabilities)
                   :initarg :capabilities)
     #+n (server-info :accessor server-info
                  :initform (make-instance 'server-info
                                           :name *server-name*
                                           :version *server-version*)
                  :initarg :server-info)))


(defun create ()
    (make-instance 'result))
