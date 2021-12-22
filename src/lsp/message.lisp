(defpackage :alive/lsp/message
    (:use :cl)
    (:export :content-length
             :create-header
             :create-result
             :id
             :params
             :request-payload
             :result-payload
             :error-payload
             :to-wire))

(in-package :alive/lsp/message)


(defclass header ()
    ((content-length :accessor content-length
                     :initform nil
                     :initarg :content-length)))


(defclass payload ()
    ((jsonrpc :accessor jsonrpc
              :initform "2.0"
              :initarg :jsonrpc)
     (id :accessor id
         :initform nil
         :initarg :id)))


(defclass request-payload (payload)
    ((method :accessor method-name
             :initform nil
             :initarg :method)
     (params :accessor params
             :initform nil
             :initarg :params)))


(defclass result-payload (payload)
    ((result :accessor result
             :initform nil
             :initarg :result)))


(defclass error-payload (payload)
    ((error :accessor error-info
            :initform nil
            :initarg :error)))


(defclass message ()
    ((header :accessor header
             :initform (make-instance 'message-header)
             :initarg :header)
     (payload :accessor payload
              :initform nil
              :initarg :payload)))


(defun to-wire (msg)
    (with-output-to-string (str)
        (let* ((end-line (format nil "~C~C" #\return #\newline))
               (payload (json:encode-json-to-string msg)))
            (format str "Content-Length ~A~A" (length payload) end-line)
            (format str "~A" end-line)
            (format str "~A" payload))))


(defun create-header ()
    (make-instance 'header))


(defun create-result (req-id result-data)
    (make-instance 'result-payload
                   :id req-id
                   :result result-data))
