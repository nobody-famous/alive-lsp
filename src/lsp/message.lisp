(defpackage :alive/lsp/message
    (:use :cl)
    (:export :content-length
             :create-header
             :create-response
             :id
             :params
             :request-payload
             :response-payload
             :to-wire))

(in-package :alive/lsp/message)


(defclass header ()
    ((content-length :accessor content-length
                     :initform nil
                     :initarg :content-length)))


(defclass payload ()
    ((id :accessor id
         :initform nil
         :initarg :id)))


(defclass request-payload (payload)
    ((jsonrpc :accessor jsonrpc
              :initform 2.0
              :initarg :jsonrpc)
     (method-name :accessor method-name
                  :initform nil
                  :initarg :method-name)
     (params :accessor params
             :initform nil
             :initarg :params)))


(defclass response-payload (payload)
    ((result :accessor result
             :initform nil
             :initarg :result)
     (error-info :accessor error-info
                 :initform nil
                 :initarg :error-info)))


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


(defun create-response (req-id result-data &optional (error-data nil))
    (let ((resp (make-instance 'response-payload)))
        (setf (id resp) req-id)

        (cond (result-data (setf (result resp) result-data))
              (error-data (setf (error-info resp) error-data))
              (t (error "No response values")))

        resp))
