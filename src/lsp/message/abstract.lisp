(defpackage :alive/lsp/message/abstract
    (:use :cl)
    (:export :create-error
             :create-notification
             :create-request
             :create-response))

(in-package :alive/lsp/message/abstract)


(defun create-request (id name &key params)
    (let ((req (make-hash-table :test #'equalp)))

        (setf (gethash "id" req) id)
        (setf (gethash "jsonrpc" req) "2.0")
        (setf (gethash "method" req) name)
        (setf (gethash "params" req) params)

        req))


(defun create-notification (name &key params)
    (let ((req (make-hash-table :test #'equalp)))

        (setf (gethash "jsonrpc" req) "2.0")
        (setf (gethash "method" req) name)
        (setf (gethash "params" req) params)

        req))


(defun create-response (id &key result-value error-value)
    (let ((resp (make-hash-table :test #'equalp)))

        (setf (gethash "id" resp) id)
        (setf (gethash "jsonrpc" resp) "2.0")

        (cond ((and error-value result-value) (error "Cannot create response with result and error"))
              (result-value (setf (gethash "result" resp) result-value))
              (error-value (setf (gethash "error" resp) error-value)))

        resp))


(defun create-error (id &key code message)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "code" data) code)
        (setf (gethash "message" data) message)

        (create-response id :error-value data)))
