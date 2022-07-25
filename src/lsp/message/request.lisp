(defpackage :alive/lsp/message/request
    (:use :cl)
    (:export :config
             :debugger)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/request)


(defun config (id &key items)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash "items" params) items)

        (message:create-request id "workspace/configuration" :params params)))


(defun debugger (id &key message restarts stack-trace)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash "message" params) message)
        (setf (gethash "restarts" params) restarts)
        (setf (gethash "stackTrace" params) stack-trace)

        (message:create-request id "$/alive/debugger" :params params)))
