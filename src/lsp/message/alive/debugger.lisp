(defpackage :alive/lsp/message/alive/debugger
    (:use :cl)
    (:export :create-request)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/debugger)


(defun create-request (id &key message restarts stack-trace)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash "message" params) message)
        (setf (gethash "restarts" params) restarts)
        (setf (gethash "stackTrace" params) stack-trace)

        (message:create-request id "$/alive/debugger" :params params)))
