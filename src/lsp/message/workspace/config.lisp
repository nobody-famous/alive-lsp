(defpackage :alive/lsp/message/workspace/config
    (:use :cl)
    (:export :create-request)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/workspace/config)


(defun create-request (id &key items)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash "items" params) items)

        (message:create-request id "workspace/configuration" :params params)))
