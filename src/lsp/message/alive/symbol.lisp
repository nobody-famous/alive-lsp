(defpackage :alive/lsp/message/alive/symbol
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/symbol)


(defun create-response (id &key value)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "value" data) value)

        (message:create-response id :result-value data)))
