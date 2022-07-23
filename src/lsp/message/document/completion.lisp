(defpackage :alive/lsp/message/document/completion
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/document/completion)


(defun create-response (id &key items)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "items" data) items)
        (message:create-response id :result-value data)))
