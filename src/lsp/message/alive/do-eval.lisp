(defpackage :alive/lsp/message/alive/do-eval
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/do-eval)


(defun create-response (id text)
    (let ((data (make-hash-table)))
        (setf (gethash "text" data) text)
        (message:create-response id
                                 :result-value data)))
