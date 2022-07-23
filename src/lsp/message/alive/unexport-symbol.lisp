(defpackage :alive/lsp/message/alive/unexport-symbol
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/unexport-symbol)


(defun create-response (id)
    (message:create-response id :result-value T))
