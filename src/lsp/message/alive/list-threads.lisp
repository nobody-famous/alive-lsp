(defpackage :alive/lsp/message/alive/list-threads
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/list-threads)


(defun create-response (id threads)
    (message:create-response id
                             :result-value (list (cons :threads threads))))
