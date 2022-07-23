(defpackage :alive/lsp/message/alive/get-pkg
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/get-pkg)


(defun create-response (id &key pkg-name)
    (let ((data (make-hash-table)))
        (setf (gethash "package" data) pkg-name)
        (message:create-response id
                                 :result-value data)))
