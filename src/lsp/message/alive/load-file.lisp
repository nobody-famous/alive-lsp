(defpackage :alive/lsp/message/alive/load-file
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/load-file)


(defun create-response (id msgs)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "messages" data) msgs)

        (message:create-response id
                                 :result-value data)))
