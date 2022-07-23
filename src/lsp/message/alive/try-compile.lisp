(defpackage :alive/lsp/message/alive/try-compile
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/try-compile)


(defun create-response (id msgs)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "messages" data) msgs)

        (message:create-response id :result-value data)))
