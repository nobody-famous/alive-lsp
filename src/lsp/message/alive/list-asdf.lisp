(defpackage :alive/lsp/message/alive/list-asdf
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/list-asdf)


(defun create-response (id systems)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "systems" data) systems)

        (message:create-response id :result-value data)))
