(defpackage :alive/lsp/message/alive/do-inspect
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/do-inspect)


(defun create-response (id &key insp-id result)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "id" data) insp-id)
        (setf (gethash "result" data) result)

        (message:create-response id :result-value data)))
