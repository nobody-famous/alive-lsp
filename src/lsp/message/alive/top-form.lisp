(defpackage :alive/lsp/message/alive/top-form
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/top-form)


(defun create-response (id &key start end)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "start" data) start)
        (setf (gethash "end" data) end)

        (message:create-response id
                                 :result-value data)))
