(defpackage :alive/lsp/message/alive/list-threads
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/list-threads)


(defun create-response (id threads)
    (let ((data (make-hash-table)))
        (setf (gethash "threads" data) threads)
        (message:create-response id
                                 :result-value data)))
