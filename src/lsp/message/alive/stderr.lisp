(defpackage :alive/lsp/message/alive/stderr
    (:use :cl)
    (:export :create)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/stderr)


(defun create (data)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash "data" params) data)

        (message:create-notification "$/alive/stderr" :params params)))
