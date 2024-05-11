(defpackage :alive/session/handler/utils
    (:use :cl)
    (:export :result)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)))

(in-package :alive/session/handler/utils)


(declaim (ftype (function (fixnum string T) hash-table) result))
(defun result (id key value)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash key data) value)
        (lsp-msg:create-response id :result-value data)))
