(defpackage :alive/session/handler/utils
    (:use :cl)
    (:export :result
             :run-in-thread)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/utils)


(declaim (ftype (function (fixnum string T) hash-table) result))
(defun result (id key value)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash key data) value)
        (lsp-msg:create-response id :result-value data)))
