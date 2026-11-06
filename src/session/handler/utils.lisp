(defpackage :alive/session/handler/utils
    (:use :cl)
    (:export :result)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/utils)


(defun result (id key value)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash key data) value)
        (lsp-msg:create-response id :result-value data)))
