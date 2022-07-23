(defpackage :alive/lsp/message/alive/list-packages
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/alive/list-packages)


(defun create-response (id packages)
    (let ((pkgs (make-hash-table)))
        (setf (gethash "packages" pkgs) packages)
        (message:create-response id
                                 :result-value pkgs)))
