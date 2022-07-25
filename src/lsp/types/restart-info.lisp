(defpackage :alive/lsp/types/restart-info
    (:use :cl)
    (:export :create-item)

    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/restart-info)


(defun create-item (&key name description)
    (let ((item (make-hash-table :test #'equalp)))

        (setf (gethash "name" item) name)
        (setf (gethash "description" item) description)

        item))
