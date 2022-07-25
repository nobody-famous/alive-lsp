(defpackage :alive/lsp/types/config-item
    (:use :cl)
    (:export :create-item))

(in-package :alive/lsp/types/config-item)


(defun create-item (&key scope-uri section)
    (let ((item (make-hash-table :test #'equalp)))

        (setf (gethash "scopeUri" item) scope-uri)
        (setf (gethash "section" item) section)

        item))
