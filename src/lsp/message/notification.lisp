(defpackage :alive/lsp/message/notification
    (:use :cl)
    (:export :query
             :refresh
             :stderr
             :stdout)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/notification)


(defun create (name &key key value)
    (let ((params (make-hash-table :test #'equalp)))

        (setf (gethash key params) value)

        (message:create-notification name :params params)))


(defun refresh ()
    (message:create-notification "$/alive/refresh"
                                 :params (make-hash-table)))


(defun stdout (value)
    (create "$/alive/stdout"
            :key "data"
            :value value))


(defun stderr (value)
    (create "$/alive/stderr"
            :key "data"
            :value value))


(defun query (value)
    (create "$/alive/query-io"
            :key "data"
            :value value))
