(defpackage :alive/session/message
    (:use :cl)
    (:export :handle)
    (:local-nicknames (:errors :alive/lsp/errors)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/message)


(declaim (ftype (function (cons) (values (or null hash-table) &optional)) handle))
(defun handle (msg)
    (cond ((assoc :method msg) nil)
          ((or (assoc :result msg)
               (assoc :error msg)) nil)
          (T (lsp-msg:create-error (assoc :id msg)
                                   :code errors:*request-failed*
                                   :message (format nil "No handler for message")))))
