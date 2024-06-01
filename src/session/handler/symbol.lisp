(defpackage :alive/session/handler/symbol
    (:use :cl)
    (:export :for-pos)
    (:local-nicknames (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/symbol)


(declaim (ftype (function (list) hash-table) for-pos))
(defun for-pos (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text uri) ""))
           (result (alive/lsp/symbol:for-pos :text text :pos pos)))

        (utils:result id "value" result)))
