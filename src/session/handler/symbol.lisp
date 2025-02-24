(defpackage :alive/session/handler/symbol
    (:use :cl)
    (:export :new-do-unexport
             :new-for-pos)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:refresh :alive/session/refresh)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/symbol)


(declaim (ftype (function (state:state list) hash-table) new-for-pos))
(defun new-for-pos (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (result (alive/lsp/symbol:for-pos :text text :pos pos)))

        (utils:result id "value" result)))


(declaim (ftype (function (alive/deps:dependencies state:state list) hash-table) new-do-unexport))
(defun new-do-unexport (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (sym-name (cdr (assoc :symbol params)))
           (pkg-name (cdr (assoc :package params))))

        (packages:unexport-symbol pkg-name sym-name)
        (refresh:new-send deps state)
        (lsp-msg:create-response id :result-value T)))
