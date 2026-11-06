(defpackage :alive/session/handler/symbol
    (:use :cl)
    (:export :do-unexport
             :for-pos)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:refresh :alive/session/refresh)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/symbol)


(defun for-pos (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (forms (state:get-file-forms state uri))
           (result (alive/lsp/symbol:for-pos :forms forms :pos pos)))

        (utils:result id "value" result)))


(defun do-unexport (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (sym-name (cdr (assoc :symbol params)))
           (pkg-name (cdr (assoc :package params))))

        (packages:unexport-symbol pkg-name sym-name)
        (refresh:send deps state)
        (lsp-msg:create-response id :result-value T)))
