(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(declaim (ftype (function (deps:dependencies state:state cons) hash-table) trace-fn))
(defun trace-fn (deps state msg)
    (declare (ignore deps))
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (tokens (tokenizer:from-stream (make-string-input-stream text))))
        (declare (ignore pos tokens))

        (utils:result id "traced" nil)))


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (deps:list-all-traced deps)))
