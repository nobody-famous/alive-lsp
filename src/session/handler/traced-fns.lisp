(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:state :alive/session/state)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(declaim (ftype (function (string cons) (values (or null string) (or null string))) get-function-for-pos))
(defun get-function-for-pos (text pos)
    (declare (ignore text pos))

    #+n (loop :for token :in (tokenizer:from-stream (make-string-input-stream text))
              :do (alive/test/utils:print-hash-table "***** TOKEN" token)
              :finally (return nil))

    (values nil nil))


(declaim (ftype (function (deps:dependencies(or null string) (or null string)) boolean) do-trace-fn))
(defun do-trace-fn (deps pkg-name fn-name)
    (let ((*package* (if pkg-name
                         (packages:for-string pkg-name)
                         *package*)))
        (deps:trace-fn deps fn-name)))


(declaim (ftype (function (deps:dependencies state:state cons) hash-table) trace-fn))
(defun trace-fn (deps state msg)
    (declare (ignore deps))
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) "")))

        (multiple-value-bind (pkg-name fn-name)
                (get-function-for-pos text pos)
            (declare (ignore pkg-name fn-name))
            (lsp-msg:create-response id :result-value T))))


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (deps:list-all-traced deps)))
