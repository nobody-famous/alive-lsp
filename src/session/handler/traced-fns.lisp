(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(defun get-function-for-pos (text pos)
    (declare (ignore pos))

    (loop :for token :in (tokenizer:from-stream (make-string-input-stream text))
          :do (alive/test/utils:print-hash-table "***** TOKEN" token)
          :finally (return nil)))


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
            nil)

        (utils:result id "traced" nil)))


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (deps:list-all-traced deps)))
