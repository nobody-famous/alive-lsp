(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (deps:list-all-traced deps)))
