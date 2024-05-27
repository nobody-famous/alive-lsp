(defpackage :alive/session/handler/asdf
    (:use :cl)
    (:export :list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/asdf)


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (utils:result (cdr (assoc :id msg))
                  "systems"
                  (deps:list-all-asdf)))
