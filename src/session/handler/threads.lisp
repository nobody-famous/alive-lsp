(defpackage :alive/session/handler/threads
    (:use :cl)
    (:export :list-all)
    (:local-nicknames (:state :alive/session/state)
                      (:threads :alive/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/threads)


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (state:lock (mutex)
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (threads:get-thread-id (bt:current-thread))))
                               (threads:list-all))))

            (utils:result (cdr (assoc :id msg)) "threads" threads))))
