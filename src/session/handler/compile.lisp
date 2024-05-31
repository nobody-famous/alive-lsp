(defpackage :alive/session/handler/compile
    (:use :cl)
    (:export :try)
    (:local-nicknames (:deps :alive/deps)
                      (:spawn :alive/session/spawn)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/compile)


(declaim (ftype (function (list) null) try))
(defun try (msg)
    (spawn:new-thread "Try Compile"
        (let* ((params (cdr (assoc :params msg)))
               (path (cdr (assoc :path params)))
               (msgs (handler-case
                             (deps:try-compile path)
                         (T () nil))))
            (deps:send-msg (utils:result (cdr (assoc :id msg)) "messages" msgs)))))
