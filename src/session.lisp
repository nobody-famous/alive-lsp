(defpackage :alive/session
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:logger :alive/logger)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session)


(defun stop (state)
    (logger:info-msg "Stopping session")
    (state:set-running state nil))


(declaim (ftype (function (alive/deps:dependencies state:state) null) start))
(defun start (deps state)
    (state:set-running state T)

    (spawn:new-thread "Session Message Reader"
        (alive/session/message-loop:run deps state))

    (logger:info-msg "Session started")
    nil)
