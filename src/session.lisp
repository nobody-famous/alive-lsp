(defpackage :alive/session
    (:use :cl)
    (:export :new-start
             :new-stop)
    (:local-nicknames (:logger :alive/logger)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session)


(defun new-stop (state)
    (logger:info-msg "Stopping session")
    (state:set-running state nil))


(declaim (ftype (function (alive/deps:dependencies state:state) null) new-start))
(defun new-start (deps state)
    (state:set-running state T)

    (spawn:new-thread "Session Message Reader"
        (alive/session/message-loop:new-run deps state))

    (logger:info-msg "Session started")
    nil)
