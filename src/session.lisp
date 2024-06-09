(defpackage :alive/session
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:logger :alive/logger)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session)


(defun stop ()
    (logger:info-msg "Stopping session")

    (state:set-running nil)

    (alive/context:destroy))


(declaim (ftype (function () null) start))
(defun start ()
    (state:with-state (state:create)
        (state:add-listener (state:create-listener (lambda () (alive/context:destroy))))
        (state:set-running T)

        (spawn:new-thread "Session Message Reader"
            (alive/session/message-loop:run))

        (logger:info-msg "Session started")
        nil))
