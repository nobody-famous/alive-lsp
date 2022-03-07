(defpackage :alive/test/session/state
    (:use :cl)
    (:export :create)
    (:local-nicknames (:session :alive/session)))

(in-package :alive/test/session/state)


(defclass test-state (session::state)
    ())


(defun create ()
    nil)
