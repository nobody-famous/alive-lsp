(defpackage :alive/test/session
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:session :alive/session)
                      (:utils :alive/test/utils)))

(in-package :alive/test/session)


(defclass test-state (session::state)
        ((msg :accessor msg
              :initform nil
              :initarg :msg)))


(defmethod session::get-input-stream ((obj test-state))
    (let ((to-return (when (msg obj)
                           (msg obj))))
        (utils:stream-from-string to-return)))


(defmethod session::destroy ((obj test-state))
    nil)


(defun test-start ()
    (clue:test "Start Test"
        (alive/logger:with-logging (alive/logger:create *standard-output* alive/logger:*info*)
            (let ((s (make-instance 'test-state :msg "foo")))
                (session:start s)
                (session:stop s)))))


(defun run-all ()
    (clue:suite "Session Tests"
        (test-start)))
