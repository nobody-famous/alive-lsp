(defpackage :alive/test/log
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/test/log)


(defun test-create ()
    (clue:test "Create"
        (let ((log (logger:create *standard-output*)))
            (clue:check-equal :expected logger:*error*
                              :actual (logger:has-level log logger:*error*)))))


(defun run-all ()
    (clue:suite "Logger Tests"
        (test-create)))
