(defpackage :alive/test/session/handler/init
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:init :alive/session/handler/init)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/init)


(defun test-request ()
    (clue:test "Request"
        (clue:expect-fail (lambda () (init:request (list (cons :id "foo")))))
        (clue:check-exists (init:request (list (cons :id 5))))))


(defun test-initialized ()
    (clue:test "Initialized"
        (let ((state (state:create)))
            (init:new-initialized state (list (cons :id 5)))
            (clue:check-equal :expected T
                              :actual (state:new-initialized state)))))


(defun run-all ()
    (clue:suite "Init Handler Tests"
        (test-request)
        (test-initialized)))
