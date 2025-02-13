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
        (clue:expect-fail (lambda () (init:initialized (list (cons :id 5)))))
        (state:with-state (state:create)
            (init:initialized (list (cons :id 5)))
            (clue:check-equal :expected T
                              :actual (state:initialized)))))


(defun run-all ()
    (clue:suite "Init Handler Tests"
        (test-request)
        (test-initialized)))
