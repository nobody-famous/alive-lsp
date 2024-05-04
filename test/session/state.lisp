(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)))

(in-package :alive/test/session/state)


(defun test-add-history ()
    (clue:suite "History"
        (clue:test "Add"
            (let ((state (make-instance 'state:state)))
                (state:add-history state 5)
                (clue:check-equal :expected 5
                                  :actual (state:get-history-item state 0))))

        (clue:test "Get out of range"
            (let ((state (make-instance 'state:state)))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state 10))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state -10))))))


(defun run-all ()
    (clue:suite "Session State Tests"
        (test-add-history)))
