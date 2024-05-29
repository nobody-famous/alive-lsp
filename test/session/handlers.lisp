(defpackage :alive/test/session/handlers
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:handlers :alive/session/handlers)))

(in-package :alive/test/session/handlers)


(defun ignore-msg (msg)
    (declare (ignore msg)))


(defun test-types ()
    (clue:test "Types"
        (clue:check-equal :expected NIL
                          :actual (typep 5 'handlers::list-of-handlers))
        (clue:check-equal :expected NIL
                          :actual (typep (list 5) 'handlers::list-of-handlers))
        (clue:check-equal :expected T
                          :actual (typep (list (cons "foo" 'ignore-msg)) 'handlers::list-of-handlers))))


(defun run-all ()
    (clue:suite "Handler Tests"
        (test-types)))
