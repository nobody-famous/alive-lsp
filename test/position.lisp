(defpackage :alive/test/position
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:pos :alive/position)))

(in-package :alive/test/position)


(defun test-create ()
    (clue:test "Create"
        (let ((good (pos:create 5 10)))
            (check-type good pos:text-position))))


(defun test-type ()
    (clue:test "Type"
        (let ((not-object (make-array 5))
              (no-line (list (cons :line 5)))
              (no-character (list (cons :character 10))))
            (clue:expect-fail (lambda () (check-type not-object pos:text-position)))
            (clue:expect-fail (lambda () (check-type no-line pos:text-position)))
            (clue:expect-fail (lambda () (check-type no-character pos:text-position))))))


(defun run-all ()
    (clue:suite "Position Tests"
        (test-create)
        (test-type)))
