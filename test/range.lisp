(defpackage :alive/test/range
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:range :alive/range)
                      (:pos :alive/position)))

(in-package :alive/test/range)


(defun test-create ()
    (clue:test "Create"
        (let ((good (range:create (pos:create 5 10) (pos:create 10 15))))
            (check-type good range:text-range))))


(defun test-type ()
    (clue:test "Type"
        (let ((not-object (make-array 5))
              (no-line (list (cons :line 5)))
              (no-character (list (cons :character 10))))
            (clue:expect-fail (lambda () (check-type not-object range:text-range)))
            (clue:expect-fail (lambda () (check-type no-line range:text-range)))
            (clue:expect-fail (lambda () (check-type no-character range:text-range))))))


(defun run-all ()
    (clue:suite "Range Tests"
        (test-create)
        (test-type)))
