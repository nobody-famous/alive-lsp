(defpackage :alive/test/location
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:loc :alive/location)))

(in-package :alive/test/location)


(defun test-create ()
    (clue:test "Create"
        (let ((good (loc:create "/some/file" (list (cons :start 5)
                                                   (cons :end 10)))))
            (check-type good loc:text-location))))


(defun test-type ()
    (clue:test "Type"
        (let ((not-object (make-array 5))
              (no-range (list (cons :uri 5)))
              (no-uri (list (cons :range 10))))
            (clue:expect-fail (lambda () (check-type not-object loc:text-location)))
            (clue:expect-fail (lambda () (check-type no-uri loc:text-location)))
            (clue:expect-fail (lambda () (check-type no-range loc:text-location))))))


(defun run-all ()
    (clue:suite "location Tests"
        (test-create)
        (test-type)))
