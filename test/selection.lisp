(defpackage :alive/test/selection
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:pos :alive/position)
                      (:selection :alive/selection)))

(in-package :alive/test/selection)


(defun basic ()
    (clue:test "Basic Selection"
        (let ((forms (forms:from-stream (make-string-input-stream (format nil "(foo bar (baz bong))~%(checking foo)"))))
              (pos-list (list (pos:create 0 12))))
            (selection:ranges forms pos-list))))


(defun no-forms ()
    (clue:test "Selection No Forms"
        (let ((forms (forms:from-stream (make-string-input-stream "foo")))
              (pos-list (list (pos:create 0 2))))
            (selection:ranges forms pos-list))))


(defun run-all ()
    (clue:suite "Selection Tests"
        (basic)))
