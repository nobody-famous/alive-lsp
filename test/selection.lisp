(defpackage :alive/test/selection
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:selection :alive/selection)))

(in-package :alive/test/selection)


(defun test-forms ()
    (clue:suite "Forms Tests"
        (clue:test "Nested Selection"
            (let ((forms (forms:from-stream (make-string-input-stream (format nil "(foo bar (baz bong))~%(checking foo)"))))
                  (pos-list (list (pos:create 0 12))))
                (clue:check-equal :expected (list (list (cons :range (range:create (pos:create 0 10) (pos:create 0 13)))
                                                        (cons :parent (list (cons :range (range:create (pos:create 0 9) (pos:create 0 19)))
                                                                            (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 0 20)))
                                                                                                (cons :parent nil)))))))
                                  :actual (selection:ranges forms pos-list))))

        (clue:test "Second Nested Selection"
            (let ((forms (forms:from-stream (make-string-input-stream (format nil "(foo bar (baz bong))~%(checking foo)"))))
                  (pos-list (list (pos:create 1 11))))
                (clue:check-equal :expected (list (list (cons :range (range:create (pos:create 1 10) (pos:create 1 13)))
                                                        (cons :parent (list (cons :range (range:create (pos:create 1 0) (pos:create 1 14)))
                                                                            (cons :parent nil)))))
                                  :actual (selection:ranges forms pos-list))))

        (clue:test "One Form"
            (let ((forms (forms:from-stream (make-string-input-stream "foo")))
                  (pos-list (list (pos:create 0 2))))
                (clue:check-equal :expected (list (list (cons :range (range:create (pos:create 0 0) (pos:create 0 3)))
                                                        (cons :parent nil)))
                                  :actual (selection:ranges forms pos-list))))

        (clue:test "Out of range"
            (let ((forms (forms:from-stream (make-string-input-stream "foo")))
                  (pos-list (list (pos:create -1 -5))))
                (clue:check-equal :expected (list nil)
                                  :actual (selection:ranges forms pos-list))))))


(defun test-failure ()
    (clue:test "Failures"
        (let ((forms (forms:from-stream (make-string-input-stream "foo"))))
            (clue:expect-fail (lambda ()
                                  (selection:ranges forms 5))))))


(defun run-all ()
    (clue:suite "Selection Tests"
        (test-forms)
        (test-failure)))
