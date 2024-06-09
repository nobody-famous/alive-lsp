(defpackage :alive/test/context
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:context :alive/context)))

(in-package :alive/test/context)


(defun test-create ()
    (clue:test "Create"
        (let ((destroy-called nil))
            (context:with-context (:input-stream 5
                                                 :output-stream 10
                                                 :destroy-fn (lambda ()
                                                                 (setf destroy-called T)))
                (clue:check-equal :expected 5
                                  :actual (context:get-input-stream))
                (clue:check-equal :expected 10
                                  :actual (context:get-output-stream)))
            (clue:check-equal :expected nil
                              :actual destroy-called))))


(defun test-failure ()
    (clue:test "Context Failures"
        (clue:expect-fail (lambda () (context:get-input-stream)))
        (clue:expect-fail (lambda () (context:get-output-stream)))))


(defun run-all ()
    (clue:suite "Context Tests"
        (test-create)
        (test-failure)))
