(defpackage :alive/test/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/eval)))

(in-package :alive/test/eval)


(defun test-basic ()
    (clue:test "Basic Eval"
        (let ((fn-called nil))
            (deps:with-deps (deps:create :eval-fn (lambda (s)
                                                      (declare (ignore s))
                                                      (setf fn-called T)))
                (eval:from-string "(+ 1 2)")
                (clue:check-equal :expected T
                                  :actual fn-called)))))


(defun run-all ()
    (clue:suite "Eval Tests"
        (test-basic)))
