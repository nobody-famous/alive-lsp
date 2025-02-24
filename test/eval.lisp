(defpackage :alive/test/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/sys/eval)))

(in-package :alive/test/eval)


(defun test-basic ()
    (clue:test "Basic Eval"
        (let* ((fn-called nil)
               (deps (deps:create :eval-fn (lambda (s)
                                                   (declare (ignore s))
                                                   (setf fn-called T)))))
            (eval:new-from-string deps "(+ 1 2)")
            (clue:check-equal :expected T
                              :actual fn-called))))


(defun test-no-package ()
    (clue:test "No package"
        (let ((deps (deps:create)))
            (clue:expect-fail (lambda () (eval:new-from-string deps "(+ 1 2)" :pkg-name "foo"))))))


(defun run-all ()
    (clue:suite "Eval Tests"
        (test-basic)
        (test-no-package)))
