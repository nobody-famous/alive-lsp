(defpackage :alive/test/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:eval :alive/eval)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/eval)


(defun basic ()
    (run:test "Basic Eval"
              (lambda ()
                  (eval:from-string "(+ 1 2)"))))


(defun errors ()
    (run:test "Errors Eval"
              (lambda ()
                  (eval:from-string "(/ 5 0)"))))


(defun run-all ()
    (run:suite "Eval Tests"
               (lambda ()
                   (basic))))
