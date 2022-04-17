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
                  (check:are-equal 3 (eval:from-string "(+ 1 2)")))))


(defun errors ()
    (run:test "Errors Eval"
              (lambda ()
                  (handler-bind ((T (lambda (c)
                                        (format T "CAUGHT ~A~%" c)
                                        (format T "EXIT ~A~%" (find-restart 'exit))
                                        (loop :for item :in (compute-restarts c) :do
                                                  (format T "RESTART ~A ~A~%" (restart-name item) item)))))
                      (eval:from-string "(/ 5 0)")))))


(defun run-all ()
    (run:suite "Eval Tests"
               (lambda ()
                   (basic))))
