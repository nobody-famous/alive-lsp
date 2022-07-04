(defpackage :alive/test/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:eval :alive/eval)))

(in-package :alive/test/eval)


(defun basic ()
    (clue:test "Basic Eval"
        (clue:check-equal :expected 3
                          :actual (eval:from-string "(+ 1 2)"))))


(defun errors ()
    (clue:test "Errors Eval"
        (handler-bind ((T (lambda (c)
                              (format T "CAUGHT ~A~%" c)
                              (format T "EXIT ~A~%" (find-restart 'exit))
                              (loop :for item :in (compute-restarts c) :do
                                        (format T "RESTART ~A ~A~%" (restart-name item) item)))))
            (eval:from-string "(/ 5 0)"))))


(defun stdin ()
    (clue:test "Stdin Eval"
        (eval:from-string "(read-line)"
                          :stdin-fn (lambda ()
                                        (format T "STDIN-FN CALLED~%"))
                          :stdout-fn (lambda (data)
                                         (format T "STDOUT ~A~%" data)))))


(defun run-all ()
    (clue:suite "Eval Tests"
        (basic)))