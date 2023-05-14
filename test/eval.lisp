(defpackage :alive/test/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:eval :alive/eval)))

(in-package :alive/test/eval)


(defun test-basic ()
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


(defconstant test-string "Test data")


(defun test-stdin ()
    (clue:test "Stdin Eval"
        (clue:check-equal :expected test-string
                          :actual (eval:from-string "(read-line)"
                                                    :stdin-fn (lambda ()
                                                                  test-string)))))


(defun test-stdout ()
    (clue:test "Stdout Eval"
        (let ((text nil))
            (eval:from-string (format nil "(format T \"~A\")" test-string)
                              :stdout-fn (lambda (data)
                                             test-string))
            (clue:check-equal :expected test-string
                              :actual text))))


(defun run-all ()
    (clue:suite "Eval Tests"
        (test-basic)
        (test-stdin)))