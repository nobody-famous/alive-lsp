(defpackage :alive/test/debugger
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:debugger :alive/debugger)))

(in-package :alive/test/debugger)


(defun bomb ()
    (when t
          (format T "before break~%")
          (break)))


(defun test-forms ()
    (clue:test "Forms Test"
        (block test
            (handler-bind ((sb-impl::step-form-condition (lambda (c)
                                                             (format T "CAUGHT STEP ~A~%" c)
                                                             (return-from test)))
                           (condition (lambda (c)
                                          (format T "CAUGHT COND ~A~%" c)
                                          (return-from test))))
                (format T "before bomb~%")
                (bomb)
                (format T "after bomb~%")))))


(defun run-all ()
    (clue:suite "Debugger Tests"
        (test-forms)))
