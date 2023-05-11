(defpackage :alive/test/compile
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/compile)


(defun do-compile (path)
    (let ((msgs (alive/file:do-compile path
                                       :stdout-fn (lambda (data)
                                                      #+n (declare (ignore data))
                                                      (format T "STDOUT ~A~%" data))
                                       :stderr-fn (lambda (data)
                                                      #+n (declare (ignore data))
                                                      (format T "STDERR ~A~%" data)))))

        (format T "MSGS ~A~%" msgs)
        (mapcar (lambda (msg) (gethash "message" msg))
                msgs)))


(defun test-try-compile ()
    (clue:test "Try Compile"
        (clue:check-equal :actual (do-compile "test/files/compile/broken.lisp")
                          :expected (list "The variable B is defined but never used."
                                          "The variable B is defined but never used."))))


(defun run-all ()
    (clue:suite "Compile Tests"
        (test-try-compile)))
