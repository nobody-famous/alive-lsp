(defpackage :alive/test/compile
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/compile)


(defun do-compile (path)
    (let ((msgs (alive/file:try-compile path
                                        :stdout-fn (lambda (data)
                                                       (declare (ignore data)))
                                        :stderr-fn (lambda (data)
                                                       (declare (ignore data))))))

        (mapcar (lambda (msg) (gethash "severity" msg))
                msgs)))


(defun test-try-compile ()
    (clue:test "Try Compile"
        (clue:check-equal :actual (do-compile "test/files/compile/broken.lisp")
                          :expected (list alive/types:*sev-error*
                                          alive/types:*sev-warn*))))


(defun run-all ()
    (clue:suite "Compile Tests"
        (test-try-compile)))
