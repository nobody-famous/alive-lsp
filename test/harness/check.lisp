(defpackage :alive/test/harness/check
    (:use :cl)
    (:export :are-equal)
    (:local-nicknames (:err :alive/test/harness/errors)))

(in-package :alive/test/harness/check)


(defun are-equal (expected actual)
    (unless (equalp expected actual)
            (error 'err:test-failed
                   :expected expected
                   :actual actual)))
