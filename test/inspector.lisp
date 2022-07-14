(defpackage :alive/test/inspector
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:inspector :alive/inspector)
                      (:eval :alive/eval)))

(in-package :alive/test/inspector)


(defun fn-test ()
    (clue:test "Inspect Function Test"
        (let* ((result (eval:from-string "'defun"
                                         :pkg-name "cl-user"))
               (insp-result (inspector:to-result result)))

            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 3
                              :actual (hash-table-count insp-result)))))


(defun run-all ()
    (clue:suite "Inspector Tests"
        (fn-test)))
