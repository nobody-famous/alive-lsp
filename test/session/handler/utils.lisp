(defpackage :alive/test/session/handler/utils
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:utils :alive/session/handler/utils)))

(in-package :alive/test/session/handler/utils)


(defun test-result ()
    (clue:test "Create Result"
        (let ((result (utils:result 5 "foo" "bar")))
            (clue:check-exists (gethash "result" result)))))


(defun run-all ()
    (clue:suite "Handler Utils Tests"
        nil))
