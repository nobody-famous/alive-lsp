(defpackage :alive/test/session/handler/inspect
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:inspect :alive/session/handler/inspect)))

(in-package :alive/test/session/handler/inspect)


(defun test-do-inspect ()
    (clue:test "Do inspect"
        (clue:expect-fail (lambda () (inspect:do-inspect (list (cons :id 5)
                                                               (cons :params (list (cons :text "foo")))))))

        (deps:with-deps (deps:create)
            (inspect:do-inspect (list (cons :id 5)
                                      (cons :params (list (cons :text "foo"))))))))


(defun run-all ()
    (clue:suite "Inspect Tests"
        (test-do-inspect)))
