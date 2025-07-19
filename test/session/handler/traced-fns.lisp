(defpackage :alive/test/session/handler/traced-fns
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:handler :alive/session/handler/traced-fns)))

(in-package :alive/test/session/handler/traced-fns)


(defun test-list-all ()
    (clue:test "List All"
        (let ((result (gethash "result" (handler:list-all (deps:create :list-all-traced (lambda () (list (cons :a "b"))))
                                                          (list (cons :id 5))))))
            (clue:check-exists (gethash "traced" result)))))


(defun run-all ()
    (clue:suite "Traced Functions Tests"
        (test-list-all)))