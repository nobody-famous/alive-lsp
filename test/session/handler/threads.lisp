(defpackage :alive/test/session/handler/threads
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)
                      (:threads :alive/session/handler/threads)))

(in-package :alive/test/session/handler/threads)


(defun test-list-all ()
    (clue:test "List all"
        (clue:expect-fail (lambda () (threads:list-all (list (cons :id 5)))))
        (state:with-state (state:create)
            (clue:check-exists (gethash "result" (threads:list-all (list (cons :id 5))))))))


(defun run-all ()
    (clue:suite "Threads Handler Tests"
        (test-list-all)))
