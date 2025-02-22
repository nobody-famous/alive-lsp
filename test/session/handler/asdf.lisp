(defpackage :alive/test/session/handler/asdf
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:asdf :alive/session/handler/asdf)
                      (:deps :alive/deps)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/asdf)


(defun test-list-all ()
    (clue:test "List All"
        (let ((deps (deps:new-create :list-all-asdf (lambda ()
                                                        (list 1)))))
            (clue:check-exists (gethash "systems"
                                        (gethash "result" (asdf:new-list-all deps (list (cons :id 5)))))))))


(defun test-load-system ()
    (clue:test "Load System"
        (let ((state (state:create))
              (deps (deps:new-create)))
            (asdf:new-load-system deps state (list (cons :id 5))))))


(defun run-all ()
    (clue:suite "ASDF Tests"
        (test-list-all)
        (test-load-system)))
