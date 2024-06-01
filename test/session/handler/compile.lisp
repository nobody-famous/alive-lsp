(defpackage :alive/test/session/handler/compile
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:compile :alive/session/handler/compile)
                      (:deps :alive/deps)
                      (:spawn :alive/session/spawn)))

(in-package :alive/test/session/handler/compile)


(defun test-try ()
    (clue:test "Try"
        (let ((sent-msg nil))
            (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                       (setf sent-msg msg)
                                                       nil))
                (compile:try (list (cons :id 5)))
                (clue:check-exists (gethash "result" sent-msg))))))


(defun test-file ()
    (clue:test "File"
        (let ((sent-msg nil))
            (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                       (setf sent-msg msg)
                                                       nil))
                (compile:file (list (cons :id 5)
                                    (cons :params (list (cons :path "some/path")))))
                (clue:check-exists (gethash "result" sent-msg))))))


(defun run-all ()
    (clue:suite "Compile Tests"
        (test-try)
        (test-file)))
