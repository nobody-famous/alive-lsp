(defpackage :alive/test/session/deps
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/session/deps)))

(in-package :alive/test/session/deps)


(defun test-msg-handler ()
    (clue:test "Message handler"
        (clue:expect-fail (lambda () (deps:msg-handler)))
        (deps:with-deps (deps:create :msg-handler (lambda (msg) (declare (ignore msg))))
            (clue:check-equal :expected T
                              :actual (functionp (deps:msg-handler))))))


(defun test-read-msg ()
    (clue:test "Read message"
        (clue:expect-fail (lambda () (deps:read-msg)))
        (deps:with-deps (deps:create :read-msg (lambda () (list 1 2)))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:read-msg)))
        (deps:with-deps (deps:create)
            (clue:check-equal :expected nil
                              :actual (deps:read-msg)))))


(defun test-send-msg ()
    (clue:test "Send message"
        (clue:expect-fail (lambda () (deps:send-msg nil)))
        (let ((sent nil))
            (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                       (declare (ignore msg))
                                                       (setf sent T)
                                                       nil))
                (deps:send-msg 5)
                (clue:check-equal :expected t :actual sent)))

        (let ((sent nil))
            (deps:with-deps (deps:create)
                (deps:send-msg 5)
                (clue:check-equal :expected nil :actual sent)))))


(defun run-all ()
    (clue:suite "Dependency Tests"
        (test-msg-handler)
        (test-read-msg)
        (test-send-msg)))
