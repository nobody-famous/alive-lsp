(defpackage :alive/test/deps
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)))

(in-package :alive/test/deps)


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
            (clue:expect-fail (lambda () (deps:read-msg))))))


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

        (deps:with-deps (deps:create)
            (clue:expect-fail (lambda () (deps:send-msg 5))))))


(defun test-send-request ()
    (clue:test "Send request"
        (clue:expect-fail (lambda () (deps:send-request (list (cons :id 5)))))
        (deps:with-deps (deps:create :send-request (lambda (msg)
                                                       (lsp-msg:create-response (cdr (assoc :id msg)) :result-value "foo")))
            (clue:check-equal :expected "foo"
                              :actual (gethash "result" (deps:send-request (list (cons :id 5))))))))


(defun run-all ()
    (clue:suite "Dependency Tests"
        (test-msg-handler)
        (test-read-msg)
        (test-send-msg)
        (test-send-request)))
