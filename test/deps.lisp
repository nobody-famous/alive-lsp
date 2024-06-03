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
                              :actual (deps:read-msg)))))


(defun test-send-msg ()
    (clue:test "Send message"
        (let ((sent nil))
            (clue:expect-fail (lambda () (deps:send-msg (list (cons :id 5)))))
            (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                       (declare (ignore msg))
                                                       (setf sent T)
                                                       nil))
                (deps:send-msg 5)
                (clue:check-equal :expected t :actual sent)))))


(defun test-send-request ()
    (clue:test "Send request"
        (clue:expect-fail (lambda () (deps:send-request (make-hash-table))))
        (deps:with-deps (deps:create :send-request (lambda (msg)
                                                       (declare (ignore msg))
                                                       (list (cons :result "foo"))))
            (clue:check-equal :expected "foo"
                              :actual (cdr (assoc :result (deps:send-request (make-hash-table))))))))


(defun test-list-all-threads ()
    (clue:test "Lilst all threads"
        (clue:expect-fail (lambda () (deps:list-all-threads)))
        (deps:with-deps (deps:create :list-all-threads (lambda () (list 1 2)))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:list-all-threads)))))


(defun test-kill-thread ()
    (clue:test "Kill thread"
        (clue:expect-fail (lambda () (deps:kill-thread nil)))
        (deps:with-deps (deps:create)
            (deps:kill-thread nil))))


(defun test-get-thread-id ()
    (clue:test "Get thread id"
        (clue:expect-fail (lambda () (deps:get-thread-id (bt:current-thread))))
        (deps:with-deps (deps:create :get-thread-id (lambda (thread)
                                                        (declare (ignore thread))
                                                        5))
            (clue:check-equal :expected 5
                              :actual (deps:get-thread-id (bt:current-thread))))))


(defun test-do-eval ()
    (clue:test "Eval"
        (clue:expect-fail (lambda () (deps:do-eval (make-hash-table))))
        (deps:with-deps (deps:create :eval-fn (lambda (data) data))
            (clue:check-equal :expected "foo"
                              :actual (deps:do-eval "foo")))))


(defun run-all ()
    (clue:suite "Dependency Tests"
        (test-msg-handler)
        (test-read-msg)
        (test-send-msg)
        (test-send-request)
        (test-list-all-threads)
        (test-kill-thread)
        (test-get-thread-id)
        (test-do-eval)))
