(defpackage :alive/test/session/refresh
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)
                      (:deps :alive/deps)
                      (:refresh :alive/session/refresh)))

(in-package :alive/test/session/refresh)


(defun run-test (response)
    (let* ((sent-msg nil)
           (thread nil)
           (state (state:create))
           (deps (deps:create :send-request (lambda (req)
                                                    (declare (ignore req))
                                                    response)
                                  :send-msg (lambda (msg)
                                                (setf sent-msg msg)
                                                nil))))
        (handler-bind ((alive/session/spawn:spawned-thread (lambda (evt)
                                                               (setf thread (alive/session/spawn:thread evt)))))
            (refresh:new-send deps state)
            (bt:join-thread thread)
            (clue:check-exists sent-msg))))


(defun test-send ()
    (clue:suite "Send"
        (clue:test "OK"
            (run-test (list (cons :id 5))))

        (clue:test "Error"
            (run-test (list (cons :id 5)
                            (cons :error "foo"))))))


(defun run-all ()
    (clue:suite "Refresh Tests"
        (test-send)))
