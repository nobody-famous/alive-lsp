(defpackage :alive/test/session/threads
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/test/session/threads)


(defun test-run-in-thread ()
    (clue:suite "Run In Thread"
        (clue:test "Simple thread"
            (state:with-state (state:create)
                (let* ((started nil)
                       (thread (threads:run-in-thread "Test Method"
                                                      (list (cons :id 5))
                                                      (lambda () (setf started T)))))
                    (bt:join-thread thread)
                    (clue:check-equal :expected T
                                      :actual started))))

        (clue:test "Simple thread, no message id"
            (state:with-state (state:create)
                (let* ((started nil)
                       (thread (threads:run-in-thread "Test Method"
                                                      (list (cons :foo "bar"))
                                                      (lambda () (setf started T)))))
                    (bt:join-thread thread)
                    (clue:check-equal :expected T
                                      :actual started))))))


(defun run-all ()
    (clue:suite "Session Threads Tests"
        (test-run-in-thread)))
