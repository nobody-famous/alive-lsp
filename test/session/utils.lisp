(defpackage :alive/test/session/utils
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/session/deps)
                      (:state :alive/session/state)
                      (:utils :alive/session/utils)))

(in-package :alive/test/session/utils)


(defun div (x y)
    (/ x y))


(defun test-run-in-thread ()
    (clue:suite "Run In Thread"
        (clue:test "Simple thread"
            (state:with-state (state:create)
                (let* ((started nil)
                       (thread (utils:run-in-thread "Test Method"
                                                    (list (cons :id 5))
                                                    (lambda () (setf started T)))))
                    (bt:join-thread thread)
                    (clue:check-equal :expected T
                                      :actual started))))

        (clue:test "Simple thread, no message id"
            (state:with-state (state:create)
                (let* ((started nil)
                       (thread (utils:run-in-thread "Test Method"
                                                    (list (cons :foo "bar"))
                                                    (lambda () (setf started T)))))
                    (bt:join-thread thread)
                    (clue:check-equal :expected T
                                      :actual started))))))


(defun run-all ()
    (clue:suite "Session Utils Tests"
        (test-run-in-thread)))
