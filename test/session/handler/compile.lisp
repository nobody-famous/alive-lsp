(defpackage :alive/test/session/handler/compile
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:compile :alive/session/handler/compile)
                      (:deps :alive/deps)
                      (:spawn :alive/session/spawn)))

(in-package :alive/test/session/handler/compile)


(defun test-try ()
    (clue:test "Try"
        (let ((thread nil)
              (sent-msg nil))
            (handler-bind ((spawn:spawned-thread (lambda (evt)
                                                     (setf thread (spawn:thread evt)))))
                (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                           (setf sent-msg msg)
                                                           nil)
                                             :send-request (lambda (req)
                                                               (declare (ignore req))
                                                               (list (cons :id 5)))
                                             :try-compile (lambda (path) (declare (ignore path))))
                    (compile:try (list (cons :id 5)))
                    (bt:join-thread thread)
                    (clue:check-exists (gethash "result" sent-msg)))))))


(defun run-all ()
    (clue:suite "Compile Tests"
        (test-try)))
