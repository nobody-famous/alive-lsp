(defpackage :alive/test/session
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:run :alive/test/harness/run)))

(in-package :alive/test/session)


(defun init-msg ()
    (run:test "Initialize Message"
              (lambda ()
                  nil)))


(defun run-all ()
    (run:suite "Session Tests"
               (lambda ()
                   (init-msg))))
