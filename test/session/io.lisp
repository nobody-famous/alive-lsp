(defpackage :alive/test/session/io
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:context :alive/context)
                      (:io :alive/session/io)))

(in-package :alive/test/session/io)


(defclass test-stream (stream)
        ())


(defun test-read-msg ()
    (clue:test "Read Message"
        (clue:expect-fail (lambda () (io:read-msg)))
        (clue:expect-fail (lambda () (context:with-context ()
                                         (io:read-msg))))
        (context:with-context (:input-stream (make-instance 'test-stream))
            (io:read-msg))))


(defun run-all ()
    (clue:suite "IO Tests"
        (test-read-msg)))
