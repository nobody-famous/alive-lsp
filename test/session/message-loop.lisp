(defpackage :alive/test/session/message-loop
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:context :alive/context)
                      (:logger :alive/logger)
                      (:msg-loop :alive/session/message-loop)
                      (:state :alive/session/state)
                      (:utils :alive/test/utils)))

(in-package :alive/test/session/message-loop)


(defun loop-test ()
    (let ((state (state:create))
          (destroy-called nil)
          (in-stream (utils:stream-from-string (utils:create-msg "foo"))))
        (context:with-context (:input-stream in-stream
                                             :destroy-fn (lambda ()
                                                             (setf destroy-called T)))
            (msg-loop:run state)
            (clue:check-equal :expected T
                              :actual destroy-called))))


(defun test-run ()
    (clue:suite "Run"
        (clue:test "With logger"
            (let ((result nil))
                (with-output-to-string (str)
                    (logger:with-logging (logger:create str logger:*info*)
                        (setf result (loop-test))))
                result))

        (clue:test "Without logger"
            (loop-test))))


(defun run-all ()
    (clue:suite "Message Loop Tests"
        (test-run)))
