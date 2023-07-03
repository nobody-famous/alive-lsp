(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:message :alive/lsp/message/abstract)))

(in-package :alive/test/lsp/message)


(defun test-create-resp ()
    (clue:test "Create response"
        (handler-case
                (progn
                 (message:create-response 5 :result-value 10 :error-value 20)
                 (clue:fail "Expected error"))
            (error () nil))))


(defun run-all ()
    (clue:suite "Message tests"))
