(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:request :alive/lsp/message/request)
                      (:response :alive/lsp/message/response)))

(in-package :alive/test/lsp/message)


(defun test-create-resp ()
    (clue:test "Invalid response"
        (handler-case
                (progn
                 (message:create-response 5 :result-value 10 :error-value 20)
                 (clue:fail "Expected error"))
            (error () nil)))

    (clue:test "Macro"
        (let* ((actual (response:macro 5 "foo"))
               (result (gethash "result" actual)))
            (clue:check-equal :expected "foo"
                              :actual (gethash "text" result))))

    (clue:test "Inspect"
        (let* ((actual (response:do-inspect 5 :insp-id 10 :result 20 :result-type "foo"))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected 10
                              :actual (gethash "id" result))
            (clue:check-equal :expected 20
                              :actual (gethash "result" result))
            (clue:check-equal :expected "foo"
                              :actual (gethash "resultType" result))))

    (clue:test "Inspect default type"
        (let* ((actual (response:do-inspect 5 :insp-id 10 :result 20))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected 10
                              :actual (gethash "id" result))
            (clue:check-equal :expected 20
                              :actual (gethash "result" result))
            (clue:check-equal :expected "expr"
                              :actual (gethash "resultType" result))))

    (clue:test "Definition"
        (let* ((actual (response:definition 5 :uri 10 :range 20))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected 10
                              :actual (gethash "uri" result))
            (clue:check-equal :expected 20
                              :actual (gethash "range" result))))

    (clue:test "Load file"
        (let* ((actual (response:load-file 5 10))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected 10
                              :actual (gethash "messages" result)))))


(defun test-create-req ()
    (clue:test "Create request"
        (let* ((actual (request:debugger 5 :message 10 :restarts 15 :stack-trace 20))
               (params (gethash "params" actual)))
            (clue:check-equal :expected "$/alive/debugger"
                              :actual (gethash "method" actual))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected 10
                              :actual (gethash "message" params))
            (clue:check-equal :expected 15
                              :actual (gethash "restarts" params))
            (clue:check-equal :expected 20
                              :actual (gethash "stackTrace" params)))))

(defun run-all ()
    (clue:suite "Message tests"
        (test-create-resp)
        (test-create-req)))
