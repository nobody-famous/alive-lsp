(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:request :alive/lsp/message/request)
                      (:response :alive/lsp/message/response)
                      (:sem-tokens :alive/lsp/types/sem-tokens)))

(in-package :alive/test/lsp/message)


(defun test-create-resp ()
    (clue:test "Invalid response"
        (clue:expect-fail (lambda ()
                              (message:create-response 5 :result-value 10 :error-value 20))))

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
                              :actual (gethash "messages" result))))

    (clue:test "Do eval"
        (let* ((actual (response:do-eval 5 "10"))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected "10"
                              :actual (gethash "text" result))))

    (clue:test "Get symbol"
        (let* ((actual (response:get-symbol 5 :value "10"))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected "10"
                              :actual (gethash "value" result))))

    (clue:test "Try compile"
        (let* ((actual (response:try-compile 5 "10"))
               (result (gethash "result" actual)))
            (clue:check-equal :expected 5
                              :actual (gethash "id" actual))
            (clue:check-equal :expected "10"
                              :actual (gethash "messages" result)))))


(defun test-sem-tokens-resp ()
    (clue:test "Create sem tokens response"
        (let* ((tokens (list (sem-tokens:create :token-type sem-tokens:*symbol*
                                                :line 0
                                                :start 0
                                                :end 5)
                             (sem-tokens:create :token-type sem-tokens:*number*
                                                :line 1
                                                :start 6
                                                :end 10)))
               (actual (response:sem-tokens 5 tokens))
               (result (gethash "result" actual)))
            (clue:check-equal :expected (gethash "data" result)
                              :actual (list 0 0 5 10 0 1 6 4 3 0)))))


(defun test-selection-range ()
    (clue:test "Selection range empty"
        (let ((actual (response:selection-range 5 nil)))
            (clue:check-exists (gethash "result" actual))))

    (clue:test "Selection range"
        (let ((actual (response:selection-range 5 (list (list (list (list :start (cons :line 0) (cons :character 0))
                                                                    (list :end (cons :line 198) (cons :character 56)))
                                                              (list (list :start (cons :line 188) (cons :character 0))
                                                                    (list :end (cons :line 192) (cons :character 58)))
                                                              (list (list :start (cons :line 189) (cons :character 4))
                                                                    (list :end (cons :line 189) (cons :character 41)))
                                                              (list (list :start (cons :line 189) (cons :character 4))
                                                                    (list :end (cons :line 189) (cons :character 41))))))))
            (clue:check-exists (gethash "result" actual)))))


(defun test-doc-symbols ()
    (clue:test "Doc symbols empty"
        (let ((actual (response:doc-symbols 5 nil)))
            (clue:check-exists (gethash "result" actual)))))


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
        (test-sem-tokens-resp)
        (test-create-req)
        (test-selection-range)
        (test-doc-symbols)))
