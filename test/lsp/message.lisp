(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:event :alive/lsp/message/notification)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:request :alive/lsp/message/request)
                      (:response :alive/lsp/message/response)
                      (:sem-tokens :alive/lsp/types/sem-tokens)))

(in-package :alive/test/lsp/message)


(defun test-create-resp ()
    (clue:suite "Response tests"
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
                                  :actual (gethash "messages" result))))))


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
    (clue:suite "Selection range"
        (clue:test "Selection range empty"
            (let ((actual (response:selection-range 5 nil)))
                (clue:check-exists (gethash "result" actual))))

        (clue:test "Selection range"
            (let ((actual (response:selection-range 5 (list (list (cons :range (range:create (pos:create 0 10) (pos:create 0 13)))
                                                                  (cons :parent (list (cons :range (range:create (pos:create 0 9) (pos:create 0 19)))
                                                                                      (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 0 20)))
                                                                                                          (cons :parent nil))))))))))
                (clue:check-exists (gethash "result" actual))))))


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


(defun stream-test (fn label method)
    (clue:test label
        (let* ((actual (funcall fn "foo"))
               (params (gethash "params" actual)))
            (clue:check-equal :expected method
                              :actual (gethash "method" actual))
            (clue:check-equal :expected "foo"
                              :actual (gethash "data" params)))))


(defun test-notifications ()
    (clue:suite "Notification Tests"
        (clue:test "Refresh"
            (let ((actual (event:refresh)))
                (clue:check-equal :expected "$/alive/refresh"
                                  :actual (gethash "method" actual))))

        (stream-test 'event:stdout "Stdout" "$/alive/stdout")
        (stream-test 'event:stderr "Stderr" "$/alive/stderr")))


(defun run-all ()
    (clue:suite "Message tests"
        (test-create-resp)
        (test-sem-tokens-resp)
        (test-create-req)
        (test-selection-range)
        (test-doc-symbols)
        (test-notifications)))
