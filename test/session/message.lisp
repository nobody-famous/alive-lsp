(defpackage :alive/test/session/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:handlers :alive/session/handlers)
                      (:msg :alive/session/message)
                      (:state :alive/session/state)))

(in-package :alive/test/session/message)


(defun test-handle ()
    (clue:suite "Handle Tests"
        (clue:test "Request has handler"
            (let ((state (state:create))
                  (deps (deps:create))
                  (handlers (list (cons "foo" (lambda (deps msg) (declare (ignore deps msg)))))))
                (clue:check-equal :expected nil
                                  :actual (hash-table-p (msg:new-handle deps state handlers (list (cons :id 1)
                                                                                                  (cons :method "foo")))))))

        (clue:test "Request no handler"
            (let ((state (state:create))
                  (deps (deps:create))
                  (handlers (list (cons "foo" (lambda (deps msg) (declare (ignore deps msg)))))))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (msg:new-handle deps state handlers (list (cons :id 1)
                                                                                                  (cons :method "bar")))))))

        (clue:test "Invalid Message"
            (let ((state (state:create))
                  (deps (deps:create))
                  (handlers (list (cons "foo" (lambda (deps msg) (declare (ignore deps msg)))))))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (msg:new-handle deps state handlers (list (cons :id 5)))))))

        (clue:test "Response has handler, no callback"
            (let ((state (state:create))
                  (deps (deps:create))
                  (handlers (list (cons "foo" (lambda (deps msg) (declare (ignore deps msg)))))))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (msg:new-handle deps state handlers (list (cons :id 1)
                                                                                                  (cons :result "foo")))))))

        (clue:test "Response has handler, has callback"
            (let ((state (state:create))
                  (deps (deps:create))
                  (handlers (list (cons "foo" (lambda (deps msg) (declare (ignore deps msg)))))))
                (state:set-sent-msg-callback state 1 (lambda (msg)
                                                             (declare (ignore msg))
                                                             (make-hash-table)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (msg:new-handle deps state handlers (list (cons :id 1)
                                                                                                  (cons :error "foo")))))))))


(defun run-all ()
    (clue:suite "Message Tests"
        (test-handle)))
