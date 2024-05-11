(defpackage :alive/test/session/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:handlers :alive/session/handlers)
                      (:msg :alive/session/message)
                      (:state :alive/session/state)))

(in-package :alive/test/session/message)


(defun test-handle ()
    (clue:suite "Handle Tests"
        (clue:test "Request has handler"
            (handlers:with-handlers (list (cons "foo" (lambda (msg) (declare (ignore msg)))))
                (clue:check-equal :expected nil
                                  :actual (hash-table-p (msg:handle (list (cons :id 1)
                                                                          (cons :method "foo")))))))

        (clue:test "Request no handler"
            (clue:check-equal :expected T
                              :actual (hash-table-p (msg:handle (list (cons :id 5)
                                                                      (cons :method "foo"))))))

        (clue:test "Invalid Message"
            (clue:check-equal :expected T
                              :actual (hash-table-p (msg:handle (list (cons :id 5))))))

        (clue:test "Response has handler, no callback"
            (handlers:with-handlers (list (cons "foo" (lambda (msg) (declare (ignore msg)))))
                (state:with-state (state:create)
                    (clue:check-equal :expected T
                                      :actual (hash-table-p (msg:handle (list (cons :id 1)
                                                                              (cons :result "foo"))))))))

        (clue:test "Response has handler, has callback"
            (handlers:with-handlers (list (cons "foo" (lambda (msg) (declare (ignore msg)))))
                (state:with-state (state:create)
                    (state:set-sent-msg-callback 1 (lambda (msg)
                                                       (declare (ignore msg))
                                                       (make-hash-table)))
                    (clue:check-equal :expected T
                                      :actual (hash-table-p (msg:handle (list (cons :id 1)
                                                                              (cons :error "foo"))))))))))


(defun run-all ()
    (clue:suite "Message Tests"
        (test-handle)))
