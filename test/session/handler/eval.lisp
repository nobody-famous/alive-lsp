(defpackage :alive/test/session/handler/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/session/handler/eval)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/eval)


(defun test-handle ()
    (clue:test "Handle"
        (clue:expect-fail (lambda () (eval:handle (list (cons :id 5)))))
        (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                   (let* ((id (gethash "id" msg))
                                                          (fn (state:get-sent-msg-callback id)))
                                                       (funcall fn (list (cons :error "foo"))))))
            (state:with-state (state:create)
                (let ((eval-thread nil))
                    (handler-bind ((eval:eval-thread-event (lambda (evt)
                                                               (setf eval-thread (eval:thread evt)))))
                        (eval:handle (list (cons :id 5)))
                        (bt:join-thread eval-thread)))))))


(defun run-all ()
    (clue:suite "Eval Handler Tests"
        (test-handle)))
