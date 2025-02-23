(defpackage :alive/test/session/handler/eval
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/session/handler/eval)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/eval)


(defun run-test (msg)
    (let ((msg-sent nil))
        (let* ((state (state:create))
               (deps (deps:new-create :send-msg (lambda (msg)
                                                    (let* ((id (gethash "id" msg))
                                                           (fn (when id (state:new-get-sent-msg-callback state id))))
                                                        (setf msg-sent T)
                                                        (when fn
                                                              (funcall fn (list (cons :error "foo"))))))
                                      :eval-fn (lambda (str) (declare (ignore str))))))
            (eval:new-handle deps state msg)
            msg-sent)))


(defun test-handle ()
    (clue:suite "Handle Eval"
        (clue:test "Error"
            (clue:check-equal :expected T
                              :actual (run-test (list (cons :id 5)
                                                      (cons :params (list (cons :text "(+ 1 2)")))))))

        (clue:test "Add"
            (clue:check-equal :expected T
                              :actual (run-test (list (cons :id 5)
                                                      (cons :params (list (cons :text "(+ 1 2)")))))))

        (clue:test "Add History"
            (clue:check-equal :expected T
                              :actual (run-test (list (cons :id 5)
                                                      (cons :params (list (cons :store-result T)
                                                                          (cons :text "(+ 1 2)")))))))))


(defun run-all ()
    (clue:suite "Eval Handler Tests"
        (test-handle)))
