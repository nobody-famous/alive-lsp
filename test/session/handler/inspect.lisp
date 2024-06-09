(defpackage :alive/test/session/handler/inspect
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:inspect :alive/session/handler/inspect)
                      (:inspector :alive/inspector)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/inspect)


(defun fake-inspector (result)
    (inspector:create :text "foo" :pkg "cl-user" :result result))


(defun test-do-inspect ()
    (clue:suite "Do inspect"
        (clue:test "Fail"
            (let ((sent-msg nil))
                (state:with-state (state:create)
                    (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                               (setf sent-msg msg)
                                                               nil))
                        (inspect:do-inspect (list (cons :id 5)))
                        (clue:check-exists (gethash "error" sent-msg))))))

        (clue:test "New inspect"
            (let ((sent-msg nil))
                (state:with-state (state:create)
                    (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                               (setf sent-msg msg)
                                                               nil))
                        (inspect:do-inspect (list (cons :id 5)
                                                  (cons :params (list (cons :text "foo")))))
                        (clue:check-exists (gethash "result" sent-msg))))))

        (clue:test "New result symbol"
            (state:with-state (state:create)
                (deps:with-deps (deps:create :eval-fn (lambda (str)
                                                          (declare (ignore str))
                                                          'foo))
                    (inspect:do-inspect (list (cons :id 5)
                                              (cons :params (list (cons :text "foo")))))
                    (clue:check-exists (state:get-inspector 1)))))

        (clue:test "Old result"
            (state:with-state (state:create)
                (deps:with-deps (deps:create)
                    (state:add-inspector 5 (fake-inspector 10))
                    (inspect:do-inspect (list (cons :id 5)
                                              (cons :params (list (cons :text "foo")
                                                                  (cons :id 5))))))))))


(defun test-refresh ()
    (clue:test "Refresh"
        (deps:with-deps (deps:create)
            (inspect:refresh (list (cons :id 5))))))


(defun test-close ()
    (clue:test "Close"
        (state:with-state (state:create)
            (state:add-inspector 5 (inspector:create :text "foo" :pkg "bar" :result nil))
            (inspect:do-close (list (cons :id 1)
                                    (cons :params (list (cons :id 5)))))
            (clue:check-equal :expected nil
                              :actual (state:get-inspector 5)))))


(defun test-symbol ()
    (clue:suite "Symbol"
        (clue:test "Fail"
            (deps:with-deps nil
                (clue:expect-fail (lambda () (inspect:do-symbol (list (cons :id 5)))))))

        (clue:test "Symbol"
            (deps:with-deps (deps:create)
                (inspect:do-symbol (list (cons :id 5)))))))


(defun test-macro ()
    (clue:suite "Macro"
        (clue:test "Fail"
            (deps:with-deps nil
                (clue:expect-fail (lambda () (inspect:macro (list (cons :id 5)))))))

        (clue:test "Macro"
            (state:with-state (state:create)
                (deps:with-deps (deps:create)
                    (inspect:macro (list (cons :id 5))))))))


(defun test-inspect-eval ()
    (clue:suite "Inspect Eval"
        (clue:test "No result"
            (let ((sent-msg nil))
                (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                           (setf sent-msg msg)
                                                           nil))
                    (inspect:do-inspect-eval (list (cons :id 5)))
                    (clue:check-exists sent-msg))))

        (clue:test "Has result"
            (let ((sent-msg nil))
                (state:with-state (state:create)
                    (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                               (setf sent-msg msg)
                                                               nil)
                                                 :eval-fn (lambda (str)
                                                              (declare (ignore str))
                                                              5))
                        (inspect:do-inspect-eval (list (cons :id 5)
                                                       (cons :params (list (cons :text "(+ 1 2)")))))
                        (clue:check-exists sent-msg)))))))


(defun run-all ()
    (clue:suite "Inspect Tests"
        (test-do-inspect)
        (test-refresh)
        (test-close)
        (test-symbol)
        (test-macro)
        (test-inspect-eval)))
