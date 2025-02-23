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
            (let* ((sent-msg nil)
                   (state (state:create))
                   (deps (deps:new-create :send-msg (lambda (msg)
                                                        (setf sent-msg msg)
                                                        nil))))
                (inspect:new-do-inspect deps state (list (cons :id 5)))
                (clue:check-exists (gethash "error" sent-msg))))

        (clue:test "New inspect"
            (let* ((sent-msg nil)
                   (state (state:create))
                   (deps (deps:new-create :send-msg (lambda (msg)
                                                        (setf sent-msg msg)
                                                        nil))))
                (inspect:new-do-inspect deps state (list (cons :id 5)
                                                         (cons :params (list (cons :text "foo")))))
                (clue:check-exists (gethash "result" sent-msg))))

        (clue:test "New result symbol"
            (let ((state (state:create))
                  (deps (deps:new-create :eval-fn (lambda (str)
                                                      (declare (ignore str))
                                                      'foo))))
                (inspect:new-do-inspect deps state (list (cons :id 5)
                                                         (cons :params (list (cons :text "foo")))))
                (clue:check-exists (state:new-get-inspector state 1))))

        (clue:test "Old result"
            (let ((state (state:create))
                  (deps (deps:new-create)))
                (state:new-add-inspector state 5 (fake-inspector 10))
                (inspect:new-do-inspect deps state (list (cons :id 5)
                                                         (cons :params (list (cons :text "foo")
                                                                             (cons :id 5)))))))))


(defun test-refresh ()
    (clue:test "Refresh"
        (let ((state (state:create))
              (deps (deps:new-create)))
            (inspect:new-refresh deps state (list (cons :id 5))))))


(defun test-close ()
    (clue:test "Close"
        (let ((state (state:create)))
            (state:new-add-inspector state 5 (inspector:create :text "foo" :pkg "bar" :result nil))
            (inspect:new-do-close state (list (cons :id 1)
                                              (cons :params (list (cons :id 5)))))
            (clue:check-equal :expected nil
                              :actual (state:new-get-inspector state 5)))))


(defun test-symbol ()
    (clue:suite "Symbol"
        (clue:test "Symbol"
            (let ((state (state:create))
                  (deps (deps:new-create)))
                (inspect:new-do-symbol deps state (list (cons :id 5)))))))


(defun test-macro ()
    (clue:suite "Macro"
        (clue:test "Macro"
            (let ((state (state:create))
                  (deps (deps:new-create)))
                (inspect:new-macro deps state (list (cons :id 5)))))))


(defun test-inspect-eval ()
    (clue:suite "Inspect Eval"
        (clue:test "No result"
            (let* ((sent-msg nil)
                   (state (state:create))
                   (deps (deps:new-create :send-msg (lambda (msg)
                                                        (setf sent-msg msg)
                                                        nil))))
                (inspect:new-do-inspect-eval deps state (list (cons :id 5)))
                (clue:check-exists sent-msg)))

        (clue:test "Has result"
            (let* ((sent-msg nil)
                   (state (state:create))
                   (deps (deps:new-create :send-msg (lambda (msg)
                                                        (setf sent-msg msg)
                                                        nil)
                                          :eval-fn (lambda (str)
                                                       (declare (ignore str))
                                                       5))))
                (inspect:new-do-inspect-eval deps state (list (cons :id 5)
                                                              (cons :params (list (cons :text "(+ 1 2)")))))
                (clue:check-exists sent-msg)))))


(defun run-all ()
    (clue:suite "Inspect Tests"
        (test-do-inspect)
        (test-refresh)
        (test-close)
        (test-symbol)
        (test-macro)
        (test-inspect-eval)))
