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
            (clue:expect-fail (lambda () (inspect:do-inspect (list (cons :id 5)
                                                                   (cons :params (list (cons :text "foo"))))))))

        (clue:test "New inspect"
            (let ((sent-msg nil))
                (deps:with-deps (deps:create :send-msg (lambda (msg)
                                                           (setf sent-msg msg)
                                                           nil))
                    (inspect:do-inspect (list (cons :id 5)
                                              (cons :params (list (cons :text "foo")))))
                    (clue:check-exists (gethash "result" sent-msg)))))

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


(defun run-all ()
    (clue:suite "Inspect Tests"
        (test-do-inspect)
        (test-refresh)))
