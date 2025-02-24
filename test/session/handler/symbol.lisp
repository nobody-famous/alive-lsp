(defpackage :alive/test/session/handler/symbol
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)
                      (:symbol :alive/session/handler/symbol)))

(in-package :alive/test/session/handler/symbol)


(defun test-for-pos ()
    (clue:test "For position"
        (let ((state (state:create)))
            (clue:expect-fail (lambda () (symbol:new-for-pos state (list (cons :id 5)))))
            (clue:check-exists (gethash "result" (symbol:new-for-pos state (list (cons :id 5)
                                                                                 (cons :params (list (cons :position (list (cons :line 1)
                                                                                                                           (cons :character 2)))
                                                                                                     (cons :text-document (list (cons :uri "some/path"))))))))))))


(defun test-unexport ()
    (clue:test "Unexport"
        (let ((state (state:create))
              (deps (deps:create)))
            (clue:check-exists (gethash "result" (symbol:new-do-unexport deps state (list (cons :id 5))))))))


(defun run-all ()
    (clue:suite "Symbol Tests"
        (test-for-pos)
        (test-unexport)))
