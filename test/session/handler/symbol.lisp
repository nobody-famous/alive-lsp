(defpackage :alive/test/session/handler/symbol
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)
                      (:symbol :alive/session/handler/symbol)))

(in-package :alive/test/session/handler/symbol)


(defun test-for-pos ()
    (clue:test "For position"
        (clue:expect-fail (lambda () (symbol:for-pos (list (cons :id 5)))))
        (state:with-state (state:create)
            (clue:check-exists (gethash "result" (symbol:for-pos (list (cons :id 5)
                                                                       (cons :params (list (cons :position (list (cons :line 1)
                                                                                                                 (cons :character 2)))
                                                                                           (cons :text-document (list (cons :uri "some/path"))))))))))))


(defun test-unexport ()
    (clue:test "Unexport"
        (clue:check-exists (gethash "result" (symbol:do-unexport (list (cons :id 5)))))))


(defun run-all ()
    (clue:suite "Symbol Tests"
        (test-for-pos)
        (test-unexport)))
