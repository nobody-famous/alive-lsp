(defpackage :alive/test/session/handler/macro
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:macro :alive/session/handler/macro)))

(in-package :alive/test/session/handler/macro)


(defun test-expand ()
    (clue:suite "Expand"
        (clue:test "No text, no package"
            (let* ((deps (deps:create))
                   (response (macro:new-expand deps (list (cons :id 5))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "NIL"
                                  :actual txt)))

        (clue:test "Text, no package"
            (let* ((deps (deps:create))
                   (response (macro:new-expand deps (list (cons :id 5)
                                                          (cons :params (list (cons :text "foo"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "foo"
                                  :actual txt)))

        (clue:test "Have text and package"
            (let* ((deps (deps:create :macro-expand (lambda (txt pkg)
                                                            (list txt pkg))))
                   (response (macro:new-expand deps (list (cons :id 5)
                                                          (cons :params (list (cons :text "bar")
                                                                              (cons :package "foo"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "(bar foo)"
                                  :actual txt)))))


(defun test-expand-1 ()
    (clue:suite "Expand 1"
        (clue:test "No text, no package"
            (let* ((deps (deps:create))
                   (response (macro:new-expand-1 deps (list (cons :id 5))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "NIL"
                                  :actual txt)))

        (clue:test "Have text and package"
            (let* ((deps (deps:create :macro-expand-1 (lambda (txt pkg)
                                                              (list txt pkg))))
                   (response (macro:new-expand-1 deps (list (cons :id 5)
                                                            (cons :params (list (cons :text "bar")
                                                                                (cons :package "foo"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "(bar foo)"
                                  :actual txt)))))


(defun run-all ()
    (clue:suite "Macro Tests"
        (test-expand)
        (test-expand-1)))
