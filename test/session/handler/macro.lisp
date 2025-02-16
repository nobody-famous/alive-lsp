(defpackage :alive/test/session/handler/macro
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:macro :alive/session/handler/macro)))

(in-package :alive/test/session/handler/macro)


(defun test-expand ()
    (clue:suite "Expand"
        (clue:test "No text, no package"
            (let* ((response (macro:expand (list (cons :id 5))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "NIL"
                                  :actual txt)))

        (clue:test "Text, no package"
            (let* ((response (macro:expand (list (cons :id 5)
                                                 (cons :params (list (cons :text "foo"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "foo"
                                  :actual txt)))

        (clue:test "Have text and package"
            (deps:with-deps (deps:create :macro-expand (lambda (txt pkg)
                                                           (list txt pkg)))
                (let* ((response (macro:expand (list (cons :id 5)
                                                     (cons :params (list (cons :text "bar")
                                                                         (cons :package "foo"))))))
                       (result (gethash "result" response))
                       (txt (gethash "text" result)))
                    (clue:check-equal :expected "(bar foo)"
                                      :actual txt))))))


(defun test-expand-1 ()
    (clue:suite "Expand 1"
        (clue:test "No text, no package"
            (let* ((response (macro:expand-1 (list (cons :id 5))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "NIL"
                                  :actual txt)))

        (clue:test "Have text and package"
            (deps:with-deps (deps:create :macro-expand-1 (lambda (txt pkg)
                                                             (list txt pkg)))
                (let* ((response (macro:expand-1 (list (cons :id 5)
                                                       (cons :params (list (cons :text "bar")
                                                                           (cons :package "foo"))))))
                       (result (gethash "result" response))
                       (txt (gethash "text" result)))
                    (clue:check-equal :expected "(bar foo)"
                                      :actual txt))))))


(defun get-macro-text ()
    (clue:suite "Macro Text"
        (clue:test "Simple macro"
            (let* ((response (macro:get-text (list (cons :id 5)
                                                   (cons :params (list (cons :text "(bar 1 2)"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "(bar 1 2)"
                                  :actual txt)))

        (clue:test "Defmacro"
            (let* ((response (macro:get-text (list (cons :id 5)
                                                   (cons :params (list (cons :text "(defmacro bar (x y) `(+ x y))"))))))
                   (result (gethash "result" response))
                   (txt (gethash "text" result)))
                (clue:check-equal :expected "(bar x y)"
                                  :actual txt)))))


(defun run-all ()
    (clue:suite "Macro Tests"
        (test-expand)
        (test-expand-1)))
