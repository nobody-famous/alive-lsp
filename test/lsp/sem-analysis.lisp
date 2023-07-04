(defpackage :alive/test/lsp/sem-analysis
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)))

(in-package :alive/test/lsp/sem-analysis)


(defun test-is-number ()
    (clue:test "Invalid decimal"
        (clue:check-equal :expected NIL
                          :actual (analysis::is-number "1.2.3.4")))

    (clue:test "Invalid div"
        (clue:check-equal :expected NIL
                          :actual (analysis::is-number "1/2/3/4"))))


(defun run-all ()
    (clue:suite "Semantic Analysis Tests"
        (test-is-number)))
