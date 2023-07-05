(defpackage :alive/test/lsp/sem-analysis
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:pos :alive/position)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:token :alive/parse/token)))

(in-package :alive/test/lsp/sem-analysis)


(defun test-is-number ()
    (clue:test "Invalid decimal"
        (clue:check-equal :expected NIL
                          :actual (analysis::is-number "1.2.3.4")))

    (clue:test "Invalid div"
        (clue:check-equal :expected NIL
                          :actual (analysis::is-number "1/2/3/4"))))


(defun test-add-token ()
    (clue:test "Add token"
        (let ((state (make-instance 'analysis::analysis-state))
              (token (token:create :type-value nil
                                   :start (pos:create 0 0)
                                   :start-offset 0
                                   :end (pos:create 2 3)
                                   :end-offset 3
                                   :text "foo")))
            (analysis::add-sem-token state token sem-types:*comment*)
            (clue:check-equal :expected 3
                              :actual (length (analysis::sem-tokens state))))))


(defun run-all ()
    (clue:suite "Semantic Analysis Tests"
        (test-is-number)
        (test-add-token)))
