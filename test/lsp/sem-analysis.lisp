(defpackage :alive/test/lsp/sem-analysis
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:pos :alive/position)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:token :alive/parse/token)))

(in-package :alive/test/lsp/sem-analysis)


(defun test-is-number ()
    (clue:suite "is-number"
        (clue:test "Invalid decimal"
            (clue:check-equal :expected NIL
                              :actual (analysis::is-number "1.2.3.4")))

        (clue:test "Invalid div"
            (clue:check-equal :expected NIL
                              :actual (analysis::is-number "1/2/3/4")))))


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


(defun test-get-type ()
    (clue:test "Get type"
        (let ((state (make-instance 'analysis::analysis-state)))
            (clue:check-equal :expected nil
                              :actual (analysis::get-symbol-type state "foo")))))


(defun test-update-type ()
    (clue:suite "Update type"
        (clue:test "Update type 2 symbols"
            (let ((state (make-instance 'analysis::analysis-state)))
                (setf (analysis::lex-tokens state) (list (token:create :type-value alive/types:*symbol*
                                                                       :start (pos:create 0 0)
                                                                       :start-offset 0
                                                                       :end (pos:create 0 4)
                                                                       :end-offset 4
                                                                       :text "text")
                                                         (token:create :type-value alive/types:*symbol*
                                                                       :start (pos:create 0 5)
                                                                       :start-offset 5
                                                                       :end (pos:create 0 6)
                                                                       :end-offset 6
                                                                       :text ":")))
                (clue:check-equal :expected nil
                                  :actual (analysis::update-symbol-types state))))

        (clue:test "Update type symbol and colons"
            (let ((state (make-instance 'analysis::analysis-state)))
                (setf (analysis::lex-tokens state) (list (token:create :type-value alive/types:*symbol*
                                                                       :start (pos:create 0 0)
                                                                       :start-offset 0
                                                                       :end (pos:create 0 4)
                                                                       :end-offset 4
                                                                       :text "text")
                                                         (token:create :type-value alive/types:*colons*
                                                                       :start (pos:create 0 5)
                                                                       :start-offset 5
                                                                       :end (pos:create 0 6)
                                                                       :end-offset 6
                                                                       :text ":")))
                (clue:check-equal :expected nil
                                  :actual (analysis::update-symbol-types state))))

        (clue:test "Update type no symbols"
            (let ((state (make-instance 'analysis::analysis-state)))
                (setf (analysis::lex-tokens state) (list (token:create :type-value alive/types:*colons*
                                                                       :start (pos:create 0 0)
                                                                       :start-offset 0
                                                                       :end (pos:create 0 4)
                                                                       :end-offset 4
                                                                       :text ":")
                                                         (token:create :type-value alive/types:*colons*
                                                                       :start (pos:create 0 5)
                                                                       :start-offset 5
                                                                       :end (pos:create 0 6)
                                                                       :end-offset 6
                                                                       :text ":")))
                (clue:check-equal :expected nil
                                  :actual (analysis::update-symbol-types state))))))


(defun test-symbol-lookup ()
    (clue:test "Symbol lookup"
        (clue:check-equal :expected 10
                          :actual (analysis::lookup-symbol-type "*debug-io*"))))


(defun test-open-parens-fn-call ()
    (clue:test "Open parens fn call"
        (let ((state (make-instance 'analysis::analysis-state))
              (form (make-instance 'analysis::open-form)))
            (setf (analysis::lambda-list form) (list (list 'a)))

            (analysis::process-open-parens-fn-call state form)
            (clue:check-equal :expected :arg-init
                              :actual (analysis::expr-type (car (analysis::opens state)))))))


(defun run-all ()
    (clue:suite "Semantic Analysis Tests"
        (test-is-number)
        (test-add-token)
        (test-get-type)
        (test-update-type)
        (test-symbol-lookup)
        (test-open-parens-fn-call)))
