(defpackage :alive/test/parse/tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:pos :alive/position)
                      (:utils :alive/test/utils)
                      (:token :alive/parse/token)
                      (:tokens :alive/parse/tokenizer)))

(in-package :alive/test/parse/tokens)


(defun tokens-for-string (str)
    (with-input-from-string (s str)
        (tokens:from-stream s)))


(defun test-foo ()
    (clue:test "Test foo symbol"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 3)
                                              :end-offset 3
                                              :text "foo"))
                          :actual (tokens-for-string "foo"))))


(defun test-defun ()
    (clue:test "Test defun symbol"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 5)
                                              :text "defun"))
                          :actual (tokens-for-string "defun"))))


(defun test-comment ()
    (clue:test "Test comment"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*line-comment*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 9)
                                              :text "; Comment"))
                          :actual (tokens-for-string "; Comment"))))


(defun test-string ()
    (clue:test "Test string"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*string*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 8)
                                              :text "\"String\""))
                          :actual (tokens-for-string "\"String\""))))


(defun test-basic-macro ()
    (clue:test "Test basic macro"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*macro*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 6)
                                              :text "#'abcd"))
                          :actual (tokens-for-string "#'abcd"))))


(defun test-ifdef-macro ()
    (clue:test "Test ifdef macro"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*ifdef-false*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 3)
                                              :text "#+n"))
                          :actual (tokens-for-string "#+n"))))


(defun test-parens ()
    (clue:test "Test parens"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*open-paren*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 1)
                                              :text "(")
                                          (token:create
                                              :type-value alive/types:*close-paren*
                                              :start (pos:create 0 1)
                                              :end (pos:create 0 2)
                                              :text ")"))
                          :actual (tokens-for-string "()"))))


(defun test-sym-with-pkg ()
    (clue:test "Test symbol with package"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :end (pos:create 0 3)
                                              :text "foo")
                                          (token:create
                                              :type-value alive/types:*colons*
                                              :start (pos:create 0 3)
                                              :end (pos:create 0 5)
                                              :text "::")
                                          (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 5)
                                              :end (pos:create 0 8)
                                              :text "bar"))
                          :actual (tokens-for-string "foo::bar"))))


(defun run-all ()
    (clue:suite "Test parse tokens"
        (test-foo)
        (test-defun)
        (test-comment)
        (test-string)
        (test-basic-macro)
        (test-ifdef-macro)
        (test-parens)
        (test-sym-with-pkg)))
