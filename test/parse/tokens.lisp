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
    (clue:test "Foo symbol"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 3)
                                              :end-offset 3
                                              :text "foo"))
                          :actual (tokens-for-string "foo"))))


(defun test-defun ()
    (clue:test "Defun symbol"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 5)
                                              :end-offset 5
                                              :text "defun"))
                          :actual (tokens-for-string "defun"))))


(defun test-comment ()
    (clue:test "Comment"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*line-comment*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 9)
                                              :end-offset 9
                                              :text "; Comment"))
                          :actual (tokens-for-string "; Comment"))))


(defun test-string ()
    (clue:suite "String"
        (clue:test "String"
            (clue:check-equal :expected (list (token:create
                                                  :type-value alive/types:*string*
                                                  :start (pos:create 0 0)
                                                  :start-offset 0
                                                  :end (pos:create 0 8)
                                                  :end-offset 8
                                                  :text "\"String\""))
                              :actual (tokens-for-string "\"String\"")))

        (clue:test "Open String"
            (clue:check-equal :expected (list (token:create
                                                  :type-value alive/types:*string*
                                                  :start (pos:create 0 0)
                                                  :start-offset 0
                                                  :end (pos:create 0 7)
                                                  :end-offset 7
                                                  :text "\"String"))
                              :actual (tokens-for-string "\"String")))))


(defun test-basic-macro ()
    (clue:test "Basic macro"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*macro*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 2)
                                              :end-offset 2
                                              :text "#'")
                                          (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 2)
                                              :start-offset 2
                                              :end (pos:create 0 6)
                                              :end-offset 6
                                              :text "abcd"))
                          :actual (tokens-for-string "#'abcd"))))


(defun test-ifdef-macro ()
    (clue:suite "ifdef"
        (clue:test "Ifdef macro"
            (clue:check-equal :expected (list (token:create
                                                  :type-value alive/types:*ifdef-false*
                                                  :start (pos:create 0 0)
                                                  :start-offset 0
                                                  :end (pos:create 0 3)
                                                  :end-offset 3
                                                  :text "#+n"))
                              :actual (tokens-for-string "#+n")))

        (clue:test "Invalid ifdef macro"
            (clue:check-equal :expected (list (token:create
                                                  :type-value alive/types:*ifdef-false*
                                                  :start (pos:create 0 0)
                                                  :start-offset 0
                                                  :end (pos:create 0 3)
                                                  :end-offset 3
                                                  :text "#+("))
                              :actual (tokens-for-string "#+(")))

        (clue:test "Valid ifdef macro"
            (clue:check-equal :expected (list (token:create
                                                  :type-value alive/types:*ifdef-false*
                                                  :start (pos:create 0 0)
                                                  :start-offset 0
                                                  :end (pos:create 0 10)
                                                  :end-offset 10
                                                  :text "#+(or a b)"))
                              :actual (tokens-for-string "#+(or a b)")))))


(defun test-parens ()
    (clue:test "Parens"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*open-paren*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 1)
                                              :end-offset 1
                                              :text "(")
                                          (token:create
                                              :type-value alive/types:*close-paren*
                                              :start (pos:create 0 1)
                                              :start-offset 1
                                              :end (pos:create 0 2)
                                              :end-offset 2
                                              :text ")"))
                          :actual (tokens-for-string "()"))))


(defun test-sym-with-pkg ()
    (clue:test "Symbol with package"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 3)
                                              :end-offset 3
                                              :text "foo")
                                          (token:create
                                              :type-value alive/types:*colons*
                                              :start (pos:create 0 3)
                                              :start-offset 3
                                              :end (pos:create 0 5)
                                              :end-offset 5
                                              :text "::")
                                          (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 5)
                                              :start-offset 5
                                              :end (pos:create 0 8)
                                              :end-offset 8
                                              :text "bar"))
                          :actual (tokens-for-string "foo::bar"))))


(defun test-vertical-bars ()
    (clue:test "Vertical bar name"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 9)
                                              :end-offset 9
                                              :text "|foo:bar|"))
                          :actual (tokens-for-string "|foo:bar|")))

    (clue:test "Unterminated vertical bar name"
        (clue:check-equal :expected (list (token:create
                                              :type-value alive/types:*symbol*
                                              :start (pos:create 0 0)
                                              :start-offset 0
                                              :end (pos:create 0 7)
                                              :end-offset 7
                                              :text "|foo:ba"))
                          :actual (tokens-for-string "|foo:ba"))))


(defun run-all ()
    (clue:suite "Parse tokens"
        (test-foo)
        (test-defun)
        (test-comment)
        (test-string)
        (test-basic-macro)
        (test-ifdef-macro)
        (test-parens)
        (test-sym-with-pkg)
        (test-vertical-bars)))
