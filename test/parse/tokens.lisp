(defpackage :alive/test/parse/tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)

                      (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokens :alive/parse/tokenizer)))

(in-package :alive/test/parse/tokens)


(defun token-for-string (str)
    (with-input-from-string (s str)
        (tokens:from-stream s)))


(defun atoms ()
    (run:test "Test atoms"
              (lambda ()
                  (check:are-equal (list (token:create
                                          :type-value alive/types:*symbol*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 3)
                                          :text "foo"))
                                   (token-for-string "foo"))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*symbol*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 5)
                                          :text "defun"))
                                   (token-for-string "defun"))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*line-comment*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 9)
                                          :text "; Comment"))
                                   (token-for-string "; Comment"))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*string*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 8)
                                          :text "\"String\""))
                                   (token-for-string "\"String\""))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*macro*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 6)
                                          :text "#'abcd"))
                                   (token-for-string "#'abcd"))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*ifdef-false*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 3)
                                          :text "#+n"))
                                   (token-for-string "#+n"))

                  (check:are-equal (list (token:create
                                          :type-value alive/types:*open-paren*
                                          :start (pos:create 0 0)
                                          :end (pos:create 0 1)
                                          :text "(")
                                         (token:create
                                          :type-value alive/types:*close-paren*
                                          :start (pos:create 0 1)
                                          :end (pos:create 0 2)
                                          :text ")"))
                                   (token-for-string "()"))

                  (check:are-equal (list (token:create
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
                                   (token-for-string "foo::bar")))))


(defun run-all ()
    (run:suite "Test parse tokens"
               (lambda ()
                   (atoms))))
