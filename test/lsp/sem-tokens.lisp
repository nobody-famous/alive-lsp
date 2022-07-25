(defpackage :alive/test/lsp/sem-tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/test/utils)
                      (:sem-types :alive/lsp/types/sem-tokens)))

(in-package :alive/test/lsp/sem-tokens)


(defun get-sem-tokens (text)
    (analysis:to-sem-tokens
        (tokenizer:from-stream
            (make-string-input-stream text))))


(defun check-symbol (text expected)
    (let ((tokens (get-sem-tokens text)))
        (clue:check-equal :expected (if expected
                                        (list expected)
                                        nil)
                          :actual tokens)))


(defun check-combo (text expected)
    (let ((tokens (get-sem-tokens text)))
        (clue:check-equal :expected expected
                          :actual tokens)))


(defun test-reader-macro ()
    (clue:test "Test reader macro"
        (check-symbol "#\\Replacement_Character" (sem-types:create
                                                     :token-type sem-types:*macro*
                                                     :line 0
                                                     :start 0
                                                     :end 23))))


(defun test-block-comment ()
    (clue:test "Test block comment"
        (check-symbol "#| Stuff |#" (sem-types:create
                                        :token-type sem-types:*comment*
                                        :line 0
                                        :start 0
                                        :end 11))))


(defun test-line-comment ()
    (clue:test "Test line comment"
        (check-symbol "; Stuff" (sem-types:create
                                    :token-type sem-types:*comment*
                                    :line 0
                                    :start 0
                                    :end 7))))


(defun test-string ()
    (clue:test "Test string"
        (check-symbol "\"String\"" (sem-types:create
                                       :token-type sem-types:*string*
                                       :line 0
                                       :start 0
                                       :end 8))))


(defun test-macro ()
    (clue:test "Test macro"
        (check-symbol "#foo" (sem-types:create
                                 :token-type sem-types:*macro*
                                 :line 0
                                 :start 0
                                 :end 4))))


(defun test-colon ()
    (clue:test "Test colon"
        (check-symbol ":" (sem-types:create
                              :token-type sem-types:*symbol*
                              :line 0
                              :start 0
                              :end 1))))


(defun test-double-colon ()
    (clue:test "Test double colon"
        (check-symbol "::" (sem-types:create
                               :token-type sem-types:*symbol*
                               :line 0
                               :start 0
                               :end 2))))


(defun test-integer ()
    (clue:test "Test integer"
        (check-symbol "123" (sem-types:create
                                :token-type sem-types:*number*
                                :line 0
                                :start 0
                                :end 3))))


(defun test-ratio ()
    (clue:test "Test ratio"
        (check-symbol "123/45" (sem-types:create
                                   :token-type sem-types:*number*
                                   :line 0
                                   :start 0
                                   :end 6))))


(defun test-float ()
    (clue:test "Test float"
        (check-symbol "123.45" (sem-types:create
                                   :token-type sem-types:*number*
                                   :line 0
                                   :start 0
                                   :end 6))))


(defun test-invalid-ratio ()
    (clue:test "Test invalid ratio"
        (check-symbol "123/" nil)))


(defun test-invalid-float ()
    (clue:test "Test invalid float"
        (check-symbol "123." nil)))


(defun test-open-parens ()
    (clue:test "Test open parens"
        (check-symbol "(" (sem-types:create
                              :token-type sem-types:*parenthesis*
                              :line 0
                              :start 0
                              :end 1))))


(defun test-close-parens ()
    (clue:test "Test close parens"
        (check-symbol ")" (sem-types:create
                              :token-type sem-types:*parenthesis*
                              :line 0
                              :start 0
                              :end 1))))


(defun test-foo ()
    (clue:test "Test foo"
        (check-symbol "foo" nil)))


(defun test-defun ()
    (clue:test "Test defun"
        (check-symbol "defun" (sem-types:create
                                  :token-type sem-types:*macro*
                                  :line 0
                                  :start 0
                                  :end 5))))


(defun test-open-comment ()
    (clue:test "Test open comment"
        (check-combo "#|" (list (sem-types:create
                                    :token-type sem-types:*comment*
                                    :line 0
                                    :start 0
                                    :end 2)))))


(defun test-in-pkg-macro ()
    (clue:test "Test in-package macro"
        (check-combo "(in-package #:alive/errors) start" (list (sem-types:create
                                                                   :token-type sem-types:*parenthesis*
                                                                   :line 0
                                                                   :start 0
                                                                   :end 1)
                                                               (sem-types:create
                                                                   :token-type sem-types:*macro*
                                                                   :line 0
                                                                   :start 1
                                                                   :end 11)
                                                               (sem-types:create
                                                                   :token-type sem-types:*macro*
                                                                   :line 0
                                                                   :start 12
                                                                   :end 26)
                                                               (sem-types:create
                                                                   :token-type sem-types:*parenthesis*
                                                                   :line 0
                                                                   :start 26
                                                                   :end 27)
                                                               (sem-types:create
                                                                   :token-type sem-types:*function*
                                                                   :line 0
                                                                   :start 28
                                                                   :end 33)))))


(defun test-in-pkg-symbol ()
    (clue:test "Test in-package symbol"
        (check-combo "(in-package :alive/errors) start" (list (sem-types:create
                                                                  :token-type sem-types:*parenthesis*
                                                                  :line 0
                                                                  :start 0
                                                                  :end 1)
                                                              (sem-types:create
                                                                  :token-type sem-types:*macro*
                                                                  :line 0
                                                                  :start 1
                                                                  :end 11)
                                                              (sem-types:create
                                                                  :token-type sem-types:*symbol*
                                                                  :line 0
                                                                  :start 12
                                                                  :end 13)
                                                              (sem-types:create
                                                                  :token-type sem-types:*symbol*
                                                                  :line 0
                                                                  :start 13
                                                                  :end 25)
                                                              (sem-types:create
                                                                  :token-type sem-types:*parenthesis*
                                                                  :line 0
                                                                  :start 25
                                                                  :end 26)
                                                              (sem-types:create
                                                                  :token-type sem-types:*function*
                                                                  :line 0
                                                                  :start 27
                                                                  :end 32)))))


(defun test-in-pkg-string ()
    (clue:test "Test in-package string"
        (check-combo "(in-package \"alive/errors\") start" (list (sem-types:create
                                                                     :token-type sem-types:*parenthesis*
                                                                     :line 0
                                                                     :start 0
                                                                     :end 1)
                                                                 (sem-types:create
                                                                     :token-type sem-types:*macro*
                                                                     :line 0
                                                                     :start 1
                                                                     :end 11)
                                                                 (sem-types:create
                                                                     :token-type sem-types:*string*
                                                                     :line 0
                                                                     :start 12
                                                                     :end 26)
                                                                 (sem-types:create
                                                                     :token-type sem-types:*parenthesis*
                                                                     :line 0
                                                                     :start 26
                                                                     :end 27)
                                                                 (sem-types:create
                                                                     :token-type sem-types:*function*
                                                                     :line 0
                                                                     :start 28
                                                                     :end 33)))))


(defun test-after-if-false ()
    (clue:test "Test after if-false"
        (check-combo "#+n (+ (+ (+ 'foo))) ()" (list (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 0
                                                         :end 3)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 4
                                                         :end 5)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 5
                                                         :end 6)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 7
                                                         :end 8)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 8
                                                         :end 9)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 10
                                                         :end 11)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 11
                                                         :end 12)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 13
                                                         :end 14)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 14
                                                         :end 17)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 17
                                                         :end 18)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 18
                                                         :end 19)
                                                     (sem-types:create
                                                         :token-type sem-types:*comment*
                                                         :line 0
                                                         :start 19
                                                         :end 20)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 21
                                                         :end 22)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 22
                                                         :end 23)))))


(defun test-defun-keys ()
    (clue:test "Test defun keys"
        (check-combo "(defun foo (&key a))" (list (sem-types:create
                                                      :token-type sem-types:*parenthesis*
                                                      :line 0
                                                      :start 0
                                                      :end 1)
                                                  (sem-types:create
                                                      :token-type sem-types:*macro*
                                                      :line 0
                                                      :start 1
                                                      :end 6)
                                                  (sem-types:create
                                                      :token-type sem-types:*parenthesis*
                                                      :line 0
                                                      :start 11
                                                      :end 12)
                                                  (sem-types:create
                                                      :token-type sem-types:*keyword*
                                                      :line 0
                                                      :start 12
                                                      :end 16)
                                                  (sem-types:create
                                                      :token-type sem-types:*parameter*
                                                      :line 0
                                                      :start 17
                                                      :end 18)
                                                  (sem-types:create
                                                      :token-type sem-types:*parenthesis*
                                                      :line 0
                                                      :start 18
                                                      :end 19)
                                                  (sem-types:create
                                                      :token-type sem-types:*parenthesis*
                                                      :line 0
                                                      :start 19
                                                      :end 20)))))


(defun test-if-false ()
    (clue:test "Test if false"
        (check-combo "#+n (* () () ())" (list (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 0
                                                  :end 3)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 4
                                                  :end 5)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 5
                                                  :end 6)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 7
                                                  :end 8)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 8
                                                  :end 9)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 10
                                                  :end 11)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 11
                                                  :end 12)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 13
                                                  :end 14)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 14
                                                  :end 15)
                                              (sem-types:create
                                                  :token-type sem-types:*comment*
                                                  :line 0
                                                  :start 15
                                                  :end 16)))))


(defun test-octets ()
    (clue:test "Test octets"
        (check-combo (format nil "(sb-ext:octets-to-string octets~%'())~%(+ () () ())")
                     (list (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 0
                               :start 0
                               :end 1)
                           (sem-types:create
                               :token-type sem-types:*namespace*
                               :line 0
                               :start 1
                               :end 7)
                           (sem-types:create
                               :token-type sem-types:*symbol*
                               :line 0
                               :start 7
                               :end 8)
                           (sem-types:create
                               :token-type sem-types:*function*
                               :line 0
                               :start 8
                               :end 24)
                           (sem-types:create
                               :token-type sem-types:*keyword*
                               :line 1
                               :start 0
                               :end 1)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 1
                               :start 1
                               :end 2)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 1
                               :start 2
                               :end 3)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 1
                               :start 3
                               :end 4)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 0
                               :end 1)
                           (sem-types:create
                               :token-type sem-types:*function*
                               :line 2
                               :start 1
                               :end 2)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 3
                               :end 4)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 4
                               :end 5)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 6
                               :end 7)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 7
                               :end 8)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 9
                               :end 10)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 10
                               :end 11)
                           (sem-types:create
                               :token-type sem-types:*parenthesis*
                               :line 2
                               :start 11
                               :end 12)))))


(defun test-long-ifdef ()
    (clue:test "Test long ifdef"
        (check-combo "#+nnnnn (or (not (not)))" (list (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 0
                                                          :end 7)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 8
                                                          :end 9)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 9
                                                          :end 11)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 12
                                                          :end 13)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 13
                                                          :end 16)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 17
                                                          :end 18)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 18
                                                          :end 21)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 21
                                                          :end 22)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 22
                                                          :end 23)
                                                      (sem-types:create
                                                          :token-type sem-types:*comment*
                                                          :line 0
                                                          :start 23
                                                          :end 24)))))


(defun test-do ()
    (clue:test "Test do"
        (check-combo ":do" (list (sem-types:create
                                     :token-type sem-types:*symbol*
                                     :line 0
                                     :start 0
                                     :end 1)
                                 (sem-types:create
                                     :token-type sem-types:*symbol*
                                     :line 0
                                     :start 1
                                     :end 3)))))


(defun test-quote-foo ()
    (clue:test "Test quote foo"
        (check-combo (format nil "'foo") (list (sem-types:create
                                                   :token-type sem-types:*keyword*
                                                   :line 0
                                                   :start 0
                                                   :end 1)))))


(defun test-list-spaces ()
    (clue:test "Test list spaces"
        (check-combo "(  )" (list (sem-types:create
                                      :token-type sem-types:*parenthesis*
                                      :line 0
                                      :start 0
                                      :end 1)
                                  (sem-types:create
                                      :token-type sem-types:*parenthesis*
                                      :line 0
                                      :start 3
                                      :end 4)))))


(defun test-unclosed-list ()
    (clue:test "Test unclosed list"
        (check-combo "(123" (list (sem-types:create
                                      :token-type sem-types:*parenthesis*
                                      :line 0
                                      :start 0
                                      :end 1)
                                  (sem-types:create
                                      :token-type sem-types:*number*
                                      :line 0
                                      :start 1
                                      :end 4)))))


(defun test-unopened-list ()
    (clue:test "Test unopened list"
        (check-combo "123)" (list (sem-types:create
                                      :token-type sem-types:*number*
                                      :line 0
                                      :start 0
                                      :end 3)
                                  (sem-types:create
                                      :token-type sem-types:*parenthesis*
                                      :line 0
                                      :start 3
                                      :end 4)))))


(defun test-pkg-symbol ()
    (clue:test "Test package symbol"
        (check-combo "foo:bar" (list (sem-types:create
                                         :token-type sem-types:*namespace*
                                         :line 0
                                         :start 0
                                         :end 3)
                                     (sem-types:create
                                         :token-type sem-types:*symbol*
                                         :line 0
                                         :start 3
                                         :end 4)))))


(defun test-pkg-symbol-double ()
    (clue:test "Test package symbol double colon"
        (check-combo "foo::bar" (list (sem-types:create
                                          :token-type sem-types:*namespace*
                                          :line 0
                                          :start 0
                                          :end 3)
                                      (sem-types:create
                                          :token-type sem-types:*symbol*
                                          :line 0
                                          :start 3
                                          :end 5)))))


(defun test-symbol-in-list ()
    (clue:test "Test symbol in list"
        (check-combo "( :bar )" (list (sem-types:create
                                          :token-type sem-types:*parenthesis*
                                          :line 0
                                          :start 0
                                          :end 1)
                                      (sem-types:create
                                          :token-type sem-types:*symbol*
                                          :line 0
                                          :start 2
                                          :end 3)
                                      (sem-types:create
                                          :token-type sem-types:*symbol*
                                          :line 0
                                          :start 3
                                          :end 6)
                                      (sem-types:create
                                          :token-type sem-types:*parenthesis*
                                          :line 0
                                          :start 7
                                          :end 8)))))


(defun test-text-after-ifdef ()
    (clue:test "Test text after ifdef"
        (check-combo "#+n (+ (+ 1)) (foo)" (list (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 0
                                                     :end 3)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 4
                                                     :end 5)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 5
                                                     :end 6)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 7
                                                     :end 8)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 8
                                                     :end 9)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 10
                                                     :end 11)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 11
                                                     :end 12)
                                                 (sem-types:create
                                                     :token-type sem-types:*comment*
                                                     :line 0
                                                     :start 12
                                                     :end 13)
                                                 (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 14
                                                     :end 15)
                                                 (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 18
                                                     :end 19)))))


(defun test-ifdef-in-list ()
    (clue:test "Test ifdef in list"
        (check-combo "( + ( #+n a a ) b ) ( c )" (list (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 0
                                                           :end 1)
                                                       (sem-types:create
                                                           :token-type sem-types:*function*
                                                           :line 0
                                                           :start 2
                                                           :end 3)
                                                       (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 4
                                                           :end 5)
                                                       (sem-types:create
                                                           :token-type sem-types:*comment*
                                                           :line 0
                                                           :start 6
                                                           :end 9)
                                                       (sem-types:create
                                                           :token-type sem-types:*comment*
                                                           :line 0
                                                           :start 10
                                                           :end 11)
                                                       (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 14
                                                           :end 15)
                                                       (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 18
                                                           :end 19)
                                                       (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 20
                                                           :end 21)
                                                       (sem-types:create
                                                           :token-type sem-types:*parenthesis*
                                                           :line 0
                                                           :start 24
                                                           :end 25)))))


(defun test-let ()
    (clue:test "Test let"
        (check-combo "( let ( ( a b ) ) NIL )" (list (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 0
                                                         :end 1)
                                                     (sem-types:create
                                                         :token-type sem-types:*keyword*
                                                         :line 0
                                                         :start 2
                                                         :end 5)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 6
                                                         :end 7)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 8
                                                         :end 9)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 14
                                                         :end 15)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 16
                                                         :end 17)
                                                     (sem-types:create
                                                         :token-type sem-types:*keyword*
                                                         :line 0
                                                         :start 18
                                                         :end 21)
                                                     (sem-types:create
                                                         :token-type sem-types:*parenthesis*
                                                         :line 0
                                                         :start 22
                                                         :end 23)))))


(defun test-defun-call ()
    (clue:test "Test defun call"
        (check-combo "(cl:defun)" (list (sem-types:create
                                            :token-type sem-types:*parenthesis*
                                            :line 0
                                            :start 0
                                            :end 1)
                                        (sem-types:create
                                            :token-type sem-types:*namespace*
                                            :line 0
                                            :start 1
                                            :end 3)
                                        (sem-types:create
                                            :token-type sem-types:*symbol*
                                            :line 0
                                            :start 3
                                            :end 4)
                                        (sem-types:create
                                            :token-type sem-types:*macro*
                                            :line 0
                                            :start 4
                                            :end 9)
                                        (sem-types:create
                                            :token-type sem-types:*parenthesis*
                                            :line 0
                                            :start 9
                                            :end 10)))))


(defun test-defun-with-ifdef ()
    (clue:test "Test defun with ifdef"
        (check-combo "(defun foo (#+n a b))" (list (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 0
                                                       :end 1)
                                                   (sem-types:create
                                                       :token-type sem-types:*macro*
                                                       :line 0
                                                       :start 1
                                                       :end 6)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 11
                                                       :end 12)
                                                   (sem-types:create
                                                       :token-type sem-types:*comment*
                                                       :line 0
                                                       :start 12
                                                       :end 15)
                                                   (sem-types:create
                                                       :token-type sem-types:*comment*
                                                       :line 0
                                                       :start 16
                                                       :end 17)
                                                   (sem-types:create
                                                       :token-type sem-types:*parameter*
                                                       :line 0
                                                       :start 18
                                                       :end 19)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 19
                                                       :end 20)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 20
                                                       :end 21)))))


(defun test-defun-spaces ()
    (clue:test "Test defun spaces"
        (check-combo "(defun foo ( ) bar)" (list (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 0
                                                     :end 1)
                                                 (sem-types:create
                                                     :token-type sem-types:*macro*
                                                     :line 0
                                                     :start 1
                                                     :end 6)
                                                 (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 11
                                                     :end 12)
                                                 (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 13
                                                     :end 14)
                                                 (sem-types:create
                                                     :token-type sem-types:*parenthesis*
                                                     :line 0
                                                     :start 18
                                                     :end 19)))))


(defun test-defun-nested-args ()
    (clue:test "Test defun nested arguments"
        (check-combo "( defun foo ( ( a b ) c ) )" (list (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 0
                                                             :end 1)
                                                         (sem-types:create
                                                             :token-type sem-types:*macro*
                                                             :line 0
                                                             :start 2
                                                             :end 7)
                                                         (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 12
                                                             :end 13)
                                                         (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 14
                                                             :end 15)
                                                         (sem-types:create
                                                             :token-type sem-types:*parameter*
                                                             :line 0
                                                             :start 16
                                                             :end 17)
                                                         (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 20
                                                             :end 21)
                                                         (sem-types:create
                                                             :token-type sem-types:*parameter*
                                                             :line 0
                                                             :start 22
                                                             :end 23)
                                                         (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 24
                                                             :end 25)
                                                         (sem-types:create
                                                             :token-type sem-types:*parenthesis*
                                                             :line 0
                                                             :start 26
                                                             :end 27)))))


(defun test-defun-args ()
    (clue:test "Test defun args"
        (check-combo "(defun foo (a b) nil)" (list (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 0
                                                       :end 1)
                                                   (sem-types:create
                                                       :token-type sem-types:*macro*
                                                       :line 0
                                                       :start 1
                                                       :end 6)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 11
                                                       :end 12)
                                                   (sem-types:create
                                                       :token-type sem-types:*parameter*
                                                       :line 0
                                                       :start 12
                                                       :end 13)
                                                   (sem-types:create
                                                       :token-type sem-types:*parameter*
                                                       :line 0
                                                       :start 14
                                                       :end 15)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 15
                                                       :end 16)
                                                   (sem-types:create
                                                       :token-type sem-types:*keyword*
                                                       :line 0
                                                       :start 17
                                                       :end 20)
                                                   (sem-types:create
                                                       :token-type sem-types:*parenthesis*
                                                       :line 0
                                                       :start 20
                                                       :end 21)))))


(defun run-all ()
    (clue:suite "Semantic Tokens"
        (test-reader-macro)
        (test-block-comment)
        (test-line-comment)
        (test-string)
        (test-macro)
        (test-colon)
        (test-double-colon)
        (test-integer)
        (test-ratio)
        (test-float)
        (test-invalid-ratio)
        (test-invalid-float)
        (test-open-parens)
        (test-close-parens)
        (test-foo)
        (test-defun)
        (test-open-comment)
        (test-in-pkg-macro)
        (test-in-pkg-symbol)
        (test-in-pkg-string)
        (test-after-if-false)
        (test-defun-keys)
        (test-if-false)
        (test-octets)
        (test-long-ifdef)
        (test-do)
        (test-quote-foo)
        (test-list-spaces)
        (test-unclosed-list)
        (test-unopened-list)
        (test-pkg-symbol)
        (test-pkg-symbol-double)
        (test-symbol-in-list)
        (test-text-after-ifdef)
        (test-ifdef-in-list)
        (test-let)
        (test-defun-call)
        (test-defun-with-ifdef)
        (test-defun-spaces)
        (test-defun-nested-args)
        (test-defun-args)))
