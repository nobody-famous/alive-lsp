(defpackage :alive/test/lsp/sem-tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:tokenizer :alive/parse/tokenizer)
                      (:sem-types :alive/lsp/types/sem-tokens)

                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/lsp/sem-tokens)


(defun get-sem-tokens (text)
    (analysis:to-sem-tokens
     (tokenizer:from-stream
      (make-string-input-stream text))))


(defun symbols ()
    (labels ((check-symbol (text expected)
                  (let ((tokens (get-sem-tokens text)))
                      (check:are-equal (if expected
                                           (list expected)
                                           nil)
                                       tokens))))

        (run:test "Symbols"
                  (lambda ()
                      (check-symbol "#| Stuff |#" (sem-types:create
                                                   :token-type sem-types:*comment*
                                                   :line 0
                                                   :start 0
                                                   :end 11))
                      (check-symbol "; Stuff" (sem-types:create
                                               :token-type sem-types:*comment*
                                               :line 0
                                               :start 0
                                               :end 7))
                      (check-symbol "\"String\"" (sem-types:create
                                                  :token-type sem-types:*string*
                                                  :line 0
                                                  :start 0
                                                  :end 8))
                      (check-symbol "#foo" (sem-types:create
                                            :token-type sem-types:*macro*
                                            :line 0
                                            :start 0
                                            :end 4))
                      (check-symbol ":" (sem-types:create
                                         :token-type sem-types:*symbol*
                                         :line 0
                                         :start 0
                                         :end 1))
                      (check-symbol "::" (sem-types:create
                                          :token-type sem-types:*symbol*
                                          :line 0
                                          :start 0
                                          :end 2))
                      (check-symbol "123" (sem-types:create
                                           :token-type sem-types:*number*
                                           :line 0
                                           :start 0
                                           :end 3))
                      (check-symbol "123/45" (sem-types:create
                                              :token-type sem-types:*number*
                                              :line 0
                                              :start 0
                                              :end 6))
                      (check-symbol "123.45" (sem-types:create
                                              :token-type sem-types:*number*
                                              :line 0
                                              :start 0
                                              :end 6))
                      (check-symbol "123/" nil)
                      (check-symbol "123." nil)
                      (check-symbol "(" (sem-types:create
                                         :token-type sem-types:*parenthesis*
                                         :line 0
                                         :start 0
                                         :end 1))
                      (check-symbol ")" (sem-types:create
                                         :token-type sem-types:*parenthesis*
                                         :line 0
                                         :start 0
                                         :end 1))
                      (check-symbol "foo" nil)
                      (check-symbol "defun" (sem-types:create
                                             :token-type sem-types:*macro*
                                             :line 0
                                             :start 0
                                             :end 5))))))


(defun combos ()
    (labels ((check-combo (text expected)
                  (let ((tokens (get-sem-tokens text)))
                      (check:are-equal expected tokens))))

        (run:test "Combos"
                  (lambda ()
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
                                                                     :end 24)))
                      (check-combo ":do" (list (sem-types:create
                                                :token-type sem-types:*symbol*
                                                :line 0
                                                :start 0
                                                :end 1)
                                               (sem-types:create
                                                :token-type sem-types:*symbol*
                                                :line 0
                                                :start 1
                                                :end 3)))
                      (check-combo (format nil "foo") (list))
                      (check-combo (format nil "'foo") (list (sem-types:create
                                                              :token-type sem-types:*symbol*
                                                              :line 0
                                                              :start 0
                                                              :end 1)))
                      (check-combo "(  )" (list (sem-types:create
                                                 :token-type sem-types:*parenthesis*
                                                 :line 0
                                                 :start 0
                                                 :end 1)
                                                (sem-types:create
                                                 :token-type sem-types:*parenthesis*
                                                 :line 0
                                                 :start 3
                                                 :end 4)))
                      (check-combo "(123" (list (sem-types:create
                                                 :token-type sem-types:*parenthesis*
                                                 :line 0
                                                 :start 0
                                                 :end 1)
                                                (sem-types:create
                                                 :token-type sem-types:*number*
                                                 :line 0
                                                 :start 1
                                                 :end 4)))
                      (check-combo "123)" (list (sem-types:create
                                                 :token-type sem-types:*number*
                                                 :line 0
                                                 :start 0
                                                 :end 3)
                                                (sem-types:create
                                                 :token-type sem-types:*parenthesis*
                                                 :line 0
                                                 :start 3
                                                 :end 4)))
                      (check-combo "foo:bar" (list (sem-types:create
                                                    :token-type sem-types:*namespace*
                                                    :line 0
                                                    :start 0
                                                    :end 3)
                                                   (sem-types:create
                                                    :token-type sem-types:*symbol*
                                                    :line 0
                                                    :start 3
                                                    :end 4)))
                      (check-combo "foo::bar" (list (sem-types:create
                                                     :token-type sem-types:*namespace*
                                                     :line 0
                                                     :start 0
                                                     :end 3)
                                                    (sem-types:create
                                                     :token-type sem-types:*symbol*
                                                     :line 0
                                                     :start 3
                                                     :end 5)))
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
                                                     :end 8)))
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
                                                                :end 19)))
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
                                                                      :end 25)))
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
                                                                    :token-type sem-types:*symbol*
                                                                    :line 0
                                                                    :start 18
                                                                    :end 21)
                                                                   (sem-types:create
                                                                    :token-type sem-types:*parenthesis*
                                                                    :line 0
                                                                    :start 22
                                                                    :end 23)))
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
                                                       :end 10)))
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
                                                                  :end 21)))
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
                                                                :end 19)))
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
                                                                        :end 27)))
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
                                                                  :token-type sem-types:*symbol*
                                                                  :line 0
                                                                  :start 17
                                                                  :end 20)
                                                                 (sem-types:create
                                                                  :token-type sem-types:*parenthesis*
                                                                  :line 0
                                                                  :start 20
                                                                  :end 21)))))))


(defun run-all ()
    (run:suite "Semantic Tokens"
               (lambda ()
                   (symbols)
                   (combos))))
