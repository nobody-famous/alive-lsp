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
                      (check:are-equal (list expected) tokens))))

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
                      (check-symbol "123/" (sem-types:create
                                            :token-type sem-types:*symbol*
                                            :line 0
                                            :start 0
                                            :end 4))
                      (check-symbol "123." (sem-types:create
                                            :token-type sem-types:*symbol*
                                            :line 0
                                            :start 0
                                            :end 4))
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
                      (check-symbol "foo" (sem-types:create
                                           :token-type sem-types:*symbol*
                                           :line 0
                                           :start 0
                                           :end 3))
                      (check-symbol "defun" (sem-types:create
                                             :token-type sem-types:*keyword*
                                             :line 0
                                             :start 0
                                             :end 5))))))


(defun combos ()
    (labels ((check-combo (text expected)
                  (let ((tokens (get-sem-tokens text)))
                      (check:are-equal expected tokens))))

        (run:test "Combos"
                  (lambda ()
                      (check-combo "()" (list (sem-types:create
                                               :token-type sem-types:*parenthesis*
                                               :line 0
                                               :start 0
                                               :end 1)
                                              (sem-types:create
                                               :token-type sem-types:*parenthesis*
                                               :line 0
                                               :start 1
                                               :end 2)))
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
                                                    :end 4)
                                                   (sem-types:create
                                                    :token-type sem-types:*symbol*
                                                    :line 0
                                                    :start 4
                                                    :end 7)))
                      (check-combo "foo::bar" (list (sem-types:create
                                                     :token-type sem-types:*namespace*
                                                     :line 0
                                                     :start 0
                                                     :end 3)
                                                    (sem-types:create
                                                     :token-type sem-types:*symbol*
                                                     :line 0
                                                     :start 3
                                                     :end 5)
                                                    (sem-types:create
                                                     :token-type sem-types:*symbol*
                                                     :line 0
                                                     :start 5
                                                     :end 8)))))))


(defun run-all ()
    (run:suite "Semantic Tokens"
               (lambda ()
                   (symbols)
                   (combos))))
