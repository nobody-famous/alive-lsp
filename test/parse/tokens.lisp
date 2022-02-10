(defpackage :alive/test/parse/tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)

                      (:pos :alive/parse/pos)
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
                                          :start (pos:create :line 0 :col 0)
                                          :end (pos:create :line 0 :col 3)
                                          :text "foo"))
                                   (token-for-string "foo")))))


(defun run-all ()
    (run:suite "Test parse tokens"
               (lambda ()
                   (atoms))))
