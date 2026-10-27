(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:forms :alive/parse/forms)
                      (:pos :alive/position)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/test/lsp/completions)


(defun get-forms (text)
    (let ((tokens (tokenizer:xyz-from-stream (make-string-input-stream text))))
        (forms:from-tokens tokens)))


(defun test-symbols-m ()
    (clue:test "Symbols:m"
        (let* ((forms (get-forms "some text alive/symbols:m"))
               (actual (comps:xyz-simple :forms forms
                                         :pos (pos:create 0 25))))

            (clue:check-equal :expected 3
                              :actual (length actual)))))


(defun test-defmacro ()
    (clue:test "Defmacro"
        (let* ((forms (get-forms "some text cl-user:defmacro"))
               (actual (comps:xyz-simple :forms forms
                                         :pos (pos:create 0 26))))
            (clue:check-equal :expected 4
                              :actual (length actual)))))


(defun test-symbols ()
    (clue:test "Symbols"
        (let* ((forms (get-forms "alive/symbols:"))
               (actual (comps:xyz-simple :forms forms
                                         :pos (pos:create 0 14))))
            (clue:check-equal :expected 13
                              :actual (length actual)))))


(defun test-debug-io ()
    (clue:test "Debug-io"
        (let* ((forms (get-forms "*debug-io"))
               (actual (comps:xyz-simple :forms forms
                                         :pos (pos:create 0 9))))
            (clue:check-equal :expected 2
                              :actual (length actual)))))


(defun test-quote ()
    (clue:suite "Quote"
        (clue:test "Quote"
            (let* ((forms (get-forms "'*debug-io"))
                   (actual (comps:xyz-simple :forms forms
                                             :pos (pos:create 0 10))))
                (clue:check-equal :expected 2
                                  :actual (length actual))))

        (clue:test "Quote 2"
            (let* ((forms (get-forms "'\"\""))
                   (actual (comps:xyz-simple :forms forms
                                             :pos (pos:create 0 3))))
                (clue:check-equal :expected 0
                                  :actual (length actual))))))


(defun test-backquote ()
    (clue:test "Backquote"
        (let* ((forms (get-forms "`*debug-io"))
               (actual (comps:xyz-simple :forms forms
                                         :pos (pos:create 0 10))))
            (clue:check-equal :expected 2
                              :actual (length actual)))))


(defun test-colons ()
    (clue:suite "Colons"
        (clue:test "Colon symbol"
            (let* ((forms (get-forms ":cl-user"))
                   (actual (comps:xyz-simple :forms forms
                                             :pos (pos:create 0 8))))
                (clue:check-equal :expected 2
                                  :actual (length actual))))

        (clue:test "Colon no symbol"
            (let* ((forms (get-forms ":"))
                   (actual (comps:xyz-simple :forms forms
                                             :pos (pos:create 0 1))))
                (clue:check-equal :expected T
                                  :actual (< 100 (length actual)))))))


(defun test-symbol-no-pkg ()
    (clue:suite "Symbol without package"
        (clue:test "Packages without symbols"
            (let ((actual (comps::symbol-no-pkg :name "alive/test/eval" :pkg-name "cl-user")))
                (clue:check-equal :expected 2
                                  :actual (length actual))))

        (clue:test "Packages and symbols"
            (let ((actual (comps::symbol-no-pkg :name "zzz" :pkg-name "yyy")))
                (clue:check-equal :expected 0
                                  :actual (length actual))))))


(defun run-all ()
    (clue:suite "Completion Tests"
        (test-symbols-m)
        (test-defmacro)
        (test-symbols)
        (test-debug-io)
        (test-quote)
        (test-backquote)
        (test-colons)
        (test-symbol-no-pkg)))
