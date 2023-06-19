(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:pos :alive/position)))

(in-package :alive/test/lsp/completions)

(defun test-symbols-m ()
    (clue:test "Symbols:m"
        (let ((actual (comps:simple :text "some text alive/symbols:m"
                                    :pos (pos:create 0 27))))

            (clue:check-equal :expected 3
                              :actual (length actual)))))


(defun test-defmacro ()
    (clue:test "Defmacro"
        (let ((actual (comps:simple :text "some text cl-user:defmacro"
                                    :pos (pos:create 0 26))))

            (clue:check-equal :expected 4
                              :actual (length actual)))))


(defun test-symbols ()
    (clue:test "Symbols"
        (let ((actual (comps:simple :text "alive/symbols:"
                                    :pos (pos:create 0 14))))
            (clue:check-equal :expected 10
                              :actual (length actual)))))


(defun test-debug-io ()
    (clue:test "Debug-io"
        (let ((actual (comps:simple :text "*debug-io"
                                    :pos (pos:create 0 9))))
            (clue:check-equal :expected 2
                              :actual (length actual)))))


(defun test-quote ()
    (clue:test "Quote"
        (let ((actual (comps:simple :text "'*debug-io"
                                    :pos (pos:create 0 10))))
            (clue:check-equal :expected 2
                              :actual (length actual))))

    (clue:test "Quote 2"
        (let ((actual (comps:simple :text "'\"\""
                                    :pos (pos:create 0 3))))
            (clue:check-equal :expected 0
                              :actual (length actual)))))


(defun test-backquote ()
    (clue:test "Backquote"
        (let ((actual (comps:simple :text "`*debug-io"
                                    :pos (pos:create 0 10))))
            (clue:check-equal :expected 2
                              :actual (length actual)))))


(defun test-colons ()
    (clue:test "Colon symbol"
        (let ((actual (comps:simple :text ":cl-user"
                                    :pos (pos:create 0 8))))
            (clue:check-equal :expected 2
                              :actual (length actual))))

    (clue:test "Colon no symbol"
        (let ((actual (comps:simple :text ":"
                                    :pos (pos:create 0 8))))
            (clue:check-equal :expected T
                              :actual (< 100 (length actual))))))


(defun test-symbol-no-pkg ()
    (clue:test "Packages without symbols"
        (let ((actual (comps::symbol-no-pkg :name "alive/test/eval" :pkg-name "cl-user")))
            (clue:check-equal :expected 1
                              :actual (length actual))))

    (clue:test "Packages and symbols"
        (let ((actual (comps::symbol-no-pkg :name "zzz" :pkg-name "yyy")))
            (clue:check-equal :expected 0
                              :actual (length actual)))))


(defun test-to-snippet ()
    (clue:test "To snippet"
        (let ((actual (comps::to-snippet "foo" 5)))
            (clue:check-equal :expected nil :actual actual))))


(defun run-all ()
    (clue:suite "Completion Tests"
        (test-symbols-m)
        (test-defmacro)
        (test-symbols)
        (test-debug-io)
        (test-quote)
        (test-backquote)
        (test-colons)
        (test-symbol-no-pkg)
        (test-to-snippet)))