(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:pos :alive/position)))

(in-package :alive/test/lsp/completions)

(defun test-symbols-m ()
    (clue:test "Test symbols:m"
        (let ((actual (comps:simple :text "some text alive/symbols:m"
                                    :pos (pos:create 0 27))))

            (clue:check-equal :expected 3
                              :actual (length actual)))))


(defun test-defmacro ()
    (clue:test "Test defmacro"
        (let ((actual (comps:simple :text "some text cl-user:defmacro"
                                    :pos (pos:create 0 26))))

            (clue:check-equal :expected 4
                              :actual (length actual)))))


(defun test-symbols ()
    (clue:test "Test symbols"
        (let ((actual (comps:simple :text "alive/symbols:"
                                    :pos (pos:create 0 14))))
            (clue:check-equal :expected 10
                              :actual (length actual)))))


(defun test-debug-io ()
    (clue:test "Test debug-io"
        (let ((actual (comps:simple :text "*debug-io"
                                    :pos (pos:create 0 9))))
            (clue:check-equal :expected 2
                              :actual (length actual)))))


(defun run-all ()
    (clue:suite "Completion Tests"
        (test-symbols-m)
        (test-defmacro)
        (test-symbols)
        (test-debug-io)))