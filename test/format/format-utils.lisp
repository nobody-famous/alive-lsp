(defpackage :alive/test/format/utils
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:utils :alive/lsp/message/format-utils)))

(in-package :alive/test/format/utils)


(defun test-to-lsp-range ()
    (clue:test "To LSP range"
        (let ((actual (utils::to-lsp-range (alive/range:create (alive/position:create 1 2)
                                                               (alive/position:create 3 4)))))
            (clue:check-equal :expected (alive/position:create 1 2)
                              :actual (gethash "start" actual))
            (clue:check-equal :expected (alive/position:create 3 4)
                              :actual (gethash "end" actual)))))


(defun run-all ()
    (clue:suite "Format Utils"
        (test-to-lsp-range)))
