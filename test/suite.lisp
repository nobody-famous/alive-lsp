(defpackage :alive/test/suite
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/suite)


(defun run-all ()
    (clue:suite "Run all Alive LSP tests"
        (alive/test/lsp/completions:run-all)
        (alive/test/lsp/sem-tokens:run-all)
        (alive/test/lsp/hover:run-all)
        (alive/test/parse/tokens:run-all)
        (alive/test/parse/forms:run-all)
        (alive/test/session/messages:run-all)
        (alive/test/format/range:run-all)
        (alive/test/eval:run-all)
        (alive/test/streams:run-all)
        (alive/test/forms:run-all)
        (alive/test/asdf/load:run-all)
        (alive/test/compile:run-all)))
