(defpackage :alive/test/suite
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:run :alive/test/harness/run)))

(in-package :alive/test/suite)


(defun run-all ()
    (run:suite "Run all Alive LSP tests"
               (lambda ()
                   (alive/test/lsp/completions:run-all)
                   (alive/test/lsp/message:run-all)
                   (alive/test/lsp/sem-tokens:run-all)
                   (alive/test/parse/tokens:run-all)
                   (alive/test/parse/forms:run-all)
                   (alive/test/session/messages:run-all)
                   (alive/test/format/range:run-all)
                   (alive/test/eval:run-all))))
