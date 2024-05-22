(defpackage :alive/test/suite
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/suite)


(defun run-all ()
    (clue:suite "Run all Alive LSP tests"
        (alive/test/asdf/load:run-all)
        (alive/test/compile:run-all)
        (alive/test/context:run-all)
        (alive/test/debugger:run-all)
        (alive/test/eval:run-all)
        (alive/test/format/on-type:run-all)
        (alive/test/format/range:run-all)
        (alive/test/format/utils:run-all)
        (alive/test/forms:run-all)
        (alive/test/inspector:run-all)
        (alive/test/log:run-all)
        (alive/test/lsp/completions:run-all)
        (alive/test/lsp/sem-analysis:run-all)
        (alive/test/lsp/sem-tokens:run-all)
        (alive/test/lsp/hover:run-all)
        (alive/test/lsp/message:run-all)
        (alive/test/lsp/packet:run-all)
        (alive/test/parse/tokens:run-all)
        (alive/test/parse/forms:run-all)
        (alive/test/position:run-all)
        (alive/test/range:run-all)
        (alive/test/session:run-all)
        (alive/test/session/messages:run-all)
        (alive/test/deps:run-all)
        (alive/test/session/handlers:run-all)
        (alive/test/session/handler/document:run-all)
        (alive/test/session/handler/eval:run-all)
        (alive/test/session/handler/form-bounds:run-all)
        (alive/test/session/handler/init:run-all)
        (alive/test/session/handler/packages:run-all)
        (alive/test/session/handler/utils:run-all)
        (alive/test/session/message:run-all)
        (alive/test/session/message-loop:run-all)
        (alive/test/session/state:run-all)
        (alive/test/session/threads:run-all)
        (alive/test/selection:run-all)
        (alive/test/streams:run-all)))
