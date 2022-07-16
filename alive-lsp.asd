(in-package :asdf-user)


(defsystem "alive-lsp"
    :description ""
    :serial t
    :depends-on (#:usocket
                 #:cl-json
                 #:bordeaux-threads
                 #:flexi-streams
                 #:sb-introspect)

    :components ((:module "src"
                          :components ((:file "logger")
                                       (:file "types")
                                       (:file "errors")
                                       (:file "compile-message")
                                       (:file "position")
                                       (:file "range")
                                       (:file "text-edit")

                                       (:file "lsp/types/config-item")
                                       (:file "lsp/types/sem-tokens")
                                       (:file "lsp/types/text-doc")
                                       (:file "lsp/types/text-doc-item")
                                       (:file "lsp/types/format-options")
                                       (:file "lsp/types/formatting-options")
                                       (:file "lsp/types/debug-resp")
                                       (:file "lsp/types/user-input")
                                       (:file "lsp/types/restart-info")

                                       (:file "parse/stream")
                                       (:file "parse/token")
                                       (:file "parse/tokenizer")
                                       (:file "parse/form")
                                       (:file "parse/forms")

                                       (:file "compat/sbcl/file")
                                       (:file "compat/sbcl/streams")
                                       (:file "compat/sbcl/symbols")
                                       (:file "compat/sbcl/threads")

                                       (:file "streams")
                                       (:file "file")
                                       (:file "symbols")
                                       (:file "packages")
                                       (:file "format")
                                       (:file "threads")
                                       (:file "eval")
                                       (:file "inspector")
                                       (:file "asdf")

                                       (:file "lsp/errors")
                                       (:file "lsp/utils")
                                       (:file "lsp/completions")
                                       (:file "lsp/hover")
                                       (:file "lsp/symbol")

                                       (:file "lsp/sem-analysis")

                                       (:file "lsp/message/abstract")
                                       (:file "lsp/message/payload")
                                       (:file "lsp/message/initialize")
                                       (:file "lsp/message/alive/debugger")
                                       (:file "lsp/message/alive/do-eval")
                                       (:file "lsp/message/alive/do-inspect")
                                       (:file "lsp/message/alive/get-pkg")
                                       (:file "lsp/message/alive/remove-pkg")
                                       (:file "lsp/message/alive/load-asdf")
                                       (:file "lsp/message/alive/list-asdf")
                                       (:file "lsp/message/alive/list-packages")
                                       (:file "lsp/message/alive/list-threads")
                                       (:file "lsp/message/alive/kill-thread")
                                       (:file "lsp/message/alive/load-file")
                                       (:file "lsp/message/alive/symbol")
                                       (:file "lsp/message/alive/try-compile")
                                       (:file "lsp/message/alive/unexport-symbol")
                                       (:file "lsp/message/alive/stderr")
                                       (:file "lsp/message/alive/stdout")
                                       (:file "lsp/message/alive/top-form")
                                       (:file "lsp/message/alive/user-input")
                                       (:file "lsp/message/document/completion")
                                       (:file "lsp/message/document/did-change")
                                       (:file "lsp/message/document/did-open")
                                       (:file "lsp/message/document/hover")
                                       (:file "lsp/message/document/format-utils")
                                       (:file "lsp/message/document/range-format")
                                       (:file "lsp/message/document/fmt-on-type")
                                       (:file "lsp/message/document/sem-tokens-full")
                                       (:file "lsp/message/workspace/config")

                                       (:file "lsp/packet")
                                       (:file "lsp/parse")

                                       (:file "session")
                                       (:file "server")))))


(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"
                 "clue")
    :components ((:module "test"
                          :components ((:file "compare")
                                       
                                       (:file "compat/sbcl/compile")
                                       (:file "compat/sbcl/symbols")

                                       (:file "utils")

                                       (:file "parse/tokens")
                                       (:file "parse/forms")

                                       (:file "lsp/completions")
                                       (:file "lsp/message")
                                       (:file "lsp/sem-tokens")
                                       (:file "lsp/hover")

                                       (:file "session/messages")

                                       (:file "format/range")
                                       (:file "format/on-type")

                                       (:file "eval")
                                       (:file "inspector")
                                       (:file "streams")

                                       (:file "suite")))))
