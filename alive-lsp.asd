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
                                       (:file "lsp/types/format-options")
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
                                       (:file "lsp/message/format-utils")
                                       (:file "lsp/message/response")

                                       (:file "lsp/message/alive/debugger")
                                       (:file "lsp/message/alive/stderr")
                                       (:file "lsp/message/alive/stdout")
                                       (:file "lsp/message/workspace/config")

                                       (:file "lsp/packet")
                                       (:file "lsp/parse")

                                       (:file "session")
                                       (:file "server")))))


(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"
                 "clue")
    :components ((:module "test"
                          :components ((:file "compat/sbcl/compile")
                                       (:file "compat/sbcl/symbols")

                                       (:file "utils")

                                       (:file "parse/tokens")
                                       (:file "parse/forms")

                                       (:file "lsp/completions")
                                       (:file "lsp/sem-tokens")
                                       (:file "lsp/hover")

                                       (:file "session/messages")

                                       (:file "format/range")
                                       (:file "format/on-type")

                                       (:file "eval")
                                       (:file "inspector")
                                       (:file "streams")

                                       (:file "suite")))))
