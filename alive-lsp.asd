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
                          :components ((:file "utils")
                                       (:file "logger")
                                       (:file "types")
                                       (:file "errors")
                                       (:file "compile-message")
                                       (:file "position")
                                       (:file "range")
                                       (:file "text-edit")

                                       (:file "context")

                                       (:file "parse/token")
                                       (:file "parse/tokenizer")
                                       (:file "parse/form")
                                       (:file "parse/forms")

                                       (:file "compat/sbcl/file")
                                       (:file "compat/sbcl/streams")
                                       (:file "compat/sbcl/symbols")
                                       (:file "compat/sbcl/threads")

                                       (:file "deps")
                                       (:file "streams")
                                       (:file "file")
                                       (:file "symbols")
                                       (:file "packages")
                                       (:file "format")
                                       (:file "threads")
                                       (:file "frames")
                                       (:file "eval")
                                       (:file "inspector")
                                       (:file "asdf")
                                       (:file "debugger")
                                       (:file "macros")
                                       (:file "selection")
                                       (:file "file-utils")

                                       (:file "lsp/types/config-item")
                                       (:file "lsp/types/sem-tokens")
                                       (:file "lsp/types/format-options")
                                       (:file "lsp/types/restart-info")

                                       (:file "lsp/errors")
                                       (:file "lsp/utils")
                                       (:file "lsp/completions")
                                       (:file "lsp/definition")
                                       (:file "lsp/hover")
                                       (:file "lsp/symbol")

                                       (:file "lsp/sem-analysis")

                                       (:file "lsp/packet")
                                       (:file "lsp/parse")

                                       (:file "lsp/message/abstract")
                                       (:file "lsp/message/format-utils")
                                       (:file "lsp/message/request")
                                       (:file "lsp/message/response")
                                       (:file "lsp/message/notification")

                                       (:file "session/state")
                                       (:file "session/handlers")
                                       (:file "thread-utils")

                                       (:file "session/handler/utils")
                                       (:file "session/handler/document")
                                       (:file "session/handler/init")
                                       (:file "session/handler/form-bounds")
                                       (:file "session/handler/packages")
                                       (:file "session/handler/threads")
                                       (:file "session/refresh")
                                       (:file "session/threads")
                                       (:file "session/handler/eval")
                                       (:file "session/message")
                                       (:file "session/message-loop")
                                       (:file "session")

                                       (:file "server")))))


(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"
                 "clue"
                 "sb-cover")
    :components ((:module "test"
                          :components ((:file "compat/sbcl/compile")
                                       (:file "compat/sbcl/symbols")

                                       (:file "utils")

                                       (:file "parse/tokens")
                                       (:file "parse/forms")

                                       (:file "lsp/completions")
                                       (:file "lsp/sem-analysis")
                                       (:file "lsp/sem-tokens")
                                       (:file "lsp/hover")
                                       (:file "lsp/message")
                                       (:file "lsp/packet")

                                       (:file "session/messages")
                                       (:file "deps")
                                       (:file "session/handlers")
                                       (:file "session/handler/document")
                                       (:file "session/handler/eval")
                                       (:file "session/handler/form-bounds")
                                       (:file "session/handler/init")
                                       (:file "session/handler/packages")
                                       (:file "session/handler/threads")
                                       (:file "session/handler/utils")
                                       (:file "session/message")
                                       (:file "session/message-loop")
                                       (:file "session/refresh")
                                       (:file "session/state")
                                       (:file "session/threads")

                                       (:file "format/range")
                                       (:file "format/on-type")
                                       (:file "format/format-utils")

                                       (:file "asdf/load")

                                       (:file "log")
                                       (:file "compile")
                                       (:file "context")
                                       (:file "debugger")
                                       (:file "eval")
                                       (:file "forms")
                                       (:file "macros")
                                       (:file "inspector")
                                       (:file "position")
                                       (:file "range")
                                       (:file "selection")
                                       (:file "session")
                                       (:file "streams")

                                       (:file "suite")
                                       (:file "coverage")))))
