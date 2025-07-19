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
                                       (:file "location")
                                       (:file "text-edit")

                                       (:file "deps")

                                       (:file "parse/token")
                                       (:file "parse/tokenizer")
                                       (:file "parse/form")
                                       (:file "parse/forms")

                                       (:file "compat/sbcl/file")
                                       (:file "compat/sbcl/streams")
                                       (:file "compat/sbcl/symbols")
                                       (:file "compat/sbcl/threads")

                                       (:file "packages")
                                       (:file "symbols")

                                       (:file "sys/streams")
                                       (:file "sys/asdf")
                                       (:file "sys/eval")
                                       (:file "sys/threads")
                                       (:file "sys/traced-fns")
                                       (:file "sys/xref")

                                       (:file "file")
                                       (:file "format")
                                       (:file "frames")
                                       (:file "debugger")
                                       (:file "macros")
                                       (:file "selection")
                                       (:file "file-utils")
                                       (:file "inspector")

                                       (:file "lsp/types/config-item")
                                       (:file "lsp/types/sem-tokens")
                                       (:file "lsp/types/format-options")
                                       (:file "lsp/types/restart-info")

                                       (:file "lsp/errors")
                                       (:file "lsp/completions")
                                       (:file "lsp/definition")
                                       (:file "lsp/hover")
                                       (:file "lsp/sig-help")
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

                                       (:file "session/spawn")
                                       (:file "session/refresh")
                                       (:file "session/threads")
                                       (:file "session/transport")

                                       (:file "session/handler/utils")
                                       (:file "session/handler/compile")
                                       (:file "session/handler/document")
                                       (:file "session/handler/init")
                                       (:file "session/handler/inspect")
                                       (:file "session/handler/form-bounds")
                                       (:file "session/handler/macro")
                                       (:file "session/handler/packages")
                                       (:file "session/handler/symbol")
                                       (:file "session/handler/threads")
                                       (:file "session/handler/traced-fns")
                                       (:file "session/handler/asdf")
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
                                       (:file "lsp/sig-help")
                                       (:file "lsp/hover")
                                       (:file "lsp/packet")

                                       (:file "deps")
                                       (:file "session/handlers")
                                       (:file "session/handler/asdf")
                                       (:file "session/handler/compile")
                                       (:file "session/handler/document")
                                       (:file "session/handler/eval")
                                       (:file "session/handler/form-bounds")
                                       (:file "session/handler/init")
                                       (:file "session/handler/inspect")
                                       (:file "session/handler/macro")
                                       (:file "session/handler/packages")
                                       (:file "session/handler/symbol")
                                       (:file "session/handler/threads")
                                       (:file "session/handler/traced-fns")
                                       (:file "session/handler/utils")
                                       (:file "session/message")
                                       (:file "session/message-loop")
                                       (:file "session/refresh")
                                       (:file "session/state")

                                       (:file "format/range")
                                       (:file "format/on-type")
                                       (:file "format/format-utils")

                                       (:file "asdf/load")

                                       (:file "log")
                                       (:file "compile")
                                       (:file "eval")
                                       (:file "file-utils")
                                       (:file "forms")
                                       (:file "macros")
                                       (:file "inspector")
                                       (:file "position")
                                       (:file "range")
                                       (:file "selection")
                                       (:file "streams")

                                       (:file "suite")
                                       (:file "coverage")))))
