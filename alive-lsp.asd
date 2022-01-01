(in-package :asdf-user)

(defsystem "alive-lsp"
    :class :package-inferred-system
    :depends-on (#:usocket
                 #:cl-json
                 #:bordeaux-threads
                 #:purl

                 "alive-lsp/src/logger"

                 "alive-lsp/src/types"

                 "alive-lsp/src/parse/stream"
                 "alive-lsp/src/parse/pos"
                 "alive-lsp/src/parse/token"
                 "alive-lsp/src/parse/tokenizer"

                 "alive-lsp/src/compat/sbcl/compile"
                 "alive-lsp/src/compat/sbcl/streams"

                 "alive-lsp/src/compile"

                 "alive-lsp/src/lsp/types/sem-tokens"
                 "alive-lsp/src/lsp/types/text-doc"

                 "alive-lsp/src/lsp/sem-analysis"

                 "alive-lsp/src/lsp/message/abstract"
                 "alive-lsp/src/lsp/message/payload"
                 "alive-lsp/src/lsp/message/initialize"
                 "alive-lsp/src/lsp/message/document/did-open"
                 "alive-lsp/src/lsp/message/document/sem-tokens-full"

                 "alive-lsp/src/lsp/packet"
                 "alive-lsp/src/lsp/parse"

                 "alive-lsp/src/socket-pair"
                 "alive-lsp/src/session"
                 "alive-lsp/src/server"))

(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"

                 "alive-lsp/test/compat/sbcl/compile"
                 "alive-lsp/test/parse"
                 "alive-lsp/test/lsp/message"))

(register-system-packages "alive-lsp/src/logger" '(:alive/logger))

(register-system-packages "alive-lsp/src/types" '(:alive/types))

(register-system-packages "alive-lsp/src/parse/stream" '(:alive/parse/stream))
(register-system-packages "alive-lsp/src/parse/pos" '(:alive/parse/pos))
(register-system-packages "alive-lsp/src/parse/token" '(:alive/parse/token))
(register-system-packages "alive-lsp/src/parse/tokenizer" '(:alive/parse/tokenizer))

(register-system-packages "alive-lsp/src/compile" '(:alive/compile))

(register-system-packages "alive-lsp/src/compat/sbcl/compile" '(:alive/compile/compat))
(register-system-packages "alive-lsp/src/compat/sbcl/streams" '(:alive/streams))

(register-system-packages "alive-lsp/src/lsp/types/sem-tokens" '(:alive/lsp/types/sem-tokens))
(register-system-packages "alive-lsp/src/lsp/types/text-doc" '(:alive/lsp/types/text-doc))

(register-system-packages "alive-lsp/src/lsp/sem-analysis" '(:alive/lsp/sem-analysis))

(register-system-packages "alive-lsp/src/lsp/message/abstract" '(:alive/lsp/message/abstract))
(register-system-packages "alive-lsp/src/lsp/message/payload" '(:alive/lsp/message/payload))
(register-system-packages "alive-lsp/src/lsp/message/initialize" '(:alive/lsp/message/initialize))
(register-system-packages "alive-lsp/src/lsp/message/document/did-open" '(:alive/lsp/message/document/did-open))
(register-system-packages "alive-lsp/src/lsp/message/document/sem-tokens-full" '(:alive/lsp/message/document/sem-tokens-full))

(register-system-packages "alive-lsp/src/lsp/packet" '(:alive/lsp/packet))
(register-system-packages "alive-lsp/src/lsp/parse" '(:alive/lsp/parse))

(register-system-packages "alive-lsp/src/socket-pair" '(:alive/socket-pair))
(register-system-packages "alive-lsp/src/session" '(:alive/session))
(register-system-packages "alive-lsp/src/server" '(:alive/server))

(register-system-packages "alive-lsp/test/compat/sbcl/compile" '(:alive/test/compat/sbcl/compile))
(register-system-packages "alive-lsp/test/parse" '(:alive/test/parse))
(register-system-packages "alive-lsp/test/lsp/message" '(:alive/test/lsp/message))
