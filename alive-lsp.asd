(in-package :asdf-user)

(defsystem "alive-lsp"
    :class :package-inferred-system
    :depends-on (#:usocket
                 #:bordeaux-threads
                 "alive-lsp/src/types"
                 "alive-lsp/src/parse/stream"
                 "alive-lsp/src/compat/sbcl/compile"
                 "alive-lsp/src/compat/sbcl/streams"
                 "alive-lsp/src/compile"

                 "alive-lsp/src/lsp/message"

                 "alive-lsp/src/server"
                ))

(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"

                 "alive-lsp/test/compat/sbcl/compile"
                 "alive-lsp/test/parse"
                ))

(register-system-packages "alive-lsp/src/types" '(:alive/types))
(register-system-packages "alive-lsp/src/parse/stream" '(:alive/parse/stream))
(register-system-packages "alive-lsp/src/compile" '(:alive/compile))
(register-system-packages "alive-lsp/src/compat/sbcl/compile" '(:alive/compile/compat))
(register-system-packages "alive-lsp/src/compat/sbcl/streams" '(:alive/streams))

(register-system-packages "alive-lsp/src/lsp/message" '(:alive/lsp/message))

(register-system-packages "alive-lsp/src/server" '(:alive/server))

(register-system-packages "alive-lsp/test/compat/sbcl/compile" '(:alive/test/compat/sbcl/compile))
(register-system-packages "alive-lsp/test/parse" '(:alive/test/parse))
