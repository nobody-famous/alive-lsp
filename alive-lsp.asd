(in-package :asdf-user)

(defsystem "alive-lsp"
    :class :package-inferred-system
    :depends-on (#:usocket
                 #:bordeaux-threads
                 "alive-lsp/src/parse/stream"
                 "alive-lsp/src/compat/sbcl/compile"
                 "alive-lsp/src/compat/sbcl/streams"
                ))

(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"

                 "alive-lsp/test/compat/sbcl/compile"
                 "alive-lsp/test/parse"
                ))

(register-system-packages "alive-lsp/src/parse/stream" '(:alive/parse/stream))
(register-system-packages "alive-lsp/src/compat/sbcl/compile" '(:alive/compile))
(register-system-packages "alive-lsp/src/compat/sbcl/streams" '(:alive/streams))

(register-system-packages "alive-lsp/test/compat/sbcl/compile" '(:alive/test/compat/sbcl/compile))
(register-system-packages "alive-lsp/test/parse" '(:alive/test/parse))
