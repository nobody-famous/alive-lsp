(in-package :asdf-user)

(defsystem "alive-lsp"
    :class :package-inferred-system
    :depends-on (#:usocket
                 #:cl-json
                 #:bordeaux-threads
                 #:flexi-streams
                 #:sb-introspect

                 "alive-lsp/src/logger"

                 "alive-lsp/src/types"
                 "alive-lsp/src/errors"
                 "alive-lsp/src/compile-message"
                 "alive-lsp/src/position"
                 "alive-lsp/src/range"
                 "alive-lsp/src/text-edit"

                 "alive-lsp/src/parse/stream"
                 "alive-lsp/src/parse/token"
                 "alive-lsp/src/parse/tokenizer"
                 "alive-lsp/src/parse/form"
                 "alive-lsp/src/parse/forms"

                 "alive-lsp/src/compat/sbcl/file"
                 "alive-lsp/src/compat/sbcl/streams"
                 "alive-lsp/src/compat/sbcl/symbols"
                 "alive-lsp/src/compat/sbcl/threads"

                 "alive-lsp/src/file"
                 "alive-lsp/src/streams"
                 "alive-lsp/src/symbols"
                 "alive-lsp/src/format"
                 "alive-lsp/src/packages"
                 "alive-lsp/src/threads"
                 "alive-lsp/src/eval"
                 "alive-lsp/src/asdf"

                 "alive-lsp/src/lsp/errors"
                 "alive-lsp/src/lsp/utils"
                 "alive-lsp/src/lsp/completions"
                 "alive-lsp/src/lsp/hover"

                 "alive-lsp/src/lsp/types/config-item"
                 "alive-lsp/src/lsp/types/sem-tokens"
                 "alive-lsp/src/lsp/types/text-doc"
                 "alive-lsp/src/lsp/types/text-doc-item"
                 "alive-lsp/src/lsp/types/format-options"
                 "alive-lsp/src/lsp/types/debug-resp"
                 "alive-lsp/src/lsp/types/user-input"
                 "alive-lsp/src/lsp/types/restart-info"

                 "alive-lsp/src/lsp/sem-analysis"

                 "alive-lsp/src/lsp/message/abstract"
                 "alive-lsp/src/lsp/message/payload"
                 "alive-lsp/src/lsp/message/initialize"
                 "alive-lsp/src/lsp/message/alive/debugger"
                 "alive-lsp/src/lsp/message/alive/do-eval"
                 "alive-lsp/src/lsp/message/alive/get-pkg"
                 "alive-lsp/src/lsp/message/alive/remove-pkg"
                 "alive-lsp/src/lsp/message/alive/load-asdf"
                 "alive-lsp/src/lsp/message/alive/list-asdf"
                 "alive-lsp/src/lsp/message/alive/list-packages"
                 "alive-lsp/src/lsp/message/alive/list-threads"
                 "alive-lsp/src/lsp/message/alive/kill-thread"
                 "alive-lsp/src/lsp/message/alive/load-file"
                 "alive-lsp/src/lsp/message/alive/try-compile"
                 "alive-lsp/src/lsp/message/alive/unexport-symbol"
                 "alive-lsp/src/lsp/message/alive/stderr"
                 "alive-lsp/src/lsp/message/alive/stdout"
                 "alive-lsp/src/lsp/message/alive/top-form"
                 "alive-lsp/src/lsp/message/alive/user-input"
                 "alive-lsp/src/lsp/message/document/completion"
                 "alive-lsp/src/lsp/message/document/did-change"
                 "alive-lsp/src/lsp/message/document/did-open"
                 "alive-lsp/src/lsp/message/document/hover"
                 "alive-lsp/src/lsp/message/document/range-format"
                 "alive-lsp/src/lsp/message/document/sem-tokens-full"
                 "alive-lsp/src/lsp/message/workspace/config"

                 "alive-lsp/src/lsp/packet"
                 "alive-lsp/src/lsp/parse"

                 "alive-lsp/src/session"
                 "alive-lsp/src/server"))

(defsystem "alive-lsp/test"
    :depends-on ("alive-lsp"

                 "alive-lsp/test/harness/errors"
                 "alive-lsp/test/harness/check"
                 "alive-lsp/test/harness/formatting"
                 "alive-lsp/test/harness/run"

                 "alive-lsp/test/compat/sbcl/compile"
                 "alive-lsp/test/compat/sbcl/symbols"

                 "alive-lsp/test/utils"

                 "alive-lsp/test/parse/tokens"
                 "alive-lsp/test/parse/forms"

                 "alive-lsp/test/lsp/completions"
                 "alive-lsp/test/lsp/message"
                 "alive-lsp/test/lsp/sem-tokens"
                 "alive-lsp/test/lsp/hover"

                 "alive-lsp/test/session/state"
                 "alive-lsp/test/session/messages"

                 "alive-lsp/test/format/range"

                 "alive-lsp/test/eval"
                 "alive-lsp/test/streams"

                 "alive-lsp/test/suite"))

(register-system-packages "alive-lsp/src/logger" '(:alive/logger))

(register-system-packages "alive-lsp/src/types" '(:alive/types))
(register-system-packages "alive-lsp/src/errors" '(:alive/errors))
(register-system-packages "alive-lsp/src/compile-message" '(:alive/compile-message))
(register-system-packages "alive-lsp/src/position" '(:alive/position))
(register-system-packages "alive-lsp/src/range" '(:alive/range))
(register-system-packages "alive-lsp/src/text-edit" '(:alive/text-edit))

(register-system-packages "alive-lsp/src/parse/stream" '(:alive/parse/stream))
(register-system-packages "alive-lsp/src/parse/token" '(:alive/parse/token))
(register-system-packages "alive-lsp/src/parse/tokenizer" '(:alive/parse/tokenizer))
(register-system-packages "alive-lsp/src/parse/form" '(:alive/parse/form))
(register-system-packages "alive-lsp/src/parse/forms" '(:alive/parse/forms))

(register-system-packages "alive-lsp/src/file" '(:alive/file))
(register-system-packages "alive-lsp/src/streams" '(:alive/streams))
(register-system-packages "alive-lsp/src/symbols" '(:alive/symbols))
(register-system-packages "alive-lsp/src/format" '(:alive/format))
(register-system-packages "alive-lsp/src/threads" '(:alive/threads))
(register-system-packages "alive-lsp/src/packages" '(:alive/packages))
(register-system-packages "alive-lsp/src/eval" '(:alive/eval))
(register-system-packages "alive-lsp/src/asdf" '(:alive/asdf))

(register-system-packages "alive-lsp/src/compat/sbcl/file" '(:alive/sbcl/file))
(register-system-packages "alive-lsp/src/compat/sbcl/streams" '(:alive/sbcl/streams))
(register-system-packages "alive-lsp/src/compat/sbcl/symbols" '(:alive/sbcl/symbols))
(register-system-packages "alive-lsp/src/compat/sbcl/threads" '(:alive/sbcl/threads))

(register-system-packages "alive-lsp/src/lsp/errors" '(:alive/lsp/errors))
(register-system-packages "alive-lsp/src/lsp/utils" '(:alive/lsp/utils))
(register-system-packages "alive-lsp/src/lsp/completions" '(:alive/lsp/completions))
(register-system-packages "alive-lsp/src/lsp/hover" '(:alive/lsp/hover))

(register-system-packages "alive-lsp/src/lsp/types/config-item" '(:alive/lsp/types/config-item))
(register-system-packages "alive-lsp/src/lsp/types/sem-tokens" '(:alive/lsp/types/sem-tokens))
(register-system-packages "alive-lsp/src/lsp/types/text-doc" '(:alive/lsp/types/text-doc))
(register-system-packages "alive-lsp/src/lsp/types/text-doc-item" '(:alive/lsp/types/text-doc-item))
(register-system-packages "alive-lsp/src/lsp/types/format-options" '(:alive/lsp/types/format-options))
(register-system-packages "alive-lsp/src/lsp/types/debug-resp" '(:alive/lsp/types/debug-resp))
(register-system-packages "alive-lsp/src/lsp/types/user-input" '(:alive/lsp/types/user-input))
(register-system-packages "alive-lsp/src/lsp/types/restart-info" '(:alive/lsp/types/restart-info))

(register-system-packages "alive-lsp/src/lsp/sem-analysis" '(:alive/lsp/sem-analysis))

(register-system-packages "alive-lsp/src/lsp/message/abstract" '(:alive/lsp/message/abstract))
(register-system-packages "alive-lsp/src/lsp/message/payload" '(:alive/lsp/message/payload))
(register-system-packages "alive-lsp/src/lsp/message/initialize" '(:alive/lsp/message/initialize))
(register-system-packages "alive-lsp/src/lsp/message/alive/debugger" '(:alive/lsp/message/alive/debugger))
(register-system-packages "alive-lsp/src/lsp/message/alive/do-eval" '(:alive/lsp/message/alive/do-eval))
(register-system-packages "alive-lsp/src/lsp/message/alive/get-pkg" '(:alive/lsp/message/alive/get-pkg))
(register-system-packages "alive-lsp/src/lsp/message/alive/remove-pkg" '(:alive/lsp/message/alive/remove-pkg))
(register-system-packages "alive-lsp/src/lsp/message/alive/load-asdf" '(:alive/lsp/message/alive/load-asdf))
(register-system-packages "alive-lsp/src/lsp/message/alive/list-asdf" '(:alive/lsp/message/alive/list-asdf))
(register-system-packages "alive-lsp/src/lsp/message/alive/list-packages" '(:alive/lsp/message/alive/list-packages))
(register-system-packages "alive-lsp/src/lsp/message/alive/list-threads" '(:alive/lsp/message/alive/list-threads))
(register-system-packages "alive-lsp/src/lsp/message/alive/kill-thread" '(:alive/lsp/message/alive/kill-thread))
(register-system-packages "alive-lsp/src/lsp/message/alive/load-file" '(:alive/lsp/message/alive/load-file))
(register-system-packages "alive-lsp/src/lsp/message/alive/try-compile" '(:alive/lsp/message/alive/try-compile))
(register-system-packages "alive-lsp/src/lsp/message/alive/unexport-symbol" '(:alive/lsp/message/alive/unexport-symbol))
(register-system-packages "alive-lsp/src/lsp/message/alive/stderr" '(:alive/lsp/message/alive/stderr))
(register-system-packages "alive-lsp/src/lsp/message/alive/stdout" '(:alive/lsp/message/alive/stdout))
(register-system-packages "alive-lsp/src/lsp/message/alive/top-form" '(:alive/lsp/message/alive/top-form))
(register-system-packages "alive-lsp/src/lsp/message/alive/user-input" '(:alive/lsp/message/alive/user-input))
(register-system-packages "alive-lsp/src/lsp/message/document/completion" '(:alive/lsp/message/document/completion))
(register-system-packages "alive-lsp/src/lsp/message/document/did-change" '(:alive/lsp/message/document/did-change))
(register-system-packages "alive-lsp/src/lsp/message/document/did-open" '(:alive/lsp/message/document/did-open))
(register-system-packages "alive-lsp/src/lsp/message/document/hover" '(:alive/lsp/message/document/hover))
(register-system-packages "alive-lsp/src/lsp/message/document/range-format" '(:alive/lsp/message/document/range-format))
(register-system-packages "alive-lsp/src/lsp/message/document/sem-tokens-full" '(:alive/lsp/message/document/sem-tokens-full))
(register-system-packages "alive-lsp/src/lsp/message/workspace/config" '(:alive/lsp/message/workspace/config))

(register-system-packages "alive-lsp/src/lsp/packet" '(:alive/lsp/packet))
(register-system-packages "alive-lsp/src/lsp/parse" '(:alive/lsp/parse))

(register-system-packages "alive-lsp/src/session" '(:alive/session))
(register-system-packages "alive-lsp/src/server" '(:alive/server))

(register-system-packages "alive-lsp/test/harness/errors" '(:alive/test/harness/errors))
(register-system-packages "alive-lsp/test/harness/check" '(:alive/test/harness/check))
(register-system-packages "alive-lsp/test/harness/formatting" '(:alive/test/harness/formatting))
(register-system-packages "alive-lsp/test/harness/run" '(:alive/test/harness/run))

(register-system-packages "alive-lsp/test/suite" '(:alive/test/suite))

(register-system-packages "alive-lsp/test/compat/sbcl/compile" '(:alive/test/compat/sbcl/compile))
(register-system-packages "alive-lsp/test/compat/sbcl/symbols" '(:alive/test/compat/sbcl/symbols))

(register-system-packages "alive-lsp/test/utils" '(:alive/test/utils))

(register-system-packages "alive-lsp/test/parse/tokens" '(:alive/test/parse/tokens))
(register-system-packages "alive-lsp/test/parse/forms" '(:alive/test/parse/forms))

(register-system-packages "alive-lsp/test/lsp/completions" '(:alive/test/lsp/completions))
(register-system-packages "alive-lsp/test/lsp/message" '(:alive/test/lsp/message))
(register-system-packages "alive-lsp/test/lsp/sem-tokens" '(:alive/test/lsp/sem-tokens))
(register-system-packages "alive-lsp/test/lsp/hover" '(:alive/test/lsp/hover))

(register-system-packages "alive-lsp/test/session/state" '(:alive/test/session/state))
(register-system-packages "alive-lsp/test/session/messages" '(:alive/test/session/messages))

(register-system-packages "alive-lsp/test/format/range" '(:alive/test/format/range))

(register-system-packages "alive-lsp/test/eval" '(:alive/test/eval))
(register-system-packages "alive-lsp/test/streams" '(:alive/test/streams))