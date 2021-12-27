(defpackage :alive/lsp/types/sem-tokens
    (:use :cl)
    (:export :*mods*
             :*types*)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/sem-tokens)


(defparameter *types* (list types:*token-type-comment*
                            types:*token-type-string*
                            types:*token-type-keyword*
                            types:*token-type-number*
                            types:*token-type-regexp*
                            types:*token-type-operator*
                            types:*token-type-namespace*
                            types:*token-type-type*
                            types:*token-type-struct*
                            types:*token-type-class*
                            types:*token-type-interface*
                            types:*token-type-enum*
                            types:*token-type-typeParameter*
                            types:*token-type-function*
                            types:*token-type-member*
                            types:*token-type-macro*
                            types:*token-type-variable*
                            types:*token-type-parameter*
                            types:*token-type-property*
                            types:*token-type-label*
                            types:*token-type-parenthesis*
                            types:*token-type-symbol*))


(defparameter *mods* (list types:*token-mod-declaration*
                           types:*token-mod-documentation*
                           types:*token-mod-readonly*
                           types:*token-mod-static*
                           types:*token-mod-abstract*
                           types:*token-mod-deprecated*
                           types:*token-mod-modification*
                           types:*token-mod-async*))
