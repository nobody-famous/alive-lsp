(defpackage :alive/lsp/types/sem-tokens
    (:use :cl)
    (:export :*mods*
             :*types*))

(in-package :alive/lsp/types/sem-tokens)


(defparameter *types* (list "comment"
                            "string"
                            "keyword"
                            "number"
                            "regexp"
                            "operator"
                            "namespace"
                            "type"
                            "struct"
                            "class"
                            "interface"
                            "enum"
                            "typeParameter"
                            "function"
                            "member"
                            "macro"
                            "variable"
                            "parameter"
                            "property"
                            "label"
                            "parenthesis"
                            "symbol"))


(defparameter *mods* (list "declaration"
                           "documentation"
                           "readonly"
                           "static"
                           "abstract"
                           "deprecated"
                           "modification"
                           "async"))
