(defpackage :alive/types
    (:use :cl)
    (:export :make-compile-message
             :*sev-error*
             :*sev-warn*
             :*sev-info*

             :*token-type-comment*
             :*token-type-string*
             :*token-type-keyword*
             :*token-type-number*
             :*token-type-regexp*
             :*token-type-operator*
             :*token-type-namespace*
             :*token-type-type*
             :*token-type-struct*
             :*token-type-class*
             :*token-type-interface*
             :*token-type-enum*
             :*token-type-typeParameter*
             :*token-type-function*
             :*token-type-member*
             :*token-type-macro*
             :*token-type-variable*
             :*token-type-parameter*
             :*token-type-property*
             :*token-type-label*
             :*token-type-open-paren*
             :*token-type-close-paren*
             :*token-type-parenthesis*
             :*token-type-symbol*

             :*token-mod-declaration*
             :*token-mod-documentation*
             :*token-mod-readonly*
             :*token-mod-static*
             :*token-mod-abstract*
             :*token-mod-deprecated*
             :*token-mod-modification*
             :*token-mod-async*))

(in-package :alive/types)


(defparameter *sev-error* "error")
(defparameter *sev-warn* "warning")
(defparameter *sev-info* "info")


(defparameter *token-type-comment* "comment")
(defparameter *token-type-string* "string")
(defparameter *token-type-keyword* "keyword")
(defparameter *token-type-number* "number")
(defparameter *token-type-regexp* "regexp")
(defparameter *token-type-operator* "operator")
(defparameter *token-type-namespace* "namespace")
(defparameter *token-type-type* "type")
(defparameter *token-type-struct* "struct")
(defparameter *token-type-class* "class")
(defparameter *token-type-interface* "interface")
(defparameter *token-type-enum* "enum")
(defparameter *token-type-typeParameter* "typeParameter")
(defparameter *token-type-function* "function")
(defparameter *token-type-member* "member")
(defparameter *token-type-macro* "macro")
(defparameter *token-type-variable* "variable")
(defparameter *token-type-parameter* "parameter")
(defparameter *token-type-property* "property")
(defparameter *token-type-label* "label")
(defparameter *token-type-open-paren* "open-paren")
(defparameter *token-type-close-paren* "close-paren")
(defparameter *token-type-parenthesis* "parenthesis")
(defparameter *token-type-symbol* "symbol")


(defparameter *token-mod-declaration* "declaration")
(defparameter *token-mod-documentation* "documentation")
(defparameter *token-mod-readonly* "readonly")
(defparameter *token-mod-static* "static")
(defparameter *token-mod-abstract* "abstract")
(defparameter *token-mod-deprecated* "deprecated")
(defparameter *token-mod-modification* "modification")
(defparameter *token-mod-async* "async")


(defstruct compile-message
    severity
    location
    message)
