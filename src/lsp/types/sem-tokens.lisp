(defpackage :alive/lsp/types/sem-tokens
    (:use :cl)
    (:export :*mods*
             :*types*)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/sem-tokens)


(defparameter *type-comment* 0)
(defparameter *type-string* 1)
(defparameter *type-keyword* 2)
(defparameter *type-number* 3)
(defparameter *type-namespace* 4)
(defparameter *type-function* 5)
(defparameter *type-macro* 6)
(defparameter *type-variable* 7)
(defparameter *type-parameter* 8)
(defparameter *type-parenthesis* 9)
(defparameter *type-symbol* 10)


; The index of the types in the list needs to match their value
(defparameter *types* (list *type-comment*
                            *type-string*
                            *type-keyword*
                            *type-number*
                            *type-namespace*
                            *type-function*
                            *type-macro*
                            *type-variable*
                            *type-parameter*
                            *type-parenthesis*
                            *type-symbol*))


(defparameter *mods* (list))


(defun from-type (value)
    (cond ((eq value types:*comment*) *type-comment*)
          ((eq value types:*string*) *type-string*)
          ((eq value types:*keyword*) *type-keyword*)
          ((eq value types:*number*) *type-number*)
          ((eq value types:*namespace*) *type-namespace*)
          ((eq value types:*function*) *type-function*)
          ((eq value types:*macro*) *type-macro*)
          ((eq value types:*variable*) *type-variable*)
          ((eq value types:*parameter*) *type-parameter*)
          ((eq value types:*open-paren*) *type-parenthesis*)
          ((eq value types:*close-paren*) *type-parenthesis*)
          ((eq value types:*symbol*) *type-symbol*)
          (t (error (format nil "Unhandled token type ~A~%" value)))))
