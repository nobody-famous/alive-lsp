(defpackage :alive/lsp/types/sem-tokens
    (:use :cl)
    (:export :create
             :token
             :token-type
             :line
             :start-col
             :end-col

             :*mods*
             :*types*

             :*comment*
             :*string*
             :*keyword*
             :*number*
             :*namespace*
             :*function*
             :*macro*
             :*variable*
             :*parameter*
             :*parenthesis*
             :*symbol*)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/sem-tokens)


(defparameter *comment* 0)
(defparameter *string* 1)
(defparameter *keyword* 2)
(defparameter *number* 3)
(defparameter *namespace* 4)
(defparameter *function* 5)
(defparameter *macro* 6)
(defparameter *variable* 7)
(defparameter *parameter* 8)
(defparameter *parenthesis* 9)
(defparameter *symbol* 10)

; The index of the types in the list needs to match their value
(defparameter *types* (list "comment"
                            "string"
                            "keyword"
                            "number"
                            "namespace"
                            "function"
                            "macro"
                            "variable"
                            "parameter"
                            "parenthesis"
                            "symbol"))


(defparameter *mods* (list))


(defun end-col (token)
    (when token
          (gethash "endCol" token)))


(defun start-col (token)
    (when token
          (gethash "startCol" token)))


(defun line (token)
    (when token
          (gethash "line" token)))


(defun token-type (token)
    (when token
          (gethash "tokenType" token)))


(defun create (&key token-type line start end)
    (let ((token (make-hash-table :test #'equalp)))

        (setf (gethash "tokenType" token) token-type)
        (setf (gethash "line" token) line)
        (setf (gethash "startCol" token) start)
        (setf (gethash "endCol" token) end)

        token))
