(defpackage :alive/lsp/types/sem-tokens
    (:use :cl)
    (:export :token
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


(defclass token ()
    ((token-type :accessor token-type
                 :initform nil
                 :initarg :token-type)
     (line :accessor line
           :initform nil
           :initarg :line)
     (start-col :accessor start-col
                :initform nil
                :initarg :start-col)
     (end-col :accessor end-col
              :initform nil
              :initarg :end-col)))


(defmethod print-object ((obj token) out)
    (format out "{~A line ~A start ~A end ~A}"
            (token-type obj)
            (line obj)
            (start-col obj)
            (end-col obj)))


(defun from-type (value)
    (cond ((eq value types:*comment*) *comment*)
          ((eq value types:*string*) *string*)
          ((eq value types:*keyword*) *keyword*)
          ((eq value types:*number*) *number*)
          ((eq value types:*namespace*) *namespace*)
          ((eq value types:*function*) *function*)
          ((eq value types:*macro*) *macro*)
          ((eq value types:*variable*) *variable*)
          ((eq value types:*parameter*) *parameter*)
          ((eq value types:*open-paren*) *parenthesis*)
          ((eq value types:*close-paren*) *parenthesis*)
          ((eq value types:*symbol*) *symbol*)
          (t (error (format nil "Unhandled token type ~A~%" value)))))
