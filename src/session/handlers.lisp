(defpackage :alive/session/handlers
    (:use :cl)
    (:export :get-handler
             :with-handlers))

(in-package :alive/session/handlers)


(declaim (ftype (function (T) boolean) handler-item-p))
(defun handler-item-p (data)
    (and (consp data)
         (typep (car data) 'string)
         (typep (cdr data) 'function)))


(declaim (ftype (function (T) boolean) list-of-handlers-p))
(defun list-of-handlers-p (data)
    (and (consp data)
         (every #'handler-item-p data)))


(deftype handler-item ()
    `(satisfies handler-item-p))


(deftype list-of-handlers ()
    `(satisfies list-of-handlers-p))


(declaim (type (or null list-of-handlers) *handlers*))
(defparameter *handlers* nil)


#+n (defparameter *default-handlers* (list (cons "initialize" 'handle-init)
                                           (cons "initialized" 'handle-initialized)

                                           (cons "textDocument/completion" 'handle-completion)
                                           (cons "textDocument/definition" 'handle-definition)
                                           (cons "textDocument/didChange" 'handle-did-change)
                                           (cons "textDocument/didClose" 'handle-did-change)
                                           (cons "textDocument/didOpen" 'handle-did-open)
                                           (cons "textDocument/didSave" 'ignore-msg)
                                           (cons "textDocument/documentSymbol" 'handle-doc-symbols)
                                           (cons "textDocument/hover" 'handle-hover)
                                           (cons "textDocument/onTypeFormatting" 'handle-on-type)
                                           (cons "textDocument/rangeFormatting" 'handle-formatting)
                                           (cons "textDocument/selectionRange" 'handle-selection)
                                           (cons "textDocument/semanticTokens/full" 'handle-sem-tokens)

                                           (cons "$/setTrace" 'ignore-msg)
                                           (cons "$/cancelRequest" 'ignore-msg)

                                           (cons "$/alive/eval" 'handle-eval)
                                           (cons "$/alive/getPackageForPosition" 'handle-get-pkg)
                                           (cons "$/alive/inspect" 'handle-inspect)
                                           (cons "$/alive/inspectClose" 'handle-inspect-close)
                                           (cons "$/alive/inspectEval" 'handle-inspect-eval)
                                           (cons "$/alive/inspectMacro" 'handle-inspect-macro)
                                           (cons "$/alive/inspectRefresh" 'handle-inspect-refresh)
                                           (cons "$/alive/inspectSymbol" 'handle-inspect-sym)
                                           (cons "$/alive/killThread" 'handle-kill-thread)
                                           (cons "$/alive/listAsdfSystems" 'handle-list-asdf)
                                           (cons "$/alive/listPackages" 'handle-list-pkgs)
                                           (cons "$/alive/listThreads" 'handle-list-threads)
                                           (cons "$/alive/loadFile" 'handle-load-file)
                                           (cons "$/alive/loadAsdfSystem" 'handle-load-asdf)
                                           (cons "$/alive/macroexpand" 'handle-macroexpand)
                                           (cons "$/alive/macroexpand1" 'handle-macroexpand-1)
                                           (cons "$/alive/removePackage" 'handle-remove-pkg)
                                           (cons "$/alive/symbol" 'handle-symbol)
                                           (cons "$/alive/surroundingFormBounds" 'handle-surrounding-form)
                                           (cons "$/alive/topFormBounds" 'handle-top-form)
                                           (cons "$/alive/compile" 'handle-compile)
                                           (cons "$/alive/tryCompile" 'handle-try-compile)
                                           (cons "$/alive/unexportSymbol" 'handle-unexport)))


(declaim (ftype (function (string) (or null (function (cons) (or null hash-table)))) get-handler))
(defun get-handler (name)
    (cdr (assoc name *handlers* :test #'string=)))


(defmacro with-handlers (handlers &body body)
    `(let ((*handlers* ,handlers))
         ,@body))
