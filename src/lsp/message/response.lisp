(defpackage :alive/lsp/message/response
    (:use :cl)
    (:export :completion
             :do-eval
             :format-edits
             :hover
             :initialize)
    (:local-nicknames (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:fmt-utils :alive/lsp/message/format-utils)
                      (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/response)


(defparameter *doc-sync-none* 0)
(defparameter *doc-sync-full* 1)
(defparameter *doc-sync-incr* 2)


(defun initialize (id)
    (let* ((data (make-hash-table :test #'equalp))
           (caps (make-hash-table :test #'equalp))
           (sem-opts (make-hash-table :test #'equalp))
           (legend-opts (make-hash-table :test #'equalp))
           (comp-opts (make-hash-table :test #'equalp))
           (on-type-opts (make-hash-table :test #'equalp)))

        (setf (gethash "triggerCharacters" comp-opts) (list #\:))

        (setf (gethash "tokenTypes" legend-opts) sem-tokens:*types*)
        (setf (gethash "tokenModifiers" legend-opts) sem-tokens:*mods*)

        (setf (gethash "legend" sem-opts) legend-opts)
        (setf (gethash "full" sem-opts) T)

        (setf (gethash "firstTriggerCharacter" on-type-opts) #\newline)
        (setf (gethash "moreTriggerCharacters" on-type-opts) (list))

        (setf (gethash "textDocumentSync" caps) *doc-sync-full*)
        (setf (gethash "hoverProvider" caps) nil)
        (setf (gethash "semanticTokensProvider" caps) sem-opts)
        (setf (gethash "completionProvider" caps) comp-opts)
        (setf (gethash "documentRangeFormattingProvider" caps) T)
        (setf (gethash "documentOnTypeFormattingProvider" caps) on-type-opts)

        (setf (gethash "capabilities" data) caps)

        (message:create-response id :result-value data)))


(defun completion (id &key items)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "items" data) items)
        (message:create-response id :result-value data)))


(defun hover (id &key value)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "value" data) value)
        (message:create-response id
                                 :result-value data)))


(defun format-edits (id edits)
    (message:create-response id
                             :result-value (fmt-utils:to-text-edits edits)))


(defun do-eval (id text)
    (let ((data (make-hash-table)))
        (setf (gethash "text" data) text)
        (message:create-response id
                                 :result-value data)))
