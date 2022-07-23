(defpackage :alive/lsp/message/initialize
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:message :alive/lsp/message/abstract)))

(in-package :alive/lsp/message/initialize)


(defparameter *doc-sync-none* 0)
(defparameter *doc-sync-full* 1)
(defparameter *doc-sync-incr* 2)


(defun create-response (id)
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
