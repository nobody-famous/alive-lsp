(defpackage :alive/session/handler/init
    (:use :cl)
    (:export :initialized
             :request)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/init)


(defparameter *doc-sync-none* 0)
(defparameter *doc-sync-full* 1)
(defparameter *doc-sync-incr* 2)


(declaim (ftype (function (cons) hash-table) request))
(defun request (msg)
    (let* ((id (cdr (assoc :id msg)))
           (data (make-hash-table :test #'equalp))
           (caps (make-hash-table :test #'equalp))
           (sem-opts (make-hash-table :test #'equalp))
           (legend-opts (make-hash-table :test #'equalp))
           (comp-opts (make-hash-table :test #'equalp))
           (on-type-opts (make-hash-table :test #'equalp))
           (sig-help-opts (make-hash-table :test #'equalp)))
        (declare (type fixnum id))

        (setf (gethash "triggerCharacters" comp-opts) (list #\: #\+ #\- #\*))

        (setf (gethash "tokenTypes" legend-opts) sem-tokens:*types*)
        (setf (gethash "tokenModifiers" legend-opts) sem-tokens:*mods*)

        (setf (gethash "legend" sem-opts) legend-opts)
        (setf (gethash "full" sem-opts) T)

        (setf (gethash "firstTriggerCharacter" on-type-opts) #\newline)
        (setf (gethash "moreTriggerCharacters" on-type-opts) (list))

        (setf (gethash "triggerCharacters" sig-help-opts) (list #\space))
        (setf (gethash "signatureHelpProvider" caps) sig-help-opts)

        (setf (gethash "textDocumentSync" caps) *doc-sync-full*)
        (setf (gethash "hoverProvider" caps) nil)
        (setf (gethash "semanticTokensProvider" caps) sem-opts)
        (setf (gethash "completionProvider" caps) comp-opts)
        (setf (gethash "documentRangeFormattingProvider" caps) T)
        (setf (gethash "documentOnTypeFormattingProvider" caps) on-type-opts)
        (setf (gethash "selectionRangeProvider" caps) T)
        (setf (gethash "definitionProvider" caps) T)
        (setf (gethash "documentSymbolProvider" caps) T)
        (setf (gethash "referencesProvider" caps) T)

        (setf (gethash "capabilities" data) caps)

        (lsp-msg:create-response id :result-value data)))


(declaim (ftype (function (state:state cons) null) initialized))
(defun initialized (state msg)
    (declare (ignore msg))
    (state:set-initialized state T)
    nil)
