(defpackage :alive/lsp/message/response
    (:use :cl)
    (:export :completion
             :definition
             :do-eval
             :do-inspect
             :doc-symbols
             :get-pkg
             :get-symbol
             :hover
             :initialize
             :list-items
             :load-file
             :macro
             :selection-range
             :sem-tokens
             :top-form
             :try-compile)
    (:local-nicknames (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:sem-types :alive/lsp/types/sem-tokens)
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
        (setf (gethash "selectionRangeProvider" caps) T)
        (setf (gethash "definitionProvider" caps) T)
        (setf (gethash "documentSymbolProvider" caps) T)

        (setf (gethash "capabilities" data) caps)

        (message:create-response id :result-value data)))


(defun result (id key value)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash key data) value)
        (message:create-response id :result-value data)))


(defun completion (id &key items)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "isIncomplete" data) T)
        (setf (gethash "items" data) items)

        (message:create-response id :result-value data)))


(defun hover (id &key value)
    (result id "value" value))


(defun definition (id &key uri range)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "uri" data) uri)
        (setf (gethash "range" data) range)

        (message:create-response id :result-value data)))


(defun do-eval (id text)
    (result id "text" text))


(defun macro (id text)
    (result id "text" text))


(defun do-inspect (id &key insp-id result result-type)
    (let ((data (make-hash-table :test #'equalp))
          (expr-type (if result-type result-type "expr")))

        (setf (gethash "id" data) insp-id)
        (setf (gethash "resultType" data) expr-type)
        (setf (gethash "result" data) result)

        (message:create-response id :result-value data)))


(defun get-pkg (id &key pkg-name)
    (result id "package" pkg-name))


(defun list-items (id name items)
    (result id name items))


(defun load-file (id msgs)
    (result id "messages" msgs))


(defun get-symbol (id &key value)
    (result id "value" value))


(defun try-compile (id msgs)
    (result id "messages" msgs))


(defun top-form (id &key start end)
    (let ((data (make-hash-table :test #'equalp)))

        (setf (gethash "start" data) start)
        (setf (gethash "end" data) end)

        (message:create-response id
                                 :result-value data)))


(defun to-sem-array (sem-tokens)
    (loop :with line := 0
          :with col := 0
          :with out-list := nil

          :for token :in sem-tokens
          :for len := (- (sem-types:end-col token) (sem-types:start-col token))
          :for line-diff := (- (sem-types:line token) line)
          :for col-diff := (if (zerop line-diff)
                               (- (sem-types:start-col token) col)
                               (sem-types:start-col token)) :do

              (push line-diff out-list)
              (push col-diff out-list)
              (push len out-list)
              (push (sem-types:token-type token) out-list)
              (push 0 out-list)

              (setf line (sem-types:line token))
              (setf col (sem-types:start-col token))
          :finally (return (reverse out-list))))


(defun sem-tokens (id sem-tokens)
    (result id "data" (to-sem-array sem-tokens)))


(defun create-selection-range (range parent)
    (let ((sel-range (make-hash-table :test #'equalp)))

        (setf (gethash "range" sel-range) range)
        (setf (gethash "parent" sel-range) parent)

        sel-range))


(defun to-nested-ranges (ranges)
    (loop :with cur-item := (create-selection-range (car ranges) nil)

          :for range :in (cdr ranges)
          :do (setf cur-item (create-selection-range range cur-item))

          :finally (progn (setf (gethash "range" cur-item) range)
                          (return cur-item))))


(defun selection-range (id ranges)
    (let ((value (or (mapcar #'to-nested-ranges ranges)
                     (make-hash-table :test #'equalp))))
        (message:create-response id :result-value value)))


(defun doc-symbols (id symbols)
    (let ((result (if symbols symbols (make-hash-table))))
        (message:create-response id
                                 :result-value result)))
