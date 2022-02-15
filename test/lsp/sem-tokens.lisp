(defpackage :alive/test/lsp/sem-tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:tokenizer :alive/parse/tokenizer)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/lsp/sem-tokens)


(defun get-sem-tokens (text)
    (analysis:to-sem-tokens
     (tokenizer:from-stream
      (make-string-input-stream text))))


(defun symbols ()
    (run:test "Symbols"
              (lambda ()
                  (let ((tokens (get-sem-tokens "foo")))
                      (format T "TOKENS ~A~%" tokens)))))


(defun run-all ()
    (run:suite "Semantic Tokens"
               (lambda ()
                   (symbols))))
