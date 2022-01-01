(defpackage :alive/test/parse
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:p :alive/parse/stream)
                      (:t :alive/parse/tokenizer)
                      (:analysis :alive/lsp/sem-analysis)))

(in-package :alive/test/parse)


(defun tokenizer ()
    (with-open-file (f "test/files/parse/foo.lisp")
        (t:from-stream f)))


(defun sem-tokens ()
    (with-open-file (f "test/files/parse/foo.lisp")
        (let ((tokens (t:from-stream f)))
            (analysis:to-sem-tokens tokens))))


(defun for-compile ()
    (with-open-file (f "test/files/parse/foo.lisp")
        (loop :for expr :in (p:from f) :do
                  (format T "EXPR: ~A~%" expr))))


(defun run-all ()
    (format T "Test Parse File~%")

    (sem-tokens))
