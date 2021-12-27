(defpackage :alive/test/parse
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:p :alive/parse/stream)
                      (:t :alive/parse/tokenizer)))

(in-package :alive/test/parse)


(defun tokenizer ()
    (with-open-file (f "test/files/parse/foo.lisp")
        (t:from-stream f)))


(defun for-compile ()
    (with-open-file (f "test/files/parse/foo.lisp")
        (loop :for expr :in (p:from f) :do
                  (format T "EXPR: ~A~%" expr))))


(defun run-all ()
    (format T "Test Parse File~%")

    (tokenizer))
