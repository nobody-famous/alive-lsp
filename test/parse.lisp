(defpackage :alive/test/parse
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:p :alive/parse/stream))
)

(in-package :alive/test/parse)


(defun run-all ()
    (format T "Test Parse File~%")

    (with-open-file (f "test/files/parse/foo.lisp")
        (loop :for expr :in (p:from f) :do
                  (format T "EXPR: ~A~%" expr)
        )))
