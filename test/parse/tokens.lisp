(defpackage :alive/test/parse/tokens
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:fmt :alive/test/harness/formatting)))

(in-package :alive/test/parse/tokens)


(defun atoms ()
    (format T "Test atoms~%"))


(defun run-all ()
    (fmt:print-header "Test parse tokens")

    (atoms))
