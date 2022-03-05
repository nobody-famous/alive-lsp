(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/lsp/completions)


(defun run-all ()
    (run:suite "Completion Tests"
               (lambda ()
                   nil)))
