(defpackage :alive/test/suite
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:run :alive/test/harness/run)))

(in-package :alive/test/suite)


(defun run-all ()
    (run:suite "Run all Alive LSP tests"
                   (lambda ()
                       (alive/test/parse/tokens:run-all))))
