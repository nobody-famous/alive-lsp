(defpackage :alive/test/suite
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:fmt :alive/test/harness/formatting)))

(in-package :alive/test/suite)


(defun run-all ()
    (fmt:print-header "Run all Alive LSP tests")

    (alive/test/parse/tokens:run-all))
