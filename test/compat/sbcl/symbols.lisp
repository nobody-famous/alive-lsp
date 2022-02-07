(defpackage :alive/test/compat/sbcl/symbols
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:asymbols :alive/symbols)))

(in-package :alive/test/compat/sbcl/symbols)


(defun test-lookup ()
    (format T "Lookup ~A~%" (asymbols:callable-p "callable-p" "alive/symbols")))


(defun run-all ()
    (format T "SBCL Symbol Tests~%")

    (test-lookup))
