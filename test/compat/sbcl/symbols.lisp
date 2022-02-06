(defpackage :alive/test/compat/sbcl/symbols
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:astreams :alive/streams)
                      (:compile :alive/compile)))

(in-package :alive/test/compat/sbcl/symbols)


(defun run-all ()
    (format T "SBCL Symbol Tests~%"))
