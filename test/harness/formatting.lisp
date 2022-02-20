(defpackage :alive/test/harness/formatting
    (:use :cl)
    (:export :print-header))

(in-package :alive/test/harness/formatting)


(defun print-header (header)
    (let ((size (length header)))
        (format T "~%~A~%~v@{~A~:*~}~%"
                header
                size "-")))
