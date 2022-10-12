(defpackage :alive/test/macros
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:macros :alive/macros)))

(in-package :alive/test/macros)


(defun basic ()
    (clue:test "Basic Macro Expand"
        (let ((expanded (macros:expand "(loop :for i :below 10 :collect i)")))
            (format T "Checking macro expand ~A~%" expanded))))


(defun run-all ()
    (format T "run-all called~%"))
