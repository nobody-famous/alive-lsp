(defpackage :alive/test/files
    (:use :cl))

(in-package :alive/test/files)


(defun divide-with-restarts (x y)
    (restart-case (/ x y)
        (return-zero ()
                     :report "Return 0"
                     0)
        (divide-by-one ()
                       :report "Divide by 1"
                       (/ x 1))
        (set-new-divisor (value)
                         :report "Enter a new divisor"
                         ;;
                         ;; Ask the user for a new value:
                         :interactive (lambda () (prompt-new-value "Please enter a new divisor: "))
                         ;;
                         ;; and call the divide function with the new value…
                         ;; … possibly catching bad input again!
                         (divide-with-restarts x value))))

(defun prompt-new-value (prompt)
    (format *query-io* prompt) ;; *query-io*: the special stream to make user queries.
    (force-output *query-io*) ;; Ensure the user sees what he types.
    (list (read *query-io*))) ;; We must return a list.

(divide-with-restarts 3 0)
