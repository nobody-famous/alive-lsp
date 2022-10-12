(defpackage :alive/test/forms
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:pos :alive/position)))

(in-package :alive/test/forms)


(defun get-child-form (text pos)
    (let* ((forms (forms:from-stream (make-string-input-stream text)))
           (top-form (forms:get-top-form forms pos)))
        (forms:get-child-form top-form pos)))


(defun test-child-form ()
    (clue:test "Child Form"
        (let* ((text (format NIL "(if (a b) (c d))"))
               (all (get-child-form text (pos:create 0 1)))
               (child-1 (get-child-form text (pos:create 0 5)))
               (child-2 (get-child-form text (pos:create 0 12))))
            (format T "ALL ~A~%" all)
            (format T "CHILD-1 ~A~%" child-1)
            (format T "CHILD-2 ~A~%" child-2))))


(defun run-all ()
    (clue:suite "Forms Tests"
        (test-child-form)))
