(defpackage :alive/test/forms
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:pos :alive/position)))

(in-package :alive/test/forms)


(defun get-child-form (text pos)
    (let* ((forms (forms:xyz-from-stream (make-string-input-stream text)))
           (top-form (forms:xyz-get-top-form forms pos)))
        (forms:xyz-get-outer-form top-form pos)))


(defun test-child-form ()
    (clue:test "Child Form"
        (let* ((text (format NIL "(if (a b) (c d (e f)))"))
               (all (get-child-form text (pos:create 0 1)))
               (child-1 (get-child-form text (pos:create 0 5)))
               (child-2 (get-child-form text (pos:create 0 12)))
               (child-3 (get-child-form text (pos:create 0 17))))
            (clue:check-equal :expected (pos:create 0 0)
                              :actual (form:xyz-get-start all))
            (clue:check-equal :expected (pos:create 0 4)
                              :actual (form:xyz-get-start child-1))
            (clue:check-equal :expected (pos:create 0 10)
                              :actual (form:xyz-get-start child-2))
            (clue:check-equal :expected (pos:create 0 15)
                              :actual (form:xyz-get-start child-3)))))


(defun run-all ()
    (clue:suite "Forms Tests"
        (test-child-form)))
