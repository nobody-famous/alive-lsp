(defpackage :alive/test/format/on-type
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:formatting :alive/format)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:edit :alive/text-edit)))

(in-package :alive/test/format/on-type)


(defun check-format (&key text pos expected)
    (with-input-from-string (s text)
        (let ((actual (formatting:on-type s :pos pos)))
            (clue:check-equal :expected expected
                              :actual actual))))


(defun test-defun ()
    (clue:test "Defun"
        (check-format :text (format NIL "(defun foo ()~%~%)")
                      :pos (pos:create 1 0)
                      :expected (list (edit:create :range (range:create (pos:create 1 0) (pos:create 1 0))
                                                   :text "  ")))))


(defun run-all ()
    (clue:suite "Format On Type Tests"
        (test-defun)))
