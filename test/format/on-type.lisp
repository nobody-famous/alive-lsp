(defpackage :alive/test/format/on-type
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:edit :alive/text-edit)
                      (:formatting :alive/format)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/test/format/on-type)


(defun check-format (&key text pos expected)
    (with-input-from-string (s text)
        (let* ((tokens (tokenizer:from-stream s))
               (actual (formatting:on-type tokens :pos pos)))
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
