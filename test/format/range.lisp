(defpackage :alive/test/format/range
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:formatting :alive/format)
                      (:range :alive/range)
                      (:pos :alive/position)
                      (:edit :alive/text-edit)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/format/range)


(defun check-format (text range expected)
    (with-input-from-string (s text)
        (let ((actual (formatting:range s range)))
            (check:are-equal expected actual))))


(defun spaces ()
    (run:test "Remove Spaces Test"
              (lambda ()
                  (check-format (format nil " ( a  b  )  ")
                                (range:create (pos:create 0 0) (pos:create 0 11))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 2) (pos:create 0 3))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 4) (pos:create 0 6))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 0 7) (pos:create 0 9))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 10) (pos:create 0 12))
                                                   :text ""))))))


(defun run-all ()
    (run:suite "Range Format Tests"
               (lambda ()
                   (spaces))))
