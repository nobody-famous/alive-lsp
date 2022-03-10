(defpackage :alive/test/format/range
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:formatting :alive/format)
                      (:range :alive/range)
                      (:pos :alive/position)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/format/range)


(defun do-format (text range)
    (with-input-from-string (s text)
        (formatting:range s range)))


(defun spaces ()
    (run:test "Remove Spaces Test"
              (lambda ()
                  (do-format " ( a b ) " (range:create :start (pos:create :line 0 :col 4)
                                                       :end (pos:create :line 0 :col 9))))))


(defun run-all ()
    (run:suite "Range Format Tests"
               (lambda ()
                   (spaces))))
