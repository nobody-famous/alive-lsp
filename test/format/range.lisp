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
                  (check-format (format nil " ( ; Do not remove~%)  ")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                                   :text "")))

                  (check-format (format nil " (   ; Do not remove~%)  ")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 2) (pos:create 0 5))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                                   :text "")))

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
                                                   :text "")))

                  (check-format (format nil " ( a  b  )  ")
                                (range:create (pos:create 0 3) (pos:create 0 7))
                                (list (edit:create :range (range:create (pos:create 0 4) (pos:create 0 6))
                                                   :text " ")))

                  (check-format (format nil " ( a  b  ~%  )~%)~%)~%)  ")
                                (range:create (pos:create 0 0) (pos:create 4 3))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 2) (pos:create 0 3))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 4) (pos:create 0 6))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 0 7) (pos:create 1 2))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 1 3) (pos:create 2 0))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 2 1) (pos:create 3 0))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 3 1) (pos:create 4 0))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 4 1) (pos:create 4 3))
                                                   :text ""))))))


(defun indent ()
    (run:test "Indent Test"
              (lambda ()
                  ;   (check-format (format nil "(    a b~%c d)")
                  ;                 (range:create (pos:create 0 0) (pos:create 1 3))
                  ;                 (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                  ;                                    :text "")
                  ;                       (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                  ;                                    :text (format nil "~% "))))

                  (check-format (format nil "(    a (b~%c)   ~%   d)")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 9) (pos:create 1 0))
                                                   :text (format nil "~%    "))
                                      (edit:create :range (range:create (pos:create 1 2) (pos:create 2 3))
                                                   :text (format nil "~% ")))))))


(defun run-all ()
    (run:suite "Range Format Tests"
               (lambda ()
                   (spaces)
                   (indent))))
