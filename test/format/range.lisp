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
    (run:test "Spaces Test"
              (lambda ()
                  (check-format (format nil "()()")
                                (range:create (pos:create 0 0) (pos:create 1 0))
                                (list (edit:create :range (range:create (pos:create 0 2) (pos:create 0 2))
                                                   :text " ")))

                  (check-format (format nil "a()b")
                                (range:create (pos:create 0 0) (pos:create 1 0))
                                (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 1))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 0 3) (pos:create 0 3))
                                                   :text " ")))

                  (check-format (format nil "; Do not remove~%  ")
                                (range:create (pos:create 0 0) (pos:create 1 2))
                                (list (edit:create :range (range:create (pos:create 0 15) (pos:create 1 2))
                                                   :text "")))

                  (check-format (format nil "'(1 2 3 4~%   5 6 7 8)")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 9) (pos:create 1 3))
                                                   :text (format nil "~%    "))))

                  (check-format (format nil " ( ; Do not remove~%)  ")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                                   :text "")))

                  (check-format (format nil " (   ; Do not remove~%)  ")
                                (range:create (pos:create 0 0) (pos:create 2 0))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 2) (pos:create 0 5))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                                   :text "")))

                  (check-format (format nil " ( a  b  )  ")
                                (range:create (pos:create 0 0) (pos:create 1 0))
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

                  (check-format (format nil "dd~%( a~%    b~%      c)")
                                (range:create (pos:create 2 0) (pos:create 3 0))
                                (list (edit:create :range (range:create (pos:create 1 3) (pos:create 2 4))
                                                   :text (format nil "~%  "))))

                  (check-format (format nil " (((( a  b  ~%  )~%)~%)~%)  ")
                                (range:create (pos:create 0 0) (pos:create 5 0))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 5) (pos:create 0 6))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 7) (pos:create 0 9))
                                                   :text " ")
                                      (edit:create :range (range:create (pos:create 0 10) (pos:create 1 2))
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
                  (check-format (format nil "( ; Foo~%")
                                (range:create (pos:create 0 0) (pos:create 2 0))
                                (list (edit:create :range (range:create (pos:create 0 7) (pos:create 1 0))
                                                   :text (format nil ""))))

                  (check-format (format nil "( ; Foo~%)")
                                (range:create (pos:create 0 0) (pos:create 2 0))
                                (list))

                  (check-format (format nil "(a~%    b)")
                                (range:create (pos:create 1 0) (pos:create 1 6))
                                (list (edit:create :range (range:create (pos:create 0 2) (pos:create 1 4))
                                                   :text (format nil "~% "))))

                  (check-format (format nil "(    a b~%c d)")
                                (range:create (pos:create 0 0) (pos:create 1 3))
                                (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                                                   :text (format nil "~%   "))))

                  (check-format (format nil "(    a (b~%c)   ~%   d)")
                                (range:create (pos:create 0 0) (pos:create 2 5))
                                (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                                                   :text "")
                                      (edit:create :range (range:create (pos:create 0 9) (pos:create 1 0))
                                                   :text (format nil "~%    "))
                                      (edit:create :range (range:create (pos:create 1 2) (pos:create 2 3))
                                                   :text (format nil "~%   "))))

                  (check-format (format nil "(a~%    (b~%        (c)))")
                                (range:create (pos:create 0 0) (pos:create 3 0))
                                (list (edit:create :range (range:create (pos:create 0 2) (pos:create 1 4))
                                                   :text (format nil "~% "))
                                      (edit:create :range (range:create (pos:create 1 6) (pos:create 2 8))
                                                   :text (format nil "~%  "))))

                  (check-format (format nil "  (a~%(b~%c))")
                                (range:create (pos:create 0 0) (pos:create 3 0))
                                (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 2))
                                                   :text (format nil ""))
                                      (edit:create :range (range:create (pos:create 0 4) (pos:create 1 0))
                                                   :text (format nil "~% "))
                                      (edit:create :range (range:create (pos:create 1 2) (pos:create 2 0))
                                                   :text (format nil "~%  "))))

                  (check-format (format nil "(  defun foo ()~%      nil)")
                                (range:create (pos:create 0 0) (pos:create 3 0))
                                (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 3))
                                                   :text (format nil ""))
                                      (edit:create :range (range:create (pos:create 0 15) (pos:create 1 6))
                                                   :text (format nil "~%   ")))))))


(defun run-all ()
    (run:suite "Range Format Tests"
               (lambda ()
                   (spaces)
                   (indent))))
