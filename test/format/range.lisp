(defpackage :alive/test/format/range
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:formatting :alive/format)
                      (:range :alive/range)
                      (:pos :alive/position)
                      (:edit :alive/text-edit)
                      (:utils :alive/test/utils)))

(in-package :alive/test/format/range)


(defun check-format (text range expected)
    (with-input-from-string (s text)
        (let ((actual (formatting:range s range)))
            (clue:check-equal :expected expected
                              :actual actual))))


(defun test-insert-between-lists ()
    (clue:test "Insert between lists"
        (check-format (format nil "()()")
                      (range:create (pos:create 0 0) (pos:create 1 0))
                      (list (edit:create :range (range:create (pos:create 0 2) (pos:create 0 2))
                                         :text " ")))))


(defun test-before-after-list ()
    (clue:test "Before/after list"
        (check-format (format nil "a()b")
                      (range:create (pos:create 0 0) (pos:create 1 0))
                      (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 1))
                                         :text " ")
                            (edit:create :range (range:create (pos:create 0 3) (pos:create 0 3))
                                         :text " ")))))


(defun test-nl-after-comment ()
    (clue:test "Newline after comment"
        (check-format (format nil "; Do not remove~%  ")
                      (range:create (pos:create 0 0) (pos:create 1 2))
                      (list (edit:create :range (range:create (pos:create 0 15) (pos:create 1 2))
                                         :text (format NIL "~A" alive/format:eol))))))


(defun test-quoted-list-nl ()
    (clue:test "Quoted list with newline"
        (check-format (format nil "'(1 2 3 4~%   5 6 7 8)")
                      (range:create (pos:create 0 0) (pos:create 1 3))
                      (list (edit:create :range (range:create (pos:create 0 9) (pos:create 1 3))
                                         :text (format nil "~%    "))))))


(defun test-comment-after-open ()
    (clue:test "Comment after open paren"
        (check-format (format nil " ( ; Do not remove~%)  ")
                      (range:create (pos:create 0 0) (pos:create 1 3))
                      (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                         :text "")
                            (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                         :text "")))))


(defun test-comment-after-open-spaces ()
    (clue:test "Comment after open multiple spaces"
        (check-format (format nil " (   ; Do not remove~%)  ")
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 1))
                                         :text "")
                            (edit:create :range (range:create (pos:create 0 2) (pos:create 0 5))
                                         :text " ")
                            (edit:create :range (range:create (pos:create 1 1) (pos:create 1 3))
                                         :text "")))))


(defun test-list ()
    (clue:test "List"
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
                                         :text "")))))


(defun test-list-range ()
    (clue:test "List range"
        (check-format (format nil " ( a  b  )  ")
                      (range:create (pos:create 0 3) (pos:create 0 7))
                      (list (edit:create :range (range:create (pos:create 0 4) (pos:create 0 6))
                                         :text " ")))))


(defun test-ml-range ()
    (clue:test "Multiline range"
        (check-format (format nil "dd~%( a~%    b~%      c)")
                      (range:create (pos:create 2 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 1 3) (pos:create 2 4))
                                         :text (format nil "~%  "))))))


(defun test-stack-close-parens ()
    (clue:test "Stack close parens"
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
                                         :text "")))))


(defun test-indent-loop ()
    (clue:test "Indent loop"
        (check-format (format nil "(loop :for i :from 1 :to 10~%:do~%nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 27) (pos:create 1 0))
                                         :text (format nil "~A      " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 3) (pos:create 2 0))
                                         :text (format nil "~A        " alive/format:eol))))))


(defun test-indent-loop-ml ()
    (clue:test "Indent loop multiline"
        (check-format (format nil "(loop :for i :from 1 :to 10 :do~%nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 31) (pos:create 1 0))
                                         :text (format nil "~A        " alive/format:eol))))))


(defun test-indent-loop-ml-token ()
    (clue:test "Indent loop multiline token"
        (check-format (format nil "(#|~ASome stuff~A|# loop :for i :from 1 :to 10 :do~%nil)" alive/format:eol alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 5 0))
                      (list (edit:create :range (range:create (pos:create 0 31) (pos:create 1 0))
                                         :text (format nil "~A        " alive/format:eol))))))


(defun test-indent-cond ()
    (clue:test "Indent cond"
        (check-format (format nil "(cond (a~%b~%c))")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                                         :text (format nil "~A        " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 1) (pos:create 2 0))
                                         :text (format nil "~A        " alive/format:eol))))))


(defun test-indent-and ()
    (clue:test "Indent and"
        (check-format (format nil "(and nil~%nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                                         :text (format nil "~A     " alive/format:eol))))))


(defun test-indent-rest ()
    (clue:test "Indent rest"
        (check-format (format nil "(in-package :alive/logger)~A(msg log~A\"\"~A\"\")"
                          alive/format:eol
                          alive/format:eol
                          alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 4 0))
                      (list (edit:create :range (range:create (pos:create 1 8) (pos:create 2 0))
                                         :text (format nil "~A    " alive/format:eol))
                            (edit:create :range (range:create (pos:create 2 2) (pos:create 3 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-strip-indent ()
    (clue:test "Strip indent"
        (check-format (format nil "(error)~%     foo")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 7) (pos:create 1 5))
                                         :text (format nil "~A" alive/format:eol))))))


(defun test-indent-body ()
    (clue:test "Indent body"
        (check-format (format nil "(defun foo~%()~%nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 10) (pos:create 1 0))
                                         :text (format nil "~A    " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 2) (pos:create 2 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-align-list ()
    (clue:test "Align list"
        (check-format (format nil "(a:b c~%d)")
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list (edit:create :range (range:create (pos:create 0 6) (pos:create 1 0))
                                         :text (format nil "~A     " alive/format:eol))))))


(defun test-align-list-2 ()
    (clue:test "Align list 2"
        (check-format (format nil "(:b c~%d)")
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list (edit:create :range (range:create (pos:create 0 5) (pos:create 1 0))
                                         :text (format nil "~A    " alive/format:eol))))))


(defun test-nl-after-open ()
    (clue:test "Newline after open parens"
        (check-format (format nil "( ; Foo~A" alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list))))


(defun test-nl-after-open-with-close ()
    (clue:test "Newline after open with close parens"
        (check-format (format nil "( ; Foo~A)" alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list))))


(defun test-align-list-3 ()
    (clue:test "Align list 3"
        (check-format (format nil "(a~%    b)")
                      (range:create (pos:create 1 0) (pos:create 1 6))
                      (list (edit:create :range (range:create (pos:create 0 2) (pos:create 1 4))
                                         :text (format nil "~A " alive/format:eol))))))


(defun test-align-list-4 ()
    (clue:test "Align list 4"
        (check-format (format nil "(    a b~%c d)")
                      (range:create (pos:create 0 0) (pos:create 1 3))
                      (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                                         :text "")
                            (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                                         :text (format nil "~A   " alive/format:eol))))))


(defun test-align-nested ()
    (clue:test "Align nested lists"
        (check-format (format nil "(    a (b~%c)   ~%   d)")
                      (range:create (pos:create 0 0) (pos:create 2 5))
                      (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 5))
                                         :text "")
                            (edit:create :range (range:create (pos:create 0 9) (pos:create 1 0))
                                         :text (format nil "~A    " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 2) (pos:create 2 3))
                                         :text (format nil "~A   " alive/format:eol))))))


(defun test-align-nested-2 ()
    (clue:test "Align nested lists 2"
        (check-format (format nil "(a~%    (b~%        (c)))")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 2) (pos:create 1 4))
                                         :text (format nil "~A " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 6) (pos:create 2 8))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-align-nested-3 ()
    (clue:test "Align nested lists 3"
        (check-format (format nil "  (a~%(b~%c))")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 0) (pos:create 0 2))
                                         :text (format nil ""))
                            (edit:create :range (range:create (pos:create 0 4) (pos:create 1 0))
                                         :text (format nil "~A " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 2) (pos:create 2 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-indent-defun ()
    (clue:test "Indent defun"
        (check-format (format nil "(  defun foo ()~%      nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 1) (pos:create 0 3))
                                         :text (format nil ""))
                            (edit:create :range (range:create (pos:create 0 15) (pos:create 1 6))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-strip-leading-nl ()
    (clue:test "Strip leading newlines"
        (check-format (format nil "~%~%()")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 0) (pos:create 2 0))
                                         :text (format nil ""))))))


(defun test-strip-nl-in-list ()
    (clue:test "Strip newlines in list"
        (check-format (format nil "(~%~%)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 1) (pos:create 2 0))
                                         :text (format nil ""))))))


(defun test-macro-body ()
    (clue:test "Macro body"
        (check-format (format nil "(unwind-protect~%nil~%nil)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 15) (pos:create 1 0))
                                         :text (format nil "~A    " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 3) (pos:create 2 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-in-package ()
    (clue:test "in-package"
        (check-format (format nil "(in-package~A#:foo)" alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list (edit:create :range (range:create (pos:create 0 11) (pos:create 1 0))
                                         :text (format nil "~A  " alive/format:eol))))

        (check-format (format nil "(in-package~Abar:foo)" alive/format:eol)
                      (range:create (pos:create 0 0) (pos:create 2 0))
                      (list (edit:create :range (range:create (pos:create 0 11) (pos:create 1 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-insert-range ()
    (clue:test "Insert range"
        (check-format (format nil "()foo")
                      (range:create (pos:create 0 0) (pos:create 0 1))
                      nil)))


(defun test-assoc ()
    (clue:test "Assoc"
        (check-format (format nil "(assoc~%a~%b)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 6) (pos:create 1 0))
                                         :text (format nil "~A  " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 1) (pos:create 2 0))
                                         :text (format nil "~A  " alive/format:eol))))))


(defun test-invalid-fn ()
    (clue:test "Invalid function"
        (check-format (format nil "(assoc::~%a~%b)")
                      (range:create (pos:create 0 0) (pos:create 3 0))
                      (list (edit:create :range (range:create (pos:create 0 8) (pos:create 1 0))
                                         :text (format nil "~A      " alive/format:eol))
                            (edit:create :range (range:create (pos:create 1 1) (pos:create 2 0))
                                         :text (format nil "~A      " alive/format:eol))))))


(defun run-all ()
    (clue:suite "Range Format Tests"
        (test-insert-between-lists)
        (test-before-after-list)
        (test-nl-after-comment)
        (test-quoted-list-nl)
        (test-comment-after-open)
        (test-comment-after-open-spaces)
        (test-list)
        (test-list-range)
        (test-ml-range)
        (test-stack-close-parens)
        (test-indent-loop)
        (test-indent-loop-ml)
        (test-indent-loop-ml-token)
        (test-indent-cond)
        (test-indent-and)
        (test-indent-rest)
        (test-strip-indent)
        (test-indent-body)
        (test-align-list)
        (test-align-list-2)
        (test-align-list-3)
        (test-align-list-4)
        (test-nl-after-open)
        (test-nl-after-open-with-close)
        (test-align-nested)
        (test-align-nested-2)
        (test-align-nested-3)
        (test-indent-defun)
        (test-strip-leading-nl)
        (test-strip-nl-in-list)
        (test-macro-body)
        (test-in-package)
        (test-insert-range)
        (test-assoc)
        (test-invalid-fn)))