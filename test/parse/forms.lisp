(defpackage :alive/test/parse/forms
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:token :alive/parse/token)
                      (:p :alive/parse/forms)))

(in-package :alive/test/parse/forms)


(defun check-forms (text expected)
    (let* ((input (make-string-input-stream text))
           (forms (p:from-stream input)))

        (clue:check-equal :expected expected
                          :actual forms)))


(defun test-quoted-list ()
    (clue:test "Quoted list"
        (check-forms (format nil "('(1))")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 6)
                                        :form-type types:*open-paren*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 5)
                                                                 :form-type types:*quote*
                                                                 :kids (list (form:create :start (pos:create 0 2)
                                                                                          :end (pos:create 0 5)
                                                                                          :form-type types:*open-paren*
                                                                                          :kids (list (form:create :start (pos:create 0 3)
                                                                                                                   :end (pos:create 0 4)
                                                                                                                   :form-type types:*symbol*
                                                                                                                   :kids (list))))))))))))


(defun test-quoted-sym-with-pkg ()
    (clue:test "Quoted symbol with package"
        (check-forms (format nil "'foo:bar")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 8)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 8)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-sym-with-pkg ()
    (clue:test "Symbol with package"
        (check-forms (format nil "foo:bar")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 7)
                                        :form-type types:*symbol*
                                        :kids (list))))))


(defun test-empty-text ()
    (clue:test "Empty text"
        (check-forms ""
                     (list))))


(defun test-foo ()
    (clue:test "Foo symbol"
        (check-forms "foo"
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 3)
                                        :form-type types:*symbol*)))))


(defun test-empty-list ()
    (clue:test "Empty list"
        (check-forms "()"
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 2)
                                        :form-type types:*open-paren*)))))


(defun test-empty-list-with-nl ()
    (clue:test "Empty list with newline"
        (check-forms (format nil "(~%)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 1 1)
                                        :form-type types:*open-paren*)))))


(defun test-quoted-list-with-foo ()
    (clue:test "Quoted list with foo symbol"
        (check-forms (format nil "'(foo)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 6)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 6)
                                                                 :form-type types:*open-paren*
                                                                 :kids (list (form:create :start (pos:create 0 2)
                                                                                          :end (pos:create 0 5)
                                                                                          :form-type types:*symbol*)))))))))


(defun test-back-quote-with-comma ()
    (clue:test "Back quote with comma"
        (check-forms (format nil "`(foo ,bar)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 11)
                                        :form-type types:*back-quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 11)
                                                                 :form-type types:*open-paren*
                                                                 :kids (list (form:create :start (pos:create 0 2)
                                                                                          :end (pos:create 0 5)
                                                                                          :form-type types:*symbol*
                                                                                          :kids (list))
                                                                             (form:create :start (pos:create 0 6)
                                                                                          :end (pos:create 0 10)
                                                                                          :form-type types:*comma*
                                                                                          :kids (list (form:create :start (pos:create 0 7)
                                                                                                                   :end (pos:create 0 10)
                                                                                                                   :form-type types:*symbol*
                                                                                                                   :kids (list))))))))))))


(defun test-double-quote-with-comma ()
    (clue:test "Double quote with comma"
        (check-forms (format nil "''(foo ,bar)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 12)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 2)
                                                                 :end (pos:create 0 12)
                                                                 :form-type types:*open-paren*
                                                                 :kids (list (form:create :start (pos:create 0 3)
                                                                                          :end (pos:create 0 6)
                                                                                          :form-type types:*symbol*
                                                                                          :kids (list))
                                                                             (form:create :start (pos:create 0 7)
                                                                                          :end (pos:create 0 11)
                                                                                          :form-type types:*comma*
                                                                                          :kids (list (form:create :start (pos:create 0 8)
                                                                                                                   :end (pos:create 0 11)
                                                                                                                   :form-type types:*symbol*
                                                                                                                   :kids (list))))))))))))


(defun test-quoted-symbol ()
    (clue:test "Quoted symbol"
        (check-forms (format nil "'foo")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 4)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 4)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-back-quoted-symbol ()
    (clue:test "Back quoted symbol"
        (check-forms (format nil "`foo")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 4)
                                        :form-type types:*back-quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 4)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-symbols-list ()
    (clue:test "List of symbols"
        (check-forms (format nil "(a bb cccc)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 11)
                                        :form-type types:*open-paren*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 2)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))
                                                    (form:create :start (pos:create 0 3)
                                                                 :end (pos:create 0 5)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))
                                                    (form:create :start (pos:create 0 6)
                                                                 :end (pos:create 0 10)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-in-package ()
    (clue:test "In-package form"
        (check-forms (format nil (format nil "(IN-PACKAGE :foo)~%bar"))
                     (list (form:create :start (pos:create 0 0)
                                        :start-offset 0
                                        :end (pos:create 0 17)
                                        :end-offset 17
                                        :in-pkg T
                                        :form-type types:*open-paren*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :start-offset 1
                                                                 :end (pos:create 0 11)
                                                                 :end-offset 11
                                                                 :form-type types:*symbol*
                                                                 :in-pkg T)
                                                    (form:create :start (pos:create 0 12)
                                                                 :start-offset 12
                                                                 :end (pos:create 0 16)
                                                                 :end-offset 16
                                                                 :form-type types:*symbol*
                                                                 :in-pkg T)))
                           (form:create :start (pos:create 1 0)
                                        :start-offset 18
                                        :end (pos:create 1 3)
                                        :end-offset 21
                                        :form-type types:*symbol*)))))


(defun test-sym-with-pkg-nl ()
    (clue:test "Symbol with package with newline"
        (check-forms (format nil (format nil "'foo:bar~%fff"))
                     (list (form:create :start (pos:create 0 0)
                                        :start-offset 0
                                        :end (pos:create 0 8)
                                        :end-offset 8
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :start-offset 1
                                                                 :end (pos:create 0 8)
                                                                 :end-offset 8
                                                                 :form-type types:*symbol*)))
                           (form:create :start (pos:create 1 0)
                                        :start-offset 9
                                        :end (pos:create 1 3)
                                        :end-offset 12
                                        :form-type types:*symbol*)))))


(defun test-getters ()
    (clue:test "Getters"
        (let ((form (form:create :start (pos:create 1 0)
                                 :start-offset 9
                                 :end (pos:create 1 3)
                                 :end-offset 12
                                 :form-type types:*symbol*)))
            (clue:check-equal :expected 9
                              :actual (form:get-start-offset form)))))


(defun run-all ()
    (clue:suite "Parse forms"
        (test-quoted-list)
        (test-quoted-sym-with-pkg)
        (test-sym-with-pkg)
        (test-empty-text)
        (test-foo)
        (test-empty-list)
        (test-empty-list-with-nl)
        (test-quoted-list-with-foo)
        (test-back-quote-with-comma)
        (test-double-quote-with-comma)
        (test-quoted-symbol)
        (test-back-quoted-symbol)
        (test-symbols-list)
        (test-in-package)
        (test-sym-with-pkg-nl)
        (test-getters)))
