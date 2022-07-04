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


(defun validate-behavior (text)
    (let* ((input (make-string-input-stream text))
           (forms (alive/parse/stream:from input)))
        (loop :for form :in forms :do
                  (format T "~A~%" form))))


(defun test-quoted-list ()
    (clue:test "Test quoted list"
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
    (clue:test "Test quoted symbol with package"
        (check-forms (format nil "'foo:bar")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 8)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 8)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-sym-with-pkg ()
    (clue:test "Test symbol with package"
        (check-forms (format nil "foo:bar")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 7)
                                        :form-type types:*symbol*
                                        :kids (list))))))


(defun test-empty-text ()
    (clue:test "Test empty text"
        (check-forms ""
                     (list))))


(defun test-foo ()
    (clue:test "Test foo symbol"
        (check-forms "foo"
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 3)
                                        :form-type types:*symbol*)))))


(defun test-empty-list ()
    (clue:test "Test empty list"
        (check-forms "()"
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 2)
                                        :form-type types:*open-paren*)))))


(defun test-empty-list-with-nl ()
    (clue:test "Test empty list with newline"
        (check-forms (format nil "(~%)")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 1 1)
                                        :form-type types:*open-paren*)))))


(defun test-quoted-list-with-foo ()
    (clue:test "Test quoted list with foo symbol"
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
    (clue:test "Test back quote with comma"
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
    (clue:test "Test double quote with comma"
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
    (clue:test "Test quoted symbol"
        (check-forms (format nil "'foo")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 4)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 4)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-back-quoted-symbol ()
    (clue:test "Test back quoted symbol"
        (check-forms (format nil "`foo")
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 4)
                                        :form-type types:*back-quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 4)
                                                                 :form-type types:*symbol*
                                                                 :kids (list))))))))


(defun test-symbols-list ()
    (clue:test "Test list of symbols"
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
    (clue:test "Test in-package form"
        (check-forms (format nil (format nil "(IN-PACKAGE :foo)~%bar"))
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 17)
                                        :in-pkg T
                                        :form-type types:*open-paren*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 11)
                                                                 :form-type types:*symbol*
                                                                 :in-pkg T)
                                                    (form:create :start (pos:create 0 12)
                                                                 :end (pos:create 0 16)
                                                                 :form-type types:*symbol*
                                                                 :in-pkg T)))
                           (form:create :start (pos:create 1 0)
                                        :end (pos:create 1 3)
                                        :form-type types:*symbol*)))))


(defun test-sym-with-pkg-nl ()
    (clue:test "Test symbol with package with newline"
        (check-forms (format nil (format nil "'foo:bar~%fff"))
                     (list (form:create :start (pos:create 0 0)
                                        :end (pos:create 0 8)
                                        :form-type types:*quote*
                                        :kids (list (form:create :start (pos:create 0 1)
                                                                 :end (pos:create 0 8)
                                                                 :form-type types:*symbol*)))
                           (form:create :start (pos:create 1 0)
                                        :end (pos:create 1 3)
                                        :form-type types:*symbol*)))))


(defun run-all ()
    (clue:suite "Test parse forms"
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
        (test-sym-with-pkg-nl)))
