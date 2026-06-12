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

        #+n (loop :for form :in forms
                  :do (alive/test/utils:print-hash-table "***** FORM" form)
                      (loop :for token :in (gethash "tokens" form)
                            :do (alive/test/utils:print-hash-table "***** TOKEN" token))
                      (loop :for kid :in (gethash "kids" form)
                            :do (alive/test/utils:print-hash-table "***** KID" kid)
                                (loop :for token :in (gethash "tokens" kid)
                                      :do (alive/test/utils:print-hash-table "***** KID TOKEN" token))
                                (loop :for kid :in (gethash "kids" kid)
                                      :do (alive/test/utils:print-hash-table "***** GRANDKID" kid)
                                          (loop :for token :in (gethash "tokens" kid)
                                                :do (alive/test/utils:print-hash-table "***** GRANDKID TOKEN" token)))))
        (clue:check-equal :expected expected
                          :actual forms)))


(defun xyz-check-forms (text expected)
    (let* ((input (make-string-input-stream text))
           (forms (p:xyz-from-stream input)))

        #+n (loop :for form :in forms
                  :do (format T "***** FORM ~A~%" form)

                      (loop :for token :in (form:xyz-get-tokens form)
                            :do (alive/test/utils:print-hash-table "***** TOKEN" token))
                      (loop :for kid :in (form:xyz-get-kids form)
                            :do (format T "***** KID ~A~%" kid)
                                (loop :for token :in (form:xyz-get-tokens kid)
                                      :do (alive/test/utils:print-hash-table "***** KID TOKEN" token))
                                (loop :for kid :in (form:xyz-get-kids kid)
                                      :do (format T "***** GRAND KID ~A~%" kid)
                                          (loop :for token :in (form:xyz-get-tokens kid)
                                                :do (alive/test/utils:print-hash-table "***** GRAND KID TOKEN" token))
                                          (loop :for kid :in (form:xyz-get-kids kid)
                                                :do (format T "***** GREAT GRAND KID ~A~%" kid)
                                                    (loop :for token :in (form:xyz-get-tokens kid)
                                                          :do (alive/test/utils:print-hash-table "***** GREAT GRAND KID TOKEN" token))))))

        (clue:check-equal :expected expected
                          :actual forms)))


(defun test-quoted-list ()
    (clue:test "Quoted list"
        (xyz-check-forms (format nil "('(1))")
                         (list (form:xyz-create :form-type types:*open-paren*
                                                :tokens (list (token:create :type-value types:*open-paren*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "(")
                                                              (token:create :type-value types:*close-paren*
                                                                            :start (pos:create 0 5)
                                                                            :start-offset 5
                                                                            :end (pos:create 0 6)
                                                                            :end-offset 6
                                                                            :text ")"))
                                                :kids (list (form:xyz-create :form-type types:*quote*
                                                                             :tokens (list (token:create :type-value types:*quote*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "'"))
                                                                             :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                                                          :tokens (list (token:create :type-value types:*open-paren*
                                                                                                                                      :start (pos:create 0 2)
                                                                                                                                      :start-offset 2
                                                                                                                                      :end (pos:create 0 3)
                                                                                                                                      :end-offset 3
                                                                                                                                      :text "(")
                                                                                                                        (token:create :type-value types:*close-paren*
                                                                                                                                      :start (pos:create 0 4)
                                                                                                                                      :start-offset 4
                                                                                                                                      :end (pos:create 0 5)
                                                                                                                                      :end-offset 5
                                                                                                                                      :text ")"))
                                                                                                          :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                                                       :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                                                   :start (pos:create 0 3)
                                                                                                                                                                   :start-offset 3
                                                                                                                                                                   :end (pos:create 0 4)
                                                                                                                                                                   :end-offset 4
                                                                                                                                                                   :text "1"))
                                                                                                                                       :kids (list))))))))))))


(defun test-quoted-sym-with-pkg ()
    (clue:test "Quoted symbol with package"
        (xyz-check-forms (format nil "'foo:bar")
                         (list (form:xyz-create :form-type types:*quote*
                                                :tokens (list (token:create :type-value types:*quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "'"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 4)
                                                                                                         :end-offset 4
                                                                                                         :text "foo")
                                                                                           (token:create :type-value types:*colons*
                                                                                                         :start (pos:create 0 4)
                                                                                                         :start-offset 4
                                                                                                         :end (pos:create 0 5)
                                                                                                         :end-offset 5
                                                                                                         :text ":")
                                                                                           (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 5)
                                                                                                         :start-offset 5
                                                                                                         :end (pos:create 0 8)
                                                                                                         :end-offset 8
                                                                                                         :text "bar"))
                                                                             :kids (list))))))))


(defun test-sym-with-pkg ()
    (clue:test "Symbol with package"
        (xyz-check-forms (format nil "foo:bar")
                         (list (form:xyz-create :form-type types:*symbol*
                                                :tokens (list (token:create :type-value types:*symbol*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 3)
                                                                            :end-offset 3
                                                                            :text "foo")
                                                              (token:create :type-value types:*colons*
                                                                            :start (pos:create 0 3)
                                                                            :start-offset 3
                                                                            :end (pos:create 0 4)
                                                                            :end-offset 4
                                                                            :text ":")
                                                              (token:create :type-value types:*symbol*
                                                                            :start (pos:create 0 4)
                                                                            :start-offset 4
                                                                            :end (pos:create 0 7)
                                                                            :end-offset 7
                                                                            :text "bar"))
                                                :kids (list))))))


(defun test-empty-text ()
    (clue:test "Empty text"
        (xyz-check-forms ""
                         (list))))


(defun test-foo ()
    (clue:test "Foo symbol"
        (xyz-check-forms "foo"
                         (list (form:xyz-create :form-type types:*symbol*
                                                :tokens (list (token:create :type-value types:*symbol*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 3)
                                                                            :end-offset 3
                                                                            :text "foo")))))))


(defun test-empty-list ()
    (clue:test "Empty list"
        (xyz-check-forms "()"
                         (list (form:xyz-create :form-type types:*open-paren*
                                                :tokens (list (token:create :type-value types:*open-paren*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "(")
                                                              (token:create :type-value types:*close-paren*
                                                                            :start (pos:create 0 1)
                                                                            :start-offset 1
                                                                            :end (pos:create 0 2)
                                                                            :end-offset 2
                                                                            :text ")")))))))


(defun test-empty-list-with-nl ()
    (clue:test "Empty list with newline"
        (xyz-check-forms (format nil "(~%)")
                         (list (form:xyz-create :form-type types:*open-paren*
                                                :tokens (list (token:create :type-value types:*open-paren*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "(")
                                                              (token:create :type-value types:*close-paren*
                                                                            :start (pos:create 1 0)
                                                                            :start-offset 2
                                                                            :end (pos:create 1 1)
                                                                            :end-offset 3
                                                                            :text ")")))))))


(defun test-quoted-list-with-foo ()
    (clue:test "Quoted list with foo symbol"
        (xyz-check-forms (format nil "'(foo)")
                         (list (form:xyz-create :form-type types:*quote*
                                                :tokens (list (token:create :type-value types:*quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "'"))
                                                :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                             :tokens (list (token:create :type-value types:*open-paren*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "(")
                                                                                           (token:create :type-value types:*close-paren*
                                                                                                         :start (pos:create 0 5)
                                                                                                         :start-offset 5
                                                                                                         :end (pos:create 0 6)
                                                                                                         :end-offset 6
                                                                                                         :text ")"))
                                                                             :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                          :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                      :start (pos:create 0 2)
                                                                                                                                      :start-offset 2
                                                                                                                                      :end (pos:create 0 5)
                                                                                                                                      :end-offset 5
                                                                                                                                      :text "foo")))))))))))


(defun test-double-comma ()
    (clue:test "Double commas"
        (xyz-check-forms (format nil "`(,,foo)")
                         (list (form:xyz-create :form-type types:*back-quote*
                                                :tokens (list (token:create :type-value types:*back-quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "`"))
                                                :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                             :tokens (list (token:create :type-value types:*open-paren*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "(")
                                                                                           (token:create :type-value types:*close-paren*
                                                                                                         :start (pos:create 0 7)
                                                                                                         :start-offset 7
                                                                                                         :end (pos:create 0 8)
                                                                                                         :end-offset 8
                                                                                                         :text ")"))
                                                                             :kids (list (form:xyz-create :form-type types:*comma*
                                                                                                          :tokens (list (token:create :type-value types:*comma*
                                                                                                                                      :start (pos:create 0 2)
                                                                                                                                      :start-offset 2
                                                                                                                                      :end (pos:create 0 3)
                                                                                                                                      :end-offset 3
                                                                                                                                      :text ","))
                                                                                                          :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                                                       :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                                                   :start (pos:create 0 4)
                                                                                                                                                                   :start-offset 4
                                                                                                                                                                   :end (pos:create 0 7)
                                                                                                                                                                   :end-offset 7
                                                                                                                                                                   :text "foo")))))))))))))


(defun test-back-quote-with-comma ()
    (clue:test "Back quote with comma"
        (xyz-check-forms (format nil "`(foo ,bar)")
                         (list (form:xyz-create :form-type types:*back-quote*
                                                :tokens (list (token:create :type-value types:*back-quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "`"))
                                                :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                             :tokens (list (token:create :type-value types:*open-paren*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "(")
                                                                                           (token:create :type-value types:*close-paren*
                                                                                                         :start (pos:create 0 10)
                                                                                                         :start-offset 10
                                                                                                         :end (pos:create 0 11)
                                                                                                         :end-offset 11
                                                                                                         :text ")"))
                                                                             :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                          :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                      :start (pos:create 0 2)
                                                                                                                                      :start-offset 2
                                                                                                                                      :end (pos:create 0 5)
                                                                                                                                      :end-offset 5
                                                                                                                                      :text "foo"))
                                                                                                          :kids (list))
                                                                                         (form:xyz-create :form-type types:*comma*
                                                                                                          :tokens (list (token:create :type-value types:*comma*
                                                                                                                                      :start (pos:create 0 6)
                                                                                                                                      :start-offset 6
                                                                                                                                      :end (pos:create 0 7)
                                                                                                                                      :end-offset 7
                                                                                                                                      :text ","))
                                                                                                          :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                                                       :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                                                   :start (pos:create 0 7)
                                                                                                                                                                   :start-offset 7
                                                                                                                                                                   :end (pos:create 0 10)
                                                                                                                                                                   :end-offset 10
                                                                                                                                                                   :text "bar"))
                                                                                                                                       :kids (list))))))))))))


(defun test-double-quote-with-comma ()
    (clue:test "Double quote with comma"
        (xyz-check-forms (format nil "''(foo ,bar)")
                         (list (form:xyz-create :form-type types:*quote*
                                                :tokens (list (token:create :type-value types:*quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "'"))
                                                :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                             :tokens (list (token:create :type-value types:*open-paren*
                                                                                                         :start (pos:create 0 2)
                                                                                                         :start-offset 2
                                                                                                         :end (pos:create 0 3)
                                                                                                         :end-offset 3
                                                                                                         :text "(")
                                                                                           (token:create :type-value types:*close-paren*
                                                                                                         :start (pos:create 0 11)
                                                                                                         :start-offset 11
                                                                                                         :end (pos:create 0 12)
                                                                                                         :end-offset 12
                                                                                                         :text ")"))
                                                                             :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                          :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                      :start (pos:create 0 3)
                                                                                                                                      :start-offset 3
                                                                                                                                      :end (pos:create 0 6)
                                                                                                                                      :end-offset 6
                                                                                                                                      :text "foo"))
                                                                                                          :kids (list))
                                                                                         (form:xyz-create :form-type types:*comma*
                                                                                                          :tokens (list (token:create :type-value types:*comma*
                                                                                                                                      :start (pos:create 0 7)
                                                                                                                                      :start-offset 7
                                                                                                                                      :end (pos:create 0 8)
                                                                                                                                      :end-offset 8
                                                                                                                                      :text ","))
                                                                                                          :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                                                       :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                                                   :start (pos:create 0 8)
                                                                                                                                                                   :start-offset 8
                                                                                                                                                                   :end (pos:create 0 11)
                                                                                                                                                                   :end-offset 11
                                                                                                                                                                   :text "bar"))
                                                                                                                                       :kids (list))))))))))))


(defun test-quoted-symbol ()
    (clue:test "Quoted symbol"
        (xyz-check-forms (format nil "'foo")
                         (list (form:xyz-create :form-type types:*quote*
                                                :tokens (list (token:create :type-value types:*quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "'"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 4)
                                                                                                         :end-offset 4
                                                                                                         :text "foo"))
                                                                             :kids (list))))))))

(defun test-back-quoted-symbol ()
    (clue:test "Back quoted symbol"
        (xyz-check-forms (format nil "`foo")
                         (list (form:xyz-create :form-type types:*back-quote*
                                                :tokens (list (token:create :type-value types:*back-quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "`"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 4)
                                                                                                         :end-offset 4
                                                                                                         :text "foo"))
                                                                             :kids (list))))))))


(defun test-symbols-list ()
    (clue:test "List of symbols"
        (xyz-check-forms (format nil "(a bb cccc)")
                         (list (form:xyz-create :form-type types:*open-paren*
                                                :tokens (list (token:create :type-value types:*open-paren*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "(")
                                                              (token:create :type-value types:*close-paren*
                                                                            :start (pos:create 0 10)
                                                                            :start-offset 10
                                                                            :end (pos:create 0 11)
                                                                            :end-offset 11
                                                                            :text ")"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "a"))
                                                                             :kids (list))
                                                            (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 3)
                                                                                                         :start-offset 3
                                                                                                         :end (pos:create 0 5)
                                                                                                         :end-offset 5
                                                                                                         :text "bb"))
                                                                             :kids (list))
                                                            (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 6)
                                                                                                         :start-offset 6
                                                                                                         :end (pos:create 0 10)
                                                                                                         :end-offset 10
                                                                                                         :text "cccc"))
                                                                             :kids (list))))))))


(defun test-in-package ()
    (clue:test "In-package form"
        (xyz-check-forms (format nil "(IN-PACKAGE :foo)~%bar")
                         (list (form:xyz-create :form-type types:*open-paren*
                                                :tokens (list (token:create :type-value types:*open-paren*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "(")
                                                              (token:create :type-value types:*close-paren*
                                                                            :start (pos:create 0 16)
                                                                            :start-offset 16
                                                                            :end (pos:create 0 17)
                                                                            :end-offset 17
                                                                            :text ")"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 11)
                                                                                                         :end-offset 11
                                                                                                         :text "IN-PACKAGE")))
                                                            (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*colons*
                                                                                                         :start (pos:create 0 12)
                                                                                                         :start-offset 12
                                                                                                         :end (pos:create 0 13)
                                                                                                         :end-offset 13
                                                                                                         :text ":")
                                                                                           (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 13)
                                                                                                         :start-offset 13
                                                                                                         :end (pos:create 0 16)
                                                                                                         :end-offset 16
                                                                                                         :text "foo")))))
                               (form:xyz-create :form-type types:*symbol*
                                                :tokens (list (token:create :type-value types:*symbol*
                                                                            :start (pos:create 1 0)
                                                                            :start-offset 18
                                                                            :end (pos:create 1 3)
                                                                            :end-offset 21
                                                                            :text "bar")))))))


(defun test-sym-with-pkg-nl ()
    (clue:test "Symbol with package with newline"
        (xyz-check-forms (format nil "'foo:bar~%fff")
                         (list (form:xyz-create :form-type types:*quote*
                                                :tokens (list (token:create :type-value types:*quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "'"))
                                                :kids (list (form:xyz-create :form-type types:*symbol*
                                                                             :tokens (list (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 4)
                                                                                                         :end-offset 4
                                                                                                         :text "foo")
                                                                                           (token:create :type-value types:*colons*
                                                                                                         :start (pos:create 0 4)
                                                                                                         :start-offset 4
                                                                                                         :end (pos:create 0 5)
                                                                                                         :end-offset 5
                                                                                                         :text ":")
                                                                                           (token:create :type-value types:*symbol*
                                                                                                         :start (pos:create 0 5)
                                                                                                         :start-offset 5
                                                                                                         :end (pos:create 0 8)
                                                                                                         :end-offset 8
                                                                                                         :text "bar")))))
                               (form:xyz-create :form-type types:*symbol*
                                                :tokens (list (token:create :type-value types:*symbol*
                                                                            :start (pos:create 1 0)
                                                                            :start-offset 9
                                                                            :end (pos:create 1 3)
                                                                            :end-offset 12
                                                                            :text "fff")))))))


(defun test-close-parens ()
    (clue:suite "Close parens"
        (clue:test "Close parens"
            (xyz-check-forms (format nil "(foo())")
                             (list (form:xyz-create :form-type types:*open-paren*
                                                    :tokens (list (token:create :type-value types:*open-paren*
                                                                                :start (pos:create 0 0)
                                                                                :start-offset 0
                                                                                :end (pos:create 0 1)
                                                                                :end-offset 1
                                                                                :text "(")
                                                                  (token:create :type-value types:*close-paren*
                                                                                :start (pos:create 0 6)
                                                                                :start-offset 6
                                                                                :end (pos:create 0 7)
                                                                                :end-offset 7
                                                                                :text ")"))
                                                    :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                 :tokens (list (token:create :type-value types:*symbol*
                                                                                                             :start (pos:create 0 1)
                                                                                                             :start-offset 1
                                                                                                             :end (pos:create 0 4)
                                                                                                             :end-offset 4
                                                                                                             :text "foo"))))))))

        (clue:test "Unmatched close paren"
            (xyz-check-forms (format nil ")")
                             (list (form:xyz-create :form-type types:*unmatched-close-paren*
                                                    :kids nil))))))


(defun test-getters ()
    (clue:test "Getters for symbol form"
        (let ((sym-form (form:xyz-create :form-type types:*symbol*
                                         :tokens (list (token:create :type-value types:*symbol*
                                                                     :start (pos:create 0 9)
                                                                     :start-offset 9
                                                                     :end (pos:create 0 11)
                                                                     :end-offset 11
                                                                     :text "aa")))))
            (clue:check-equal :expected 9
                              :actual (form:xyz-get-start-offset sym-form))
            (clue:check-equal :expected (pos:create 0 9)
                              :actual (form:xyz-get-start sym-form))
            (clue:check-equal :expected 11
                              :actual (form:xyz-get-end-offset sym-form))
            (clue:check-equal :expected (pos:create 0 11)
                              :actual (form:xyz-get-end sym-form))))

    (clue:test "Getters for parens form"
        (let ((paren-form (form:xyz-create :form-type types:*open-paren*
                                           :tokens (list (token:create :type-value types:*open-paren*
                                                                       :start (pos:create 0 1)
                                                                       :start-offset 1
                                                                       :end (pos:create 0 2)
                                                                       :end-offset 2
                                                                       :text "(")
                                                         (token:create :type-value types:*close-paren*
                                                                       :start (pos:create 0 5)
                                                                       :start-offset 5
                                                                       :end (pos:create 0 6)
                                                                       :end-offset 6
                                                                       :text ")"))
                                           :kids (list (form:xyz-create :form-type types:*symbol*
                                                                        :tokens (list (token:create :type-value types:*symbol*
                                                                                                    :start (pos:create 0 2)
                                                                                                    :start-offset 2
                                                                                                    :end (pos:create 0 5)
                                                                                                    :end-offset 5
                                                                                                    :text "foo")))))))
            (clue:check-equal :expected 1
                              :actual (form:xyz-get-start-offset paren-form))
            (clue:check-equal :expected (pos:create 0 1)
                              :actual (form:xyz-get-start paren-form))
            (clue:check-equal :expected 6
                              :actual (form:xyz-get-end-offset paren-form))
            (clue:check-equal :expected (pos:create 0 6)
                              :actual (form:xyz-get-end paren-form))))

    (clue:test "Getters for quoted form"
        (let ((quoted-form (form:xyz-create :form-type types:*quote*
                                            :tokens (list (token:create :type-value types:*quote*
                                                                        :start (pos:create 0 1)
                                                                        :start-offset 1
                                                                        :end (pos:create 0 2)
                                                                        :end-offset 2
                                                                        :text "'"))
                                            :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                         :tokens (list (token:create :type-value types:*open-paren*
                                                                                                     :start (pos:create 0 2)
                                                                                                     :start-offset 2
                                                                                                     :end (pos:create 0 3)
                                                                                                     :end-offset 3
                                                                                                     :text "(")
                                                                                       (token:create :type-value types:*close-paren*
                                                                                                     :start (pos:create 0 4)
                                                                                                     :start-offset 4
                                                                                                     :end (pos:create 0 5)
                                                                                                     :end-offset 5
                                                                                                     :text ")"))
                                                                         :kids (list (form:xyz-create :form-type types:*symbol*
                                                                                                      :tokens (list (token:create :type-value types:*symbol*
                                                                                                                                  :start (pos:create 0 3)
                                                                                                                                  :start-offset 3
                                                                                                                                  :end (pos:create 0 4)
                                                                                                                                  :end-offset 4
                                                                                                                                  :text "1"))
                                                                                                      :kids (list))))))))
            (clue:check-equal :expected 1
                              :actual (form:xyz-get-start-offset quoted-form))
            (clue:check-equal :expected (pos:create 0 1)
                              :actual (form:xyz-get-start quoted-form))
            (clue:check-equal :expected 5
                              :actual (form:xyz-get-end-offset quoted-form))
            (clue:check-equal :expected (pos:create 0 5)
                              :actual (form:xyz-get-end quoted-form)))))


(defun test-commas ()
    (clue:test "Commas"
        (xyz-check-forms (format nil "`(, )")
                         (list (form:xyz-create :form-type types:*back-quote*
                                                :tokens (list (token:create :type-value types:*back-quote*
                                                                            :start (pos:create 0 0)
                                                                            :start-offset 0
                                                                            :end (pos:create 0 1)
                                                                            :end-offset 1
                                                                            :text "`"))
                                                :kids (list (form:xyz-create :form-type types:*open-paren*
                                                                             :tokens (list (token:create :type-value types:*open-paren*
                                                                                                         :start (pos:create 0 1)
                                                                                                         :start-offset 1
                                                                                                         :end (pos:create 0 2)
                                                                                                         :end-offset 2
                                                                                                         :text "(")
                                                                                           (token:create :type-value types:*close-paren*
                                                                                                         :start (pos:create 0 4)
                                                                                                         :start-offset 4
                                                                                                         :end (pos:create 0 5)
                                                                                                         :end-offset 5
                                                                                                         :text ")"))
                                                                             :kids (list (form:xyz-create :form-type types:*comma*
                                                                                                          :tokens (list (token:create :type-value types:*comma*
                                                                                                                                      :start (pos:create 0 2)
                                                                                                                                      :start-offset 2
                                                                                                                                      :end (pos:create 0 3)
                                                                                                                                      :end-offset 3
                                                                                                                                      :text ","))
                                                                                                          :kids nil)))))))))


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
        (test-double-comma)
        (test-back-quote-with-comma)
        (test-double-quote-with-comma)
        (test-quoted-symbol)
        (test-back-quoted-symbol)
        (test-symbols-list)
        (test-in-package)
        (test-sym-with-pkg-nl)
        (test-close-parens)
        (test-getters)
        (test-commas)))
