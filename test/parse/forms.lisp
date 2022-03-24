(defpackage :alive/test/parse/forms
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)

                      (:pos :alive/position)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:token :alive/parse/token)
                      (:p :alive/parse/forms)))

(in-package :alive/test/parse/forms)


(defun check-forms (text expected)
    (let* ((input (make-string-input-stream text))
           (forms (p:from-stream input)))

        (check:are-equal expected forms)))


(defun validate-behavior (text)
    (let* ((input (make-string-input-stream text))
           (forms (alive/parse/stream:from input)))
        (loop :for form :in forms :do
                  (format T "~A~%" form))))


(defun basic ()
    (run:test "Basic Forms Test"
              (lambda ()
                  (check-forms (format nil "'foo:bar")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 8)
                                                  types:*quote*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 8)
                                                                     types:*symbol*
                                                                     (list))))))

                  (check-forms (format nil "foo:bar")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 7)
                                                  types:*symbol*
                                                  (list))))

                  (check-forms ""
                               (list))

                  (check-forms "foo"
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 3)
                                                  types:*symbol*)))

                  (check-forms "()"
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 2)
                                                  types:*open-paren*)))

                  (check-forms (format nil "(~%)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 1 1)
                                                  types:*open-paren*)))

                  (check-forms (format nil "'(foo)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 6)
                                                  types:*quote*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 6)
                                                                     types:*open-paren*
                                                                     (list (form:create (pos:create 0 2)
                                                                                        (pos:create 0 5)
                                                                                        types:*symbol*)))))))

                  (check-forms (format nil "`(foo ,bar)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 11)
                                                  types:*back-quote*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 11)
                                                                     types:*open-paren*
                                                                     (list (form:create (pos:create 0 2)
                                                                                        (pos:create 0 5)
                                                                                        types:*symbol*
                                                                                        (list))
                                                                           (form:create (pos:create 0 6)
                                                                                        (pos:create 0 10)
                                                                                        types:*symbol*
                                                                                        (list))))))))

                  (check-forms (format nil "''(foo ,bar)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 12)
                                                  types:*quote*
                                                  (list (form:create (pos:create 0 2)
                                                                     (pos:create 0 12)
                                                                     types:*open-paren*
                                                                     (list (form:create (pos:create 0 3)
                                                                                        (pos:create 0 6)
                                                                                        types:*symbol*
                                                                                        (list))
                                                                           (form:create (pos:create 0 7)
                                                                                        (pos:create 0 11)
                                                                                        types:*symbol*
                                                                                        (list))))))))

                  (check-forms (format nil "'foo")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 4)
                                                  types:*quote*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 4)
                                                                     types:*symbol*
                                                                     (list))))))

                  (check-forms (format nil "`foo")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 4)
                                                  types:*back-quote*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 4)
                                                                     types:*symbol*
                                                                     (list))))))

                  (check-forms (format nil "(a bb cccc)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 11)
                                                  types:*open-paren*
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 2)
                                                                     types:*symbol*
                                                                     (list))
                                                        (form:create (pos:create 0 3)
                                                                     (pos:create 0 5)
                                                                     types:*symbol*
                                                                     (list))
                                                        (form:create (pos:create 0 6)
                                                                     (pos:create 0 10)
                                                                     types:*symbol*
                                                                     (list)))))))))


(defun run-all ()
    (run:suite "Test parse forms"
               (Lambda ()
                   (basic))))
