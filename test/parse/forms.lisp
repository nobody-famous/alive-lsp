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
                  ;   (check-forms ""
                  ;                (list))

                  ;   (check-forms "foo"
                  ;                (list (form:create (pos:create 0 0)
                  ;                                   (pos:create 0 3))))

                  ;   (check-forms "()"
                  ;                (list (form:create (pos:create 0 0)
                  ;                                   (pos:create 0 2))))

                  ;   (check-forms (format nil "(~%)")
                  ;                (list (form:create (pos:create 0 0)
                  ;                                   (pos:create 1 1))))

                  (check-forms (format nil "'(foo)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 6)
                                                  (token:create :type-value types:*quote*
                                                                :start (pos:create 0 0)
                                                                :end (pos:create 0 1)
                                                                :text "'")
                                                  (list (form:create (pos:create 0 1)
                                                                     (pos:create 0 6)
                                                                     (token:create :type-value types:*quote*
                                                                                   :start (pos:create 0 1)
                                                                                   :end (pos:create 0 2)
                                                                                   :text "(")
                                                                     (list (form:create (pos:create 0 2)
                                                                                        (pos:create 0 5)
                                                                                        (token:create :type-value types:*symbol*
                                                                                                      :start (pos:create 0 2)
                                                                                                      :end (pos:create 0 5)
                                                                                                      :text "foo"))))))))

                  (check-forms (format nil "`(foo ,bar)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 11))))

                  (check-forms (format nil "''(foo ,bar)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 12))))

                  (check-forms (format nil "'foo")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 4))))

                  (check-forms (format nil "(a bb cccc)")
                               (list (form:create (pos:create 0 0)
                                                  (pos:create 0 11)
                                                  (list (form:create (pos:create 0 1) (pos:create 0 2))
                                                        (form:create (pos:create 0 3) (pos:create 0 5))
                                                        (form:create (pos:create 0 6) (pos:create 0 10)))))))))


(defun run-all ()
    (run:suite "Test parse forms"
               (Lambda ()
                   (basic))))
