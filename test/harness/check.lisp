(defpackage :alive/test/harness/check
    (:use :cl)
    (:export :are-equal)
    (:local-nicknames (:err :alive/test/harness/errors)))

(in-package :alive/test/harness/check)


(defmethod deep-equal-p ((a alive/parse/pos::pos) (b alive/parse/pos::pos))
    (and (eq (alive/parse/pos:line a) (alive/parse/pos:line b))
         (eq (alive/parse/pos:col a) (alive/parse/pos:col b))))


(defmethod deep-equal-p ((a alive/parse/token::token) (b alive/parse/token::token))
    (and (deep-equal-p (alive/parse/token:start a) (alive/parse/token:start b))
         (deep-equal-p (alive/parse/token:end a) (alive/parse/token:end b))
         (string-equal (alive/parse/token:text a) (alive/parse/token:text b))
         (eq (alive/parse/token:type-value a) (alive/parse/token:type-value b))))


(defun has-deep-equal-p (obj1 obj2)
    (find-method #'deep-equal-p
                 '()
                 (mapcar #'find-class
                         (list (type-of obj1)
                               (type-of obj2)))
                 nil))


(defun compare (obj1 obj2)
    (labels ((compare-lists (obj1 obj2)
                  (cond ((not (typep obj2 'cons)) nil)
                        ((not (eq (length obj1) (length obj2))) nil)
                        (t (loop :with same := t

                                 :for item1 :in obj1
                                 :for item2 :in obj2 :do
                                     (setf same (and same (compare item1 item2)))

                                 :finally (return same))))))

        (cond ((has-deep-equal-p obj1 obj2) (deep-equal-p obj1 obj2))
              ((typep obj1 'cons) (compare-lists obj1 obj2)))))


(defun are-equal (expected actual)
    (unless (compare expected actual)
            (error 'err:test-failed
                   :expected expected
                   :actual actual)))
