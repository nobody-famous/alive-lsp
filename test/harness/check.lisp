(defpackage :alive/test/harness/check
    (:use :cl)
    (:export :are-equal)
    (:local-nicknames (:err :alive/test/harness/errors)
                      (:types :alive/types)))

(in-package :alive/test/harness/check)


(defmethod types:deep-equal-p ((obj1 cons) obj2)
    (cond ((not (typep obj2 'cons)) nil)
          ((not (eq (length obj1) (length obj2))) nil)
          (t (loop :with same := t

                   :for item1 :in obj1
                   :for item2 :in obj2 :do
                       (setf same (and same (types:deep-equal-p item1 item2)))

                   :finally (return same)))))


(defmethod types:deep-equal-p ((a string) b)
    (and (equal (type-of a) (type-of b))
         (string-equal a b)))


(defmethod types:deep-equal-p ((a integer) b)
    (and (equal (type-of a) (type-of b))
         (eq a b)))


(defmethod types:deep-equal-p ((a T) b)
    (and (equal (type-of a) (type-of b))
         (eq a b)))


(defun are-equal (expected actual)
    (unless (types:deep-equal-p expected actual)
            (error 'err:test-failed
                   :expected expected
                   :actual actual)))
