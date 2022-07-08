(in-package :clue)


(defgeneric are-equal (obj1 obj2))


(defmethod are-equal ((obj1 cons) obj2)
    (cond ((not (typep obj2 'cons)) nil)
          ((not (eq (length obj1) (length obj2))) nil)
          (t (loop :with same := t

                   :for item1 :in obj1
                   :for item2 :in obj2 :do
                       (setf same (and same (are-equal item1 item2)))

                   :finally (return same)))))


(defmethod are-equal ((a string) b)
    (and (string-equal a b)))


(defmethod are-equal ((a integer) b)
    (and (equal (type-of a) (type-of b))
         (eq a b)))


(defmethod are-equal ((a T) b)
    (and (equal (type-of a) (type-of b))
         (eq a b)))


(defun check-equal (&key expected actual)
    (unless (are-equal expected actual)
        (error 'test-failed
            :expected expected
            :actual actual)))


(defun check-exists (obj)
    (unless obj
        (error 'test-failed
            :expected obj
            :actual NIL)))
