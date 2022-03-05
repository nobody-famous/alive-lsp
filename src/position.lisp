(defpackage :alive/position
    (:use :cl)
    (:export :create
             :line
             :col
             :less-than
             :less-or-equal
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/position)


(defclass pos ()
    ((line :accessor line
           :initform 0
           :initarg :line)
     (col :accessor col
          :initform 0
          :initarg :col)))


(defmethod print-object ((obj pos) out)
    (format out "[~A:~A]" (line obj) (col obj)))


(defmethod types:deep-equal-p ((a pos) b)
    (and (equal (type-of a) (type-of b))
         (eq (line a) (line b))
         (eq (col a) (col b))))


(defun less-than (pos1 pos2)
    (cond ((< (line pos1) (line pos2)) T)
          ((< (line pos2) (line pos1)) NIL)
          (T (< (col pos1) (col pos2)))))


(defun less-or-equal (pos1 pos2)
    (or (less-than pos1 pos2)
        (and (= (line pos1) (line pos2))
             (= (col pos1) (col pos2)))))


(defun create (&key line col)
    (make-instance 'pos
                   :line line
                   :col col))


(defun from-wire (fields)
    (labels ((add-field (id key value)
                  (cond ((eq key :line) (setf (line id) value))
                        ((eq key :character) (setf (col id) value)))))

        (loop :with id := (make-instance 'pos)

              :for field :in fields :do
                  (add-field id (car field) (cdr field))

              :finally (return id))))
