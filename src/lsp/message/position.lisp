(defpackage :alive/lsp/message/position
    (:use :cl)
    (:export :create
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/message/position)


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
