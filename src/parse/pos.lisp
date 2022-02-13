(defpackage :alive/parse/pos
    (:use :cl)
    (:export :create
             :line
             :col)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/parse/pos)


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


(defun create (&key (line 0) (col 0))
    (make-instance 'pos
                   :line line
                   :col col))
