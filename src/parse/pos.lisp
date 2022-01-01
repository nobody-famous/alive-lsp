(defpackage :alive/parse/pos
    (:use :cl)
    (:export :create
             :line
             :col))

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


(defun create (&key (line 0) (col 0))
    (make-instance 'pos
                   :line line
                   :col col))
