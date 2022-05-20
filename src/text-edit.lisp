(defpackage :alive/text-edit
    (:use :cl)
    (:export :create
             :range
             :text)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/text-edit)


(defclass change ()
        ((range :accessor range
                :initform nil
                :initarg :range)
         (text :accessor text
               :initform nil
               :initarg :text)))


(defmethod print-object ((obj change) out)
    (format out "{range: ~A; text: \"~A\"}"
        (range obj)
        (text obj)))


(defmethod types:deep-equal-p ((a change) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (range a) (range b))
         (types:deep-equal-p (text a) (text b))))


(defun create (&key range text)
    (make-instance 'change
        :range range
        :text text))
