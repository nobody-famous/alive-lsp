(defpackage :alive/parse/token
    (:use :cl)
    (:export :create))

(in-package :alive/parse/token)


(defclass token ()
    ((start :accessor start
            :initform (make-instance 'pos)
            :initarg :start)
     (end :accessor end
          :initform (make-instance 'pos)
          :initarg :end)
     (text :accessor text
           :initform nil
           :initarg :text)
     (type-value :accessor type-value
                 :initform nil
                 :initarg :type-value)))


(defun create (&key type-value start end text)
    (make-instance 'token
                   :start start
                   :end end
                   :text text
                   :type-value type-value))
