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
     (type-name :accessor type-name
                :initform nil
                :initarg :type-name)))


(defun create (&key type-name start end)
    (make-instance 'token
                   :start start
                   :end end
                   :type-name type-name))
