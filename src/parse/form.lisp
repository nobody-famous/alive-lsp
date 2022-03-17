(defpackage :alive/parse/form
    (:use :cl)
    (:export :create))

(in-package :alive/parse/form)


(defclass form ()
    ((start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)
     (kids :accessor kids
           :initform nil
           :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A~{~A~}}"
            (start obj)
            (end obj)
            (kids obj)))


(defun create (&key start end)
    (make-instance 'form
                   :start start
                   :end end))
