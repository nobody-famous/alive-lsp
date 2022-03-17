(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create)
    (:local-nicknames (:types :alive/types)))

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


(defmethod types:deep-equal-p ((a form) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))
         (types:deep-equal-p (kids a) (kids b))))


(defun add-kid (kid form)
    (let* ((rev-kids (reverse (kids form))))
        (push kid rev-kids)
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun create (start end)
    (make-instance 'form
                   :start start
                   :end end))
