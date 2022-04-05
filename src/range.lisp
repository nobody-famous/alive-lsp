(defpackage :alive/range
    (:use :cl)
    (:export :create
             :start
             :end
             :from-wire)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/range)


(defclass range ()
    ((start :accessor start
            :initform 0
            :initarg :start)
     (end :accessor end
          :initform 0
          :initarg :end)))


(defmethod print-object ((obj range) out)
    (format out "[~A:~A]" (start obj) (end obj)))


(defmethod types:deep-equal-p ((a range) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))))


(defun create (start end)
    (make-instance 'range
                   :start start
                   :end end))


(defun from-wire (fields)
    (labels ((add-field (obj key value)
                  (cond ((eq key :start) (setf (start obj) (pos:from-wire value)))
                        ((eq key :end) (setf (end obj) (pos:from-wire value))))))

        (loop :with obj := (make-instance 'range)

              :for field :in fields :do
                  (add-field obj (car field) (cdr field))

              :finally (return obj))))
