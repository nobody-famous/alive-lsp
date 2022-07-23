(defpackage :alive/range
    (:use :cl)
    (:export :create
             :start
             :end
             :from-wire)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/range)


#+n (defclass range ()
            ((start :accessor start
                    :initform 0
                    :initarg :start)
             (end :accessor end
                  :initform 0
                  :initarg :end)))


(defun start (range)
    (cdr (assoc :start range)))


(defun end (range)
    (cdr (assoc :end range)))


#+n (defmethod print-object ((obj range) out)
        (format out "[~A:~A]" (start obj) (end obj)))


(defun create (start end)
    (list (cons :start start)

          (cons :end end))
    #+n (make-instance 'range
            :start start
            :end end))


(defun from-wire (fields)
    fields
    #+n (labels ((add-field (obj key value)
                            (cond ((eq key :start) (setf (start obj) value))
                                  ((eq key :end) (setf (end obj) value)))))

            (loop :with obj := (make-instance 'range)

                  :for field :in fields :do
                      (add-field obj (car field) (cdr field))

                  :finally (return obj))))
