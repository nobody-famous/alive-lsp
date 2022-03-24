(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create
             :get-end
             :get-kids
             :get-start
             :get-form-type
             :set-end
             :set-token)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/parse/form)


(defclass form ()
    ((start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)
     (form-type :accessor form-type
                :initform nil
                :initarg :form-type)
     (kids :accessor kids
           :initform nil
           :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A type: ~A kids:~{~A~}}"
            (start obj)
            (end obj)
            (form-type obj)
            (kids obj)))


(defmethod types:deep-equal-p ((a form) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (form-type a) (form-type b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))
         (types:deep-equal-p (kids a) (kids b))))


(defun add-kid (kid form)
    (let* ((rev-kids (reverse (kids form))))
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun set-end (form pos)
    (setf (end form) pos))


(defun set-form-type (form value)
    (setf (form-type form) value))


(defun get-end (form)
    (when form
          (end form)))


(defun get-kids (form)
    (when form
          (kids form)))


(defun get-start (form)
    (when form
          (start form)))


(defun get-form-type (form)
    (when form
          (form-type form)))


(defun create (start &optional end form-type kids)
    (make-instance 'form
                   :start start
                   :end end
                   :form-type form-type
                   :kids kids))
