(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create
             :get-end
             :get-kids
             :get-start
             :get-token
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
     (token :accessor token
            :initform nil
            :initarg :token)
     (kids :accessor kids
           :initform nil
           :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A token: ~A kids:~{~A~}}"
            (start obj)
            (end obj)
            (token obj)
            (kids obj)))


(defmethod types:deep-equal-p ((a form) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))
         (types:deep-equal-p (kids a) (kids b))))


(defun add-kid (kid form)
    (let* ((rev-kids (reverse (kids form))))
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun set-end (form pos)
    (setf (end form) pos))


(defun set-token (form item)
    (setf (token form) item))


(defun get-end (form)
    (when form
          (end form)))


(defun get-kids (form)
    (when form
          (kids form)))


(defun get-start (form)
    (when form
          (start form)))


(defun get-token (form)
    (when form
          (token form)))


(defun create (start &optional end kids)
    (make-instance 'form
                   :start start
                   :end end
                   :kids kids))
