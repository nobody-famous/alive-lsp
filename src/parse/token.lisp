(defpackage :alive/parse/token
    (:use :cl)
    (:export :create
             :end
             :get-type-value
             :get-text
             :start)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/parse/token)


(defclass token ()
    ((start :accessor start
            :initform (pos:create)
            :initarg :start)
     (end :accessor end
          :initform (pos:create)
          :initarg :end)
     (text :accessor text
           :initform nil
           :initarg :text)
     (type-value :accessor type-value
                 :initform nil
                 :initarg :type-value)))


(defmethod print-object ((obj token) out)
    (format out "{~A,~A ~A ~A}"
            (start obj)
            (end obj)
            (type-value obj)
            (if (eq types:*ws* (type-value obj))
                ""
                (text obj))))


(defmethod types:deep-equal-p ((a token) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))
         (string-equal (text a) (text b))
         (eq (type-value a) (type-value b))))


(defun get-type-value (obj)
    (when obj (type-value obj)))


(defun get-text (obj)
    (when obj (text obj)))


(defun create (&key type-value start end text)
    (make-instance 'token
                   :start start
                   :end end
                   :text text
                   :type-value type-value))
