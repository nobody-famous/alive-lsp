(defpackage :alive/parse/token
    (:use :cl)
    (:export :create
             :end
             :start
             :text
             :type-value)
    (:local-nicknames (:pos :alive/parse/pos)
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


(defmethod types:deep-equal-p ((a alive/parse/token::token) (b alive/parse/token::token))
    (and (types:deep-equal-p (alive/parse/token:start a) (alive/parse/token:start b))
         (types:deep-equal-p (alive/parse/token:end a) (alive/parse/token:end b))
         (string-equal (alive/parse/token:text a) (alive/parse/token:text b))
         (eq (alive/parse/token:type-value a) (alive/parse/token:type-value b))))


(defun create (&key type-value start end text)
    (make-instance 'token
                   :start start
                   :end end
                   :text text
                   :type-value type-value))
