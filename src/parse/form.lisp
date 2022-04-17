(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create
             :get-end
             :get-kids
             :get-start
             :get-form-type
             :get-in-pkg
             :is-in-pkg
             :set-end
             :set-is-in-pkg
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
     (in-pkg-p :accessor in-pkg-p
               :initform nil
               :initarg :in-pkg-p)
     (in-pkg-name :accessor in-pkg-name
                  :initform nil
                  :initarg :in-pkg-name)
     (kids :accessor kids
           :initform nil
           :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A type: ~A; in-pkg-p: ~A; in-pkg: ~A; kids:~{~A~}}"
            (start obj)
            (end obj)
            (form-type obj)
            (in-pkg-p obj)
            (in-pkg-name obj)
            (kids obj)))


(defmethod types:deep-equal-p ((a form) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (form-type a) (form-type b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))
         (types:deep-equal-p (in-pkg-p a) (in-pkg-p b))
         (types:deep-equal-p (in-pkg-name a) (in-pkg-name b))
         (types:deep-equal-p (kids a) (kids b))))


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (kids form))))
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun set-end (form pos)
    (setf (end form) pos))


(defun set-form-type (form value)
    (setf (form-type form) value))


(defun set-is-in-pkg (form value)
    (setf (in-pkg-p form) value))


(defun is-in-pkg (form)
    (when form
          (in-pkg-p form)))


(defun get-in-pkg (form)
    (when form
          (in-pkg-name form)))


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


(defun create (&key start end form-type in-pkg kids)
    (make-instance 'form
                   :start start
                   :end end
                   :form-type form-type
                   :in-pkg-p in-pkg
                   :kids kids))
