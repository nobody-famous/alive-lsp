(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create
             :get-end
             :get-end-offset
             :get-kids
             :get-start
             :get-start-offset
             :get-form-type
             :get-in-pkg
             :is-in-pkg
             :set-end
             :set-end-offset
             :set-is-in-pkg
             :set-token)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/parse/form)


(defclass form ()
        ((start :accessor start
                :initform nil
                :initarg :start)
         (start-offset :accessor start-offset
                       :initform nil
                       :initarg :start-offset)
         (end :accessor end
              :initform nil
              :initarg :end)
         (end-offset :accessor end-offset
                     :initform nil
                     :initarg :end-offset)
         (form-type :accessor form-type
                    :initform nil
                    :initarg :form-type)
         (in-pkg-p :accessor in-pkg-p
                   :initform nil
                   :initarg :in-pkg-p)
         (kids :accessor kids
               :initform nil
               :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A(~A:~A) type: ~A; in-pkg-p: ~A; kids:~{~A~}}"
        (start obj)
        (end obj)
        (start-offset obj)
        (end-offset obj)
        (form-type obj)
        (in-pkg-p obj)
        (kids obj)))


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (kids form))))
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun set-end (form pos)
    (setf (end form) pos))


(defun set-end-offset (form pos)
    (setf (end-offset form) pos))


(defun set-form-type (form value)
    (setf (form-type form) value))


(defun set-is-in-pkg (form value)
    (setf (in-pkg-p form) value))


(defun is-in-pkg (form)
    (when form
          (in-pkg-p form)))


(defun get-end (form)
    (when form
          (end form)))


(defun get-end-offset (form)
    (when form
          (end-offset form)))


(defun get-kids (form)
    (when form
          (kids form)))


(defun get-start (form)
    (when form
          (start form)))


(defun get-start-offset (form)
    (when form
          (start-offset form)))


(defun get-form-type (form)
    (when form
          (form-type form)))


(defun create (&key start start-offset end end-offset form-type in-pkg kids)
    (make-instance 'form
        :start start
        :start-offset start-offset
        :end end
        :end-offset end-offset
        :form-type form-type
        :in-pkg-p in-pkg
        :kids kids))
