(defpackage :alive/lsp/types/user-input
    (:use :cl)
    (:export :create-options
             :get-text
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/user-input)


(defclass user-input ()
        ((text :accessor text
               :initform nil
               :initarg :text)))


(defmethod print-object ((obj user-input) out)
    (format out "{text: \"~A\"}"
        (text obj)))


(defmethod types:deep-equal-p ((a user-input) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text a) (text b))))


(defun create-item (&key text)
    (make-instance 'user-input
        :text text))


(defun get-text (obj)
    (when obj
          (text obj)))


(defun from-wire (results)
    (labels ((add-param (params key value)
                        (cond ((eq key :text) (setf (text params) value)))))

        (loop :with item := (make-instance 'user-input)
              :for result :in results :do

                  (add-param item (car result) (cdr result))

              :finally (return item))))